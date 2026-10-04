-- | Rules (spec §5.9): conventions as checks — a query over the code, the
-- reason it matters, a severity, and an optional fix (a generator). Rules
-- warn and offer; they never edit code. Escape hatches are local and
-- reported: `olifant:allow RULE — reason` comments (the line or the line
-- above), `olifant:allow-file RULE — reason` (first lines), and the
-- project's `[rules]` (severities, allowed paths with a reason).
module Olifant.Rules
  ( Severity (..)
  , RuleCheck (..)
  , RuleDef (..)
  , RulesConfig (..)
  , Finding (..)
  , Exception (..)
  , parseRules
  , parseRulesConfig
  , parseSeverity
  , checkFiles
  , fingerprint
  ) where

import Control.Monad ((>=>))
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.Word (Word64)
import Numeric (showHex)
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Olifant.Actions (matchPathSlots)
import Olifant.Glob (matchesGlob)
import Olifant.Language (Language (..), Match (..), Rule, Syntax (..), languageFor, renderRule, ruleFromTree)
import Olifant.Template (Template, Value (..), parseTemplate, render)
import Olifant.Toml (Table, inKey, list, onlyKeys, optional, parseToml, required, table, text)
import System.FilePath (joinPath, splitDirectories, takeBaseName, takeDirectory)

data Severity = Hint | Warn | Error | Off
  deriving stock (Eq, Ord, Show)

-- | What a rule looks for: an ast-grep rule over the file, or a path that
-- must exist (a template over the file: `{= dir =}`, `{= stem =}`, `{= file =}`).
data RuleCheck = RuleMatch Rule | RuleMissing Template
  deriving stock (Show)

data RuleDef = RuleDef
  { ruleId :: Text
  , ruleFiles :: Text
  -- ^ a path pattern; `{slot}`s become the fix's inputs
  , ruleExclude :: [Text]
  , ruleCheck :: RuleCheck
  , ruleMessage :: Text
  , ruleSeverity :: Severity
  , ruleFix :: Maybe Text
  }
  deriving stock (Show)

-- | The project's `[rules]`: severities (incl. `off`) and allowed paths
-- (rule, globs, reason).
data RulesConfig = RulesConfig {rcSeverity :: [(Text, Severity)], rcAllow :: [(Text, [Text], Text)]}
  deriving stock (Eq, Show)

-- | A rule's finding. Ranges: 1-based lines, UTF-8 byte columns, end just past.
data Finding = Finding
  { fRule :: Text
  , fFile :: FilePath
  , fRange :: Maybe ((Int, Int), (Int, Int))
  , fSeverity :: Severity
  , fMessage :: Text
  , fFix :: Maybe Text
  , fSets :: [(Text, Text)]
  , fFingerprint :: Text
  -- ^ what the finding is about, stable when lines move (for the baseline, spec §5.10)
  }
  deriving stock (Eq, Show)

-- | An exception the code or the project makes: where, why, and whether it
-- still suppresses anything.
data Exception = Exception {exRule :: Text, exWhere :: Text, exReason :: Maybe Text, exStale :: Bool}
  deriving stock (Eq, Show)

-- | `rules.toml`: `[rules.ID]` tables.
parseRules :: Text -> Either Text [RuleDef]
parseRules src = do
  top <- parseToml src
  onlyKeys ["rules"] top
  rs <- fromMaybe M.empty <$> optional "rules" table top
  traverse (\(k, v) -> inKey ("rules." <> k) (table v >>= rule k)) (M.toList rs)
  where
    rule k t = do
      onlyKeys ["files", "exclude", "match", "missing", "message", "severity", "fix", "since", "why"] t
      files <- required "files" text t
      exclude <- fromMaybe [] <$> optional "exclude" (list text) t
      message0 <- required "message" text t
      since <- optional "since" text t
      why <- optional "why" text t
      let story = case (since, why) of
            (Just s, Just w) -> " (since " <> s <> ": " <> w <> ")"
            (Just s, Nothing) -> " (since " <> s <> ")"
            (Nothing, Just w) -> " (" <> w <> ")"
            _ -> ""
          message = message0 <> story
      severity <- maybe (Right Warn) ruleSev =<< optional "severity" text t
      fix <- optional "fix" text t
      check <- case (M.lookup "match" t, M.lookup "missing" t) of
        (Just m, Nothing) -> RuleMatch <$> inKey "match" (ruleFromTree m)
        (Nothing, Just m) -> RuleMissing <$> inKey "missing" (text m >>= tmpl)
        _ -> Left "a rule has exactly one of `match` (an ast-grep rule) or `missing` (a path that must exist)"
      pure (RuleDef k files exclude check message severity fix)
    ruleSev s =
      parseSeverity s >>= \case
        Off -> Left "a rule's severity is hint, warn, error (turn it off in the project's [rules])"
        x -> Right x
    tmpl t = either (Left . T.pack . show) Right (parseTemplate t)

parseSeverity :: Text -> Either Text Severity
parseSeverity = \case
  "hint" -> Right Hint
  "warn" -> Right Warn
  "error" -> Right Error
  "off" -> Right Off
  other -> Left ("unknown severity `" <> other <> "` (hint, warn, error, off)")

-- | The `[rules]` table of a project's config: `ID = "severity"`, and
-- `[[rules.allow]]` tables (`rule`, `paths`, `reason`).
parseRulesConfig :: Table -> Either Text RulesConfig
parseRulesConfig top =
  optional "rules" table top >>= \case
    Nothing -> Right (RulesConfig [] [])
    Just t -> inKey "rules" $ do
      severities <- traverse (\(k, v) -> inKey k ((k,) <$> (text >=> parseSeverity) v)) [(k, v) | (k, v) <- M.toList t, k /= "allow"]
      allows <- maybe (Right []) (list (table >=> allow)) (M.lookup "allow" t)
      pure (RulesConfig severities allows)
  where
    allow a = inKey "allow" $ do
      onlyKeys ["rule", "paths", "reason"] a
      (,,) <$> required "rule" text a <*> required "paths" (list text) a <*> required "reason" text a

-- | An `olifant:allow` comment: its line, whether it covers the file, the
-- rule, and the reason.
data Allow = Allow {alLine :: Int, alFile :: Bool, alRule :: Text, alReason :: Maybe Text}

-- | Check files against the rules: findings (by line) and the exceptions
-- that applied, or are stale.
checkFiles :: Monad m => Syntax m -> [Language] -> RulesConfig -> [RuleDef] -> [FilePath] -> (FilePath -> m (Maybe Text)) -> [FilePath] -> m ([Finding], [Exception])
checkFiles syntax langs config rules paths readFile' files = do
  perFile <- traverse one files
  let fs = concatMap fst perFile
      exs = concatMap snd perFile
      usedConfig = nub [w | Exception _ w _ False <- exs]
      staleConfig =
        [ Exception r (where' globs) (Just reason) True
        | not (null files)
        , (r, globs, reason) <- rcAllow config
        , where' globs `notElem` usedConfig
        ]
  pure (fs, exs ++ staleConfig)
  where
    known = map ruleId rules
    severityOf r = fromMaybe (ruleSeverity r) (lookup (ruleId r) (rcSeverity config))
    where' globs = "paths " <> T.intercalate ", " globs
    one f = do
      content <- fromMaybe "" <$> readFile' f
      let lang = languageFor langs f
          allows = maybe [] (\l -> parseAllows (langComment l) content) lang
          applicable = [(r, slots) | r <- rules, severityOf r /= Off, Just slots <- [matchPathSlots (ruleFiles r) f], not (any (`matchesGlob` f) (ruleExclude r))]
      raw <- concat <$> traverse (\(r, slots) -> found f content lang r slots) applicable
      let suppressedBy fd =
            listToMaybe $
              [Left a | a <- allows, alRule a == fRule fd, alFile a]
                ++ [Left a | a <- allows, alRule a == fRule fd, not (alFile a), Just ((l, _), _) <- [fRange fd], alLine a == l || alLine a == l - 1]
                ++ [Right (r, globs, reason) | (r, globs, reason) <- rcAllow config, r == fRule fd, any (`matchesGlob` f) globs]
          kept = [fd | fd <- raw, isNothing (suppressedBy fd)]
          used = [s | fd <- raw, Just s <- [suppressedBy fd]]
          usedAllows = [alLine a | Left a <- used]
          lineRange a = Just ((alLine a, 1), (alLine a, 1))
          unknown = [Finding "unknown-rule" f (lineRange a) Warn ("olifant:allow names no rule `" <> alRule a <> "`") Nothing [] (fingerprint ["unknown-rule", alRule a]) | a <- allows, alRule a `notElem` known]
          noReason = [Finding "allow-without-reason" f (lineRange a) Warn ("olifant:allow " <> alRule a <> " needs a reason: `— why`") Nothing [] (fingerprint ["allow-without-reason", alRule a]) | a <- allows, alRule a `elem` known, isNothing (alReason a)]
          commentExceptions = [Exception (alRule a) (T.pack f <> ":" <> T.pack (show (alLine a))) (alReason a) (alLine a `notElem` usedAllows) | a <- allows, alRule a `elem` known]
          configExceptions = nub [Exception r (where' globs) (Just reason) False | Right (r, globs, reason) <- used]
          sorted = sortOn (\fd -> (fmap fst (fRange fd), fRule fd)) (kept ++ unknown ++ noReason)
      pure (sorted, commentExceptions ++ configExceptions)
    found f content lang r slots = case ruleCheck r of
      RuleMatch rule -> case (lang, renderRule (M.fromList [(k, VText v) | (k, v) <- slots]) rule) of
        (Just l, Right q) ->
          querySyntax syntax l q content >>= \case
            Right ms -> pure [finding f r slots (Just (range content m)) (matchText m) | m <- ms]
            Left _ -> pure []
        _ -> pure []
      RuleMissing tmpl ->
        let env = M.fromList [("dir", VText (T.pack (takeDirectory f))), ("stem", VText (T.pack (takeBaseName f))), ("file", VText (T.pack f))]
         in pure $ case render env tmpl of
              Right p | collapse (T.unpack p) `notElem` paths -> [finding f r slots Nothing p]
              _ -> []
    finding f r slots rng what = Finding (ruleId r) f rng (severityOf r) (ruleMessage r) (ruleFix r) (sortOn fst slots) (fingerprint [ruleId r, what])
    range content m =
      let ls = T.lines content
          pos (l, c) = (l + 1, 1 + BS.length (TE.encodeUtf8 (T.take c (fromMaybe "" (listToMaybe (drop l ls))))))
       in (pos (matchStart m), pos (matchEnd m))

-- | A stable fingerprint of what a finding is about: FNV-1a (64-bit) of its
-- parts with whitespace collapsed, so it survives reformatting and moves.
fingerprint :: [Text] -> Text
fingerprint parts = T.pack (showHex64 (BS.foldl' step 14695981039346656037 bytes))
  where
    bytes = TE.encodeUtf8 (T.intercalate "\0" (map (T.unwords . T.words) parts))
    step h b = (h `xor` fromIntegral b) * 1099511628211 :: Word64
    showHex64 w = let s = showHex w "" in replicate (16 - length s) '0' ++ s

-- | `a/b/../c` → `a/c`.
collapse :: FilePath -> FilePath
collapse = joinPath . reverse . foldl step [] . splitDirectories
  where
    step (_ : acc) ".." = acc
    step acc "." = acc
    step acc d = d : acc

-- | `olifant:allow` comments: a comment (the language's prefix, not inside
-- a string) whose text starts with `olifant:allow RULE` or
-- `olifant:allow-file RULE` (first 5 lines), then an optional reason after
-- `—`, `-` or `:`.
parseAllows :: Text -> Text -> [Allow]
parseAllows prefix content = catMaybes [allowAt i l | (i, l) <- zip [1 ..] (T.lines content)]
  where
    allowAt i l = listToMaybe (mapMaybe (comment i) (commentBodies l))
    commentBodies l =
      [ T.drop (T.length prefix) rest
      | (before, rest) <- T.breakOnAll prefix l
      , even (T.count "\"" before)
      ]
    comment i body = case T.words body of
      ("olifant:allow-file" : r : _) | i <= 5 -> Just (Allow i True r (reason r body))
      ("olifant:allow" : r : _) -> Just (Allow i False r (reason r body))
      _ -> Nothing
    reason r body =
      let after = T.strip (snd (T.breakOn r body))
          rest = T.strip (T.drop (T.length r) after)
          stripped = T.strip (T.dropWhile (`elem` ("—-:" :: String)) rest)
       in if T.null stripped then Nothing else Just stripped
