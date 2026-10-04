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
  , markBaselined
  , suggestChoice
  , FixDef (..)
  , FixOffer (..)
  , Replacement (..)
  ) where

import Control.Monad ((>=>))
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.Aeson qualified as J
import Data.Char (isAlphaNum, isSpace)
import Data.Word (Word64)
import Numeric (showHex)
import Data.List (nub, nubBy, sortOn, (\\))
import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Olifant.Actions (matchPathSlots)
import Olifant.Glob (matchesGlob)
import Olifant.Language (Language (..), Match (..), Rule, Syntax (..), describeSyntaxFailure, languageFor, renderRule, ruleFromTree)
import Olifant.Template (Template, Value (..), describeTemplateError, freeVariables, parseTemplate, render)
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
  , ruleFixes :: [FixDef]
  -- ^ ranked: the first is offered first
  }
  deriving stock (Show)

-- | A fix a rule offers: a generator, a label, and its inputs from the
-- finding (`with`: templates over the path's slots and dir/stem/file;
-- without it, the path's slots as they are).
data FixDef = FixDef {fdGenerator :: Text, fdLabel :: Maybe Text, fdWith :: Maybe [(Text, Template)], fdReplace :: [([Text], Template)], fdContext :: [Template]}
  deriving stock (Show)

-- | A fix offered for one finding: the generator, its label, its inputs,
-- and the code it replaces, if its rule says so.
data FixOffer = FixOffer {foGenerator :: Text, foLabel :: Maybe Text, foSets :: [(Text, Text)], foReplace :: Maybe Replacement}
  deriving stock (Eq, Show)

-- | The code a fix replaces: whole lines (1-based, inclusive) of the node
-- enclosing the finding, their text, the first line's indentation, the
-- replacement (a template over the fix's inputs and `finding`, the node's
-- code; empty deletes the lines), and the node's code.
data Replacement = Replacement {rpLines :: (Int, Int), rpOld :: Text, rpIndent :: Text, rpText :: Template, rpFinding :: Text}
  deriving stock (Eq, Show)

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
  , fFixes :: [FixOffer]
  , fText :: Text
  -- ^ the code the finding is about (for choosing a fix's inputs)
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
      onlyKeys ["files", "exclude", "match", "missing", "message", "severity", "fix", "fixes", "since", "why"] t
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
      one <- maybe [] (\g -> [FixDef g Nothing Nothing [] []]) <$> optional "fix" text t
      many <- maybe (Right []) (list (table >=> fixDef)) (M.lookup "fixes" t)
      check <- case (M.lookup "match" t, M.lookup "missing" t) of
        (Just m, Nothing) -> RuleMatch <$> inKey "match" (ruleFromTree m)
        (Nothing, Just m) -> RuleMissing <$> inKey "missing" (text m >>= tmpl >>= onlyFileVars)
        _ -> Left "a rule has exactly one of `match` (an ast-grep rule) or `missing` (a path that must exist)"
      pure (RuleDef k files exclude check message severity (one ++ many))
    ruleSev s =
      parseSeverity s >>= \case
        Off -> Left "a rule's severity is hint, warn, error (turn it off in the project's [rules])"
        x -> Right x
    tmpl t = either (Left . describeTemplateError) Right (parseTemplate t)
    fixDef f = inKey "fixes" $ do
      onlyKeys ["generator", "label", "with", "replace", "context"] f
      g <- required "generator" text f
      label <- optional "label" text f
      with <- traverse (\w -> traverse (\(k, v) -> inKey ("with." <> k) ((k,) <$> (text v >>= tmpl))) (M.toList w)) =<< optional "with" table f
      -- `replace`: one case or a list; the first whose node encloses the finding wins
      cases <- case M.lookup "replace" f of
        Nothing -> Right []
        Just v -> inKey "replace" (either (const (pure <$> (table v >>= replaceCase))) Right (list (table >=> replaceCase) v))
      -- files the model should see when it fills the replacement's holes
      context <- maybe (Right []) (list (text >=> tmpl)) (M.lookup "context" f)
      pure (FixDef g label with cases context)
    replaceCase c = do
      onlyKeys ["enclosing", "text"] c
      (,) <$> required "enclosing" (list text) c <*> required "text" (text >=> tmpl) c
    -- A `missing` path knows only the file it is about (review C2).
    onlyFileVars t = case S.toList (freeVariables t) \\ ["dir", "stem", "file"] of
      [] -> Right t
      vs -> Left ("`missing` can use {= dir =}, {= stem =} and {= file =}; not " <> T.intercalate ", " ["`" <> v <> "`" | v <- vs])

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
          unknown = [Finding "unknown-rule" f (lineRange a) Warn ("olifant:allow names no rule `" <> alRule a <> "`") [] "" (fingerprint ["unknown-rule", alRule a]) | a <- allows, alRule a `notElem` known]
          noReason = [Finding "allow-without-reason" f (lineRange a) Warn ("olifant:allow " <> alRule a <> " needs a reason: `— why`") [] "" (fingerprint ["allow-without-reason", alRule a]) | a <- allows, alRule a `elem` known, isNothing (alReason a)]
          commentExceptions = [Exception (alRule a) (T.pack f <> ":" <> T.pack (show (alLine a))) (alReason a) (alLine a `notElem` usedAllows) | a <- allows, alRule a `elem` known]
          configExceptions = nub [Exception r (where' globs) (Just reason) False | Right (r, globs, reason) <- used]
          sorted = sortOn (\fd -> (fmap fst (fRange fd), fRule fd)) (kept ++ unknown ++ noReason)
      pure (sorted, commentExceptions ++ configExceptions)
    found f content lang r slots = case ruleCheck r of
      RuleMatch rule -> case (lang, renderRule (M.fromList [(k, VText v) | (k, v) <- slots]) rule) of
        (Just l, Right q) ->
          querySyntax syntax l q content >>= \case
            -- One finding where matches nest: the outermost (an import and
            -- the path inside it are one finding).
            Right ms -> traverse (\m -> withReplacements content l m (pairs f slots r) (finding f r slots (Just (range content m)) (matchText m))) (outermost ms)
            Left failure -> pure [cannotRun f r (describeSyntaxFailure failure)]
        (Nothing, _) -> pure [cannotRun f r "no language adapter for this file"]
        (_, Left err) -> pure [cannotRun f r (describeTemplateError err)]
      RuleMissing tmpl ->
        let env = M.fromList [("dir", VText (T.pack (takeDirectory f))), ("stem", VText (T.pack (takeBaseName f))), ("file", VText (T.pack f))]
         in pure $ case render env tmpl of
              Right p | collapse (T.unpack p) `notElem` paths -> [finding f r slots Nothing p]
              Right _ -> []
              Left err -> [cannotRun f r (describeTemplateError err)]
    -- A rule that cannot run is an error: a gate must never pass silently
    -- because it could not look (review C2).
    cannotRun f r why = Finding "rule-failed" f Nothing Error ("rule `" <> ruleId r <> "` could not run on " <> T.pack f <> ": " <> why) [] "" (fingerprint ["rule-failed", ruleId r])
    finding f r slots rng what = Finding (ruleId r) f rng (severityOf r) (ruleMessage r) (map snd (pairs f slots r)) what (fingerprint [ruleId r, what])
    -- each fix definition with its offer (a fix whose inputs don't render is not offered)
    pairs f slots r =
      let env = M.fromList ([(k, VText v) | (k, v) <- slots] ++ [("dir", VText (T.pack (takeDirectory f))), ("stem", VText (T.pack (takeBaseName f))), ("file", VText (T.pack f))])
       in [ (d, FixOffer (fdGenerator d) (fdLabel d) (sortOn fst sets) Nothing)
          | d <- ruleFixes r
          , Right sets <- [maybe (Right slots) (traverse (\(k, t) -> (k,) <$> either (Left . describeTemplateError) Right (render env t))) (fdWith d)]
          ]
    -- Each fix that replaces code: the nearest enclosing node of the first
    -- case that has one (an import is deleted, a statement rewritten).
    withReplacements content lang m ps fd = do
      offers' <- traverse (\(d, o) -> replaceFor content lang m d o) ps
      pure fd {fFixes = offers'}
    replaceFor content lang m d o = go (fdReplace d)
      where
        go [] = pure o
        go ((kinds, t) : rest) =
          querySyntax syntax lang (J.object ["any" J..= [J.object ["kind" J..= k] | k <- kinds]]) content >>= \case
            Right ns
              | n : _ <- sortOn size [x | x <- ns, matchStart x <= matchStart m, matchEnd m <= matchEnd x] ->
                  let ls = T.lines content
                      (l1, l2) = (fst (matchStart n) + 1, fst (matchEnd n) + 1)
                      old = T.intercalate "\n" (take (l2 - l1 + 1) (drop (l1 - 1) ls))
                      indent = T.takeWhile isSpace (fromMaybe "" (listToMaybe (drop (l1 - 1) ls)))
                   in pure o {foReplace = Just (Replacement (l1, l2) old indent t (matchText n))}
            _ -> go rest
        size x = (fst (matchEnd x) - fst (matchStart x), snd (matchEnd x) - snd (matchStart x))
    -- the outermost of nested matches, then the first on each line
    outermost ms =
      nubBy (\a b -> fst (matchStart a) == fst (matchStart b)) $
        sortOn matchStart [m | m <- ms, not (any (\o -> o /= m && matchStart o <= matchStart m && matchEnd m <= matchEnd o) ms)]
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

-- | The one choice whose words all appear in the code (`invoice_repository`
-- for `invoices::table`: "invoice" is there; "repository" is the port's
-- kind, so suffixes like that are ignored), or none when it is ambiguous.
suggestChoice :: Text -> [Text] -> Maybe Text
suggestChoice code choices = case filter fits choices of
  [c] -> Just c
  _ -> Nothing
  where
    tokens = S.fromList (T.split (not . isAlphaNum) (T.toLower code))
    stems = S.map (\w -> fromMaybe w (T.stripSuffix "s" w)) tokens
    kinds = ["repository", "sender", "client", "port", "service", "gateway", "store"]
    fits c =
      let ws = [w | w <- T.splitOn "_" (T.toLower c), w `notElem` kinds, not (T.null w)]
       in not (null ws) && all (\w -> w `S.member` tokens || w `S.member` stems) ws

-- | Which findings the baseline covers: each (rule, file, fingerprint) key
-- covers as many findings as its count, in order — a second identical
-- finding is new (review I9).
markBaselined :: [((Text, Text, Text), Int)] -> [Finding] -> [(Finding, Bool)]
markBaselined counts = go (M.fromListWith (+) counts)
  where
    go _ [] = []
    go left (f : fs) =
      let k = (fRule f, T.pack (fFile f), fFingerprint f)
       in case M.lookup k left of
            Just n | n > 0 -> (f, True) : go (M.insert k (n - 1) left) fs
            _ -> (f, False) : go left fs

-- | Quotes that open or close a string: `\"` inside one does not count (review I11).
unescapedQuotes :: Text -> Int
unescapedQuotes = go 0 . T.unpack
  where
    go n ('\\' : _ : rest) = go n rest
    go n ('"' : rest) = go (n + 1) rest
    go n (_ : rest) = go n rest
    go n [] = n

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
      , even (unescapedQuotes before)
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
