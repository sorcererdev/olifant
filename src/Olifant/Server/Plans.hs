-- | Internal to Olifant.Server: a plan's anchors (`@path:Unit`) as the
-- server sees them: each resolved to its unit (over the editor's text),
-- stale and missing ones as diagnostics, completed after `@`, and the
-- plan's anchored lines as a list of files.
module Olifant.Server.Plans
  ( isPlanPath
  , Resolved (..)
  , resolveAnchor
  , planDiagnostics
  , anchorItems
  , FileItem (..)
  , planFiles
  , ErrandAnswer (..)
  , runErrand
  , currentBranch
  , gitOut
  , confined
  , safely
  ) where

import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Functor ((<&>))
import Data.List (find, isPrefixOf, nub)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Text.Encoding qualified as TE
import Olifant.Language.Adapter (Language (..), TestDef (..))
import Olifant.Plans (Kind (..), childrenOf)
import Olifant.Process (Cancel, runBytes)
import Olifant.Server.Errand (Block (..), Ghost (..), Grounded (..), RunFailure (..), RunResult (..), Status (..), Verb (..), cutBody, errandPrompt, groundingLimit, parseAnswer, runClaude, statusName)
import Olifant.Server.Telemetry (ErrandRecord (..), Record (..), appendRecord, errandId)
import Olifant.Server.State (appendStepLine, projectFor)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative)
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..), PlansConfig (..))
import Olifant.Language.Adapter (languageFor)
import Olifant.Plans (Anchor (..), Line (..), Section (..), Verdict (..), parsePlan, sectionOf)
import Olifant.Server.State (ServerState (..))
import Olifant.Server.Unit (Named (..), fingerprint, unitNamed, unitsOf)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (isAbsolute, takeExtension, (</>))

-- | An absolute path under the project's plans folder, ending `.md`.
isPlanPath :: Project -> FilePath -> Bool
isPlanPath p path = takeExtension path == ".md" && (dir <> "/") `isPrefixOf` normalisePath path
  where
    dir = normalisePath (prRoot p </> pcDir (catalogPlans (prCatalog p)))

-- | An anchor resolved: its file (relative, absolute), its unit and that
-- unit's fingerprint when found, and the file's text when it exists.
data Resolved = Resolved
  { rsRel :: FilePath
  , rsAbs :: FilePath
  , rsUnit :: Maybe Named
  , rsFingerprint :: Maybe Text
  , rsText :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | The unit an anchor names, over the editor's text: none when the file
-- or the unit is missing, the language is unknown, or reading fails.
resolveAnchor :: ServerState -> Project -> Anchor -> IO Resolved
resolveAnchor s p a = do
  text <- maybe (pure Nothing) (safely Nothing . readText (prWorkspace p)) (confined (anPath a))
  unit <- case (text, languageFor (catalogLanguages (prCatalog p)) rel) of
    (Just t, Just lang) -> safely Nothing (either (const Nothing) (unitNamed (anUnit a)) <$> unitsOf (pSyntax (ssPorts s)) lang t)
    _ -> pure Nothing
  pure (Resolved rel (normalisePath (prRoot p </> rel)) unit (fingerprint <$> text <*> fmap namedLines unit) text)
  where
    rel = normalisePath (anPath a)

-- | An anchor's path as one inside the project (normalised, relative), or
-- Nothing: an absolute path, or one that climbs out of the root, is never
-- read. The one place anchors are confined (a later allowlist of sibling
-- roots widens only this).
confined :: FilePath -> Maybe FilePath
confined path
  | isAbsolute path = Nothing
  | rel == ".." || "../" `isPrefixOf` rel = Nothing
  | otherwise = Just rel
  where
    rel = normalisePath path

-- | IO that cannot fail the caller: an exception (not an async one) is the default.
safely :: a -> IO a -> IO a
safely def io =
  try io >>= \case
    Right v -> pure v
    Left (e :: SomeException)
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> pure def

-- | Each anchor resolved once, by its path and unit.
resolveAll :: ServerState -> Project -> [Anchor] -> IO (M.Map (FilePath, Text) Resolved)
resolveAll s p as = M.fromList <$> mapM (\a -> (key a,) <$> resolveAnchor s p a) (nub' as)
  where
    nub' = M.elems . M.fromList . map (\a -> (key a, a))

key :: Anchor -> (FilePath, Text)
key a = (anPath a, anUnit a)

-- | A plan's anchors that no longer hold: one verified at another
-- fingerprint (information), one whose unit is not found (a hint), each
-- over the anchor's columns on its line.
planDiagnostics :: ServerState -> Project -> FilePath -> Text -> IO [Diagnostic]
planDiagnostics s p _ text = do
  let anchored = [(l, a) | l <- parsePlan text, Just a <- [lnAnchor l]]
  resolved <- resolveAll s p (map snd anchored)
  pure (mapMaybe (\(l, a) -> diagnose l a =<< M.lookup (key a) resolved) anchored)
  where
    diagnose l a r = case (rsUnit r, lnVerdict l, rsFingerprint r) of
      (Nothing, _, _) -> Just (at l a DiagnosticSeverity_Hint "? not found")
      (Just _, Verified fp, Just now) | fp /= now -> Just (at l a DiagnosticSeverity_Information ("✗ changed since " <> fp))
      _ -> Nothing
    at l a sev msg = Diagnostic (columns l a) (Just sev) Nothing Nothing (Just "olifant") msg Nothing Nothing Nothing
    columns l a =
      let row = fromIntegral (lnRow l)
          col n = fromIntegral (utf16Length (T.take n (lnText l)))
       in Range (Position row (col (anStart a))) (Position row (col (anEnd a)))

utf16Length :: Text -> Int
utf16Length = T.foldl' (\n c -> n + if fromEnum c > 0xFFFF then 2 else 1) 0

-- | Completions for a line typed up to the cursor: after `@` or `@prefix`,
-- the project's files of a known language; after `@path:prefix`, the
-- units of that file.
anchorItems :: ServerState -> Project -> Text -> IO [CompletionItem]
anchorItems s p upTo = case T.breakOnEnd "@" upTo of
  ("", _) -> pure []
  (_, typed)
    | T.any (== ' ') typed -> pure []
    | (path, unitPrefix) <- T.breakOnEnd ":" typed, not (T.null path) -> do
        units <- case confined (T.unpack (T.dropEnd 1 path)) of
          Nothing -> pure []
          Just rel | Nothing <- languageFor langs rel -> pure []
          Just rel | Just lang <- languageFor langs rel -> safely [] $ readText ws rel >>= \case
            Nothing -> pure []
            Just t -> either (const []) id <$> unitsOf (pSyntax (ssPorts s)) lang t
        pure [item CompletionItemKind_Function (namedName n) (namedName n) | n <- units, unitPrefix `T.isPrefixOf` namedName n]
    | otherwise -> do
        files <- safely [] (listFiles ws ".")
        pure [item CompletionItemKind_File (T.pack f) (T.pack f <> ":") | f <- files, isJust (languageFor langs f), T.unpack typed `isPrefixOf` f]
  where
    ws = prWorkspace p
    langs = catalogLanguages (prCatalog p)
    item kind label insert = CompletionItem label Nothing (Just kind) Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just insert) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | A plan's anchored line: its section, row (0-based) and text; its file
-- (absolute) when that exists, and its unit's definition line (1-based)
-- when found.
data FileItem = FileItem {fiSection :: Text, fiRow :: Int, fiText :: Text, fiAbs :: Maybe FilePath, fiLine :: Maybe Int}
  deriving stock (Eq, Show)

-- | One item per anchored line of a plan, in plan order.
planFiles :: ServerState -> Project -> Text -> IO [FileItem]
planFiles s p text = do
  let ls = parsePlan text
      anchored = [(l, a) | l <- ls, Just a <- [lnAnchor l]]
  resolved <- resolveAll s p (map snd anchored)
  pure
    [ FileItem (secTitle (sectionOf ls (lnRow l))) (lnRow l) (T.strip (lnText l)) (rsAbs r <$ rsText r) (namedDef <$> rsUnit r)
    | (l, a) <- anchored
    , let r = fromMaybe (Resolved (anPath a) (anPath a) Nothing Nothing Nothing) (M.lookup (key a) resolved)
    ]

-- | An errand's answer: its blocks, each ghost line with its anchor's
-- fingerprint when that resolved (Nothing otherwise); the lines the
-- grammar dropped, the anchored lines that did not resolve, the run's
-- seconds and cost, and (a `do`) the files changed in the tree. 'eaId' is
-- the id of the errand's record (outcomes name it).
data ErrandAnswer = ErrandAnswer
  { eaId :: Text
  , eaBlocks :: [(Block, [Maybe Text])]
  , eaDropped :: Int
  , eaUnresolved :: Int
  , eaSeconds :: Double
  , eaCost :: Maybe Double
  , eaChanged :: [FilePath]
  }
  deriving stock (Eq, Show)

-- | Ask, validate or do one line of a plan (its path, its 0-based row): the
-- line grounded in the units its section points at, the runner run from the
-- project's root, its answer read into ghost blocks and logged. Left is one
-- sentence: the line does not fit the verb, the runner failed, or it
-- answered nothing in the grammar. Nothing is written to the plan. Every
-- errand that reaches the runner, however it ends, appends a record to
-- `.olifant/state/errands.jsonl` and a line to the step log; a line the
-- verb refuses writes neither.
runErrand :: Cancel -> Verb -> FilePath -> Int -> ServerState -> IO (ServerState, Either Text ErrandAnswer)
runErrand token verb path row s0 =
  projectFor path s0 >>= \case
    (s, Nothing) -> pure (s, Left "this plan is in no Olifant project")
    (s, Just (_, p)) -> (s,) <$> errand s p
  where
    errand s p = do
      text <- case M.lookup path (ssDocs s) of
        Just (_, t) -> pure t
        Nothing -> fromMaybe "" <$> safely Nothing (readText (pFiles (ssPorts s)) path)
      let ls = parsePlan text
          cfg = catalogPlans (prCatalog p)
          root = prRoot p
      case find ((== row) . lnRow) ls of
        Just target | fits (lnKind target) -> do
          let sec = sectionOf ls row
              around = target : childrenOf ls row ++ [l | l <- ls, lnRow l >= secRow sec, lnRow l < secEnd sec]
              anchors = take groundingLimit (foldl (\acc a -> if any ((== key a) . key) acc then acc else acc ++ [a]) [] [a | l <- around, Just a <- [lnAnchor l]])
          grounded <- catMaybes <$> mapM (\a -> grounding a <$> resolveAnchor s p a) anchors
          let tools = if verb == Do then testTools p else []
              prompt = errandPrompt verb text ls target grounded
              rel = makeRelative root (normalisePath path)
          branch <- currentBranch root
          now <- pNow (ssPorts s)
          -- this ending's record and step log line (neither can fail the
          -- errand); its id, which the answer carries
          let record status why session mrr blocks dropped ghosts resolved unresolved changed = do
                let rid = errandId now verbName rel row session
                appendRecord (ssPorts s) root . AnErrand $
                  ErrandRecord
                    { erId = rid, erAt = now, erVerb = verbName, erPlan = rel, erRow = row, erSection = secTitle sec
                    , erBranch = branch, erModel = pcModel cfg, erStatus = status, erWhy = why
                    , erSession = session, erSeconds = rrSeconds <$> mrr, erTurns = mrr >>= rrTurns
                    , erTokens = mrr >>= rrTokens, erCost = mrr >>= rrCost, erModels = maybe [] rrModels mrr
                    , erPromptChars = T.length prompt, erGrounded = length grounded
                    , erBlocks = blocks, erLines = ghosts, erDropped = dropped, erUnresolved = unresolved
                    , erChanged = changed, erDenials = maybe 0 rrDenials mrr }
                appendStepLine (ssPorts s) root
                  [ "plan-" <> verbName
                  , T.pack rel <> ":" <> tshow row
                  , tshow ghosts <> " lines"
                  , tshow (resolved :: Int) <> " resolve"
                  , tshow (maybe 0 rrSeconds mrr) <> " s"
                  , maybe "cost unknown" (\c -> tshow c <> " usd") (mrr >>= rrCost)
                  , pcModel cfg
                  , statusName status
                  ]
                pure rid
          runClaude token cfg verb tools root prompt >>= \case
            Left f -> do
              changed <- if verb == Do then changedFiles root else pure []
              _ <- record (rfStatus f) (Just (rfWhy f)) (rfSession f) Nothing 0 0 0 0 0 (length changed)
              pure (Left (rfWhy f))
            Right rr -> case parseAnswer verb ls target (rrText rr) of
              ([], dropped) -> do
                changed <- if verb == Do then changedFiles root else pure []
                let why = verbName <> " answered nothing in the grammar"
                _ <- record Empty (Just why) (rrSession rr) (Just rr) 0 dropped 0 0 0 (length changed)
                pure (Left why)
              (blocks, dropped) -> do
                fps <- mapM (mapM (fpOf s p) . blLines) blocks
                changed <- if verb == Do then changedFiles root else pure []
                let ghosts = concatMap blLines blocks
                    resolved = length (filter isJust (concat fps))
                    unresolved = length (filter (isJust . ghAnchor) ghosts) - resolved
                rid <- record Ok Nothing (rrSession rr) (Just rr) (length blocks) dropped (length ghosts) resolved unresolved (length changed)
                pure (Right (ErrandAnswer rid (zip blocks fps) dropped unresolved (rrSeconds rr) (rrCost rr) changed))
        _ -> pure (Left (verbName <> " needs a " <> kindName <> " line"))
    fits = case verb of
      Ask -> (== Question)
      Validate -> \case Claim -> True; Heading _ -> True; _ -> False
      Do -> \case Step _ -> True; TestChild -> True; _ -> False
    verbName = case verb of Ask -> "ask"; Validate -> "validate"; Do -> "do"
    kindName = case verb of Ask -> "question"; Validate -> "claim or heading"; Do -> "step or test"
    -- a resolved unit's lines, for the prompt
    grounding a r = case (rsUnit r, rsText r) of
      (Just n, Just t) ->
        let (from, to) = namedLines n
            ls = T.splitOn "\n" t
         in Just (Grounded a (fromMaybe "" (listToMaybe' (drop (namedDef n - 1) ls))) (cutBody (T.intercalate "\n" (take (to - from + 1) (drop (from - 1) ls)))))
      _ -> Nothing
    listToMaybe' = foldr (const . Just) Nothing
    fpOf s p g = case ghAnchor g of
      Nothing -> pure Nothing
      Just a -> (\r -> rsFingerprint r <* rsUnit r) <$> resolveAnchor s p a
    -- the files the run changed: tracked ones changed since HEAD and new
    -- untracked ones, but not the plan nor Olifant's state; nothing when git
    -- cannot say (no repository, no HEAD). Both calls only read.
    changedFiles root = do
      diffed <- gitOut root ["diff", "--name-only", "--relative", "HEAD"]
      untracked <- gitOut root ["ls-files", "--others", "--exclude-standard"]
      let plan = normalisePath path
      pure $ case (diffed, untracked) of
        (Just d, Just u) -> [f | f <- nub (map (normalisePath . (root </>)) (d ++ u)), f /= plan, not ((normalisePath (root </> ".olifant/state") <> "/") `isPrefixOf` f)]
        _ -> []

-- | The branch checked out at ROOT; Nothing when it is no repository or
-- HEAD is detached.
currentBranch :: FilePath -> IO (Maybe Text)
currentBranch root =
  gitOut root ["rev-parse", "--abbrev-ref", "HEAD"] <&> \case
    Just [b] | b /= "HEAD" -> Just (T.pack b)
    _ -> Nothing

-- | A read-only git command's output lines; Nothing when it fails.
gitOut :: FilePath -> [String] -> IO (Maybe [String])
gitOut root args =
  safely Nothing $ runBytes "git" (["-C", root] ++ args) (Just root) "" >>= \case
    Right (ExitSuccess, out, _) -> pure (Just (map T.unpack (filter (not . T.null) (T.lines (TE.decodeUtf8Lenient out)))))
    _ -> pure Nothing

-- | A `do`'s test tool: the first two words of the project's test command
-- (past an `env` and its assignments), e.g. `Bash(uv run*)`; none without
-- a `[test]`.
testTools :: Project -> [Text]
testTools p = take 1 ["Bash(" <> T.unwords (take 2 (past (testCommand td))) <> "*)" | l <- catalogLanguages (prCatalog p), Just td <- [langTest l], not (null (past (testCommand td)))]
  where
    past = \case
      ("env" : rest) -> dropWhile (\w -> T.isInfixOf "=" w || "-" `T.isPrefixOf` w) rest
      ws -> ws

tshow :: Show a => a -> Text
tshow = T.pack . show
