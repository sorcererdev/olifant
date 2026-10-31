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
  , newStepSnippet
  , newStepText
  , FileItem (..)
  , planFiles
  , ErrandAnswer (..)
  , runErrand
  , Reopen (..)
  , currentBranch
  , gitOut
  , gitText
  , confined
  , safely
  ) where

import Control.Lens ((&), (?~))
import Control.Monad (filterM, when)
import Language.LSP.Protocol.Lens qualified as L
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Functor ((<&>))
import Data.Dynamic (fromDynamic, toDyn)
import Data.IORef (modifyIORef', readIORef)
import Data.List (find, isPrefixOf, nub)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Text.Encoding qualified as TE
import Olifant.Language.Adapter (Language (..), TestDef (..))
import Olifant.Plans (Kind (..), childrenOf)
import Olifant.Process (Cancel, runBytes)
import Olifant.Server.Errand (Block (..), Ghost (..), Grounded (..), RunFailure (..), RunResult (..), Status (..), Verb (..), cutBody, doAnswered, doTestCommand, errandPrompt, projectUnitWords, testWords, groundingLimit, parseAnswer, runClaude, statusName)
import Olifant.Server.LoopTypes (Fact (..))
import Data.Time (getCurrentTimeZone)
import Olifant.Server.Journal (DoEnd (..), doEvent)
import Olifant.Server.Journal qualified as Journal
import Olifant.Server.Review (Review (..), archiveErrand, changeSince, keptDiff, saveReview, takeRef, testsOfFacts)
import Olifant.Server.Telemetry (ErrandRecord (..), Record (..), appendRecord, errandId)
import Olifant.Server.State (appendStepLine, closeDoc, projectFor)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, splitDirectories, takeFileName)
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..), PlansConfig (..))
import Olifant.Language.Adapter (languageFor)
import Olifant.Plans (Anchor (..), Line (..), Section (..), Verdict (..), parsePlan, sectionOf)
import Olifant.Server.State (ServerState (..))
import Olifant.Server.Unit (Named (..), fingerprint, unitLookup, unitsOf)
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
  , rsWhy :: Maybe Text
  -- ^ why the unit is missing, when the units were read: none of that
  -- name, or a name several units have
  }
  deriving stock (Eq, Show)

-- | The unit an anchor names, over the editor's text: none when the file
-- or the unit is missing, the language is unknown, or reading fails.
resolveAnchor :: ServerState -> Project -> Anchor -> IO Resolved
resolveAnchor s p a = do
  text <- maybe (pure Nothing) (safely Nothing . readText (prWorkspace p)) (confined (anPath a))
  found <- case (text, languageFor (catalogLanguages (prCatalog p)) rel) of
    (Just t, Just lang) -> safely (Left Nothing) (either (const (Left Nothing)) (either (Left . Just) Right . unitLookup (anUnit a)) <$> unitsOf (pSyntax (ssPorts s)) lang (Just rel) t)
    _ -> pure (Left Nothing)
  let unit = either (const Nothing) Just found
  pure (Resolved rel (normalisePath (prRoot p </> rel)) unit (fingerprint <$> text <*> fmap namedLines unit) text (either id (const Nothing) found))
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

-- | Completions for a line typed up to the cursor. After `@`, whatever was
-- typed matches fuzzily (a subsequence, any case) the project's units
-- (by unit name) and files (by basename or path): units first, files
-- second, each with a `filterText` (basename, unit, path) so a client can
-- filter on its own; the list is bounded ('anchorLimit'). A unit inserts
-- `path:Unit`; a file inserts `path:` and asks the client to complete again.
-- After `@path:prefix`, the units of that file by prefix.
anchorItems :: ServerState -> Project -> Text -> IO [CompletionItem]
anchorItems s p upTo = case T.breakOnEnd "@" upTo of
  ("", _) -> pure []
  (_, typed)
    | T.any (== ' ') typed -> pure []
    | (path, unitPrefix) <- T.breakOnEnd ":" typed, not (T.null path) -> do
        units <- unitsIn (T.unpack (T.dropEnd 1 path))
        pure [item CompletionItemKind_Function (namedName n) (namedName n) | n <- units, unitPrefix `T.isPrefixOf` namedName n]
    | otherwise -> do
        files <- take fileScan . filter (\f -> isJust (confined f) && not (any (\d -> "." `isPrefixOf` d && d /= ".") (splitDirectories f))) <$> safely [] (listFiles ws ".")
        let fileHits = [f | f <- files, subseq typed (T.pack (takeFileName f)) || subseq typed (T.pack f)]
        -- units are looked for only once something is typed, and only in files of a known language
        unitHits <-
          if T.null typed
            then pure []
            else
              fmap concat . mapM (\f -> map (f,) <$> unitsIn f) $ filter (isJust . languageFor langs) files
        let units = [unitItem f n | (f, n) <- unitHits, subseq typed (namedName n)]
            filesItems = [fileItem f | f <- fileHits]
        pure (take anchorLimit (units ++ filesItems))
  where
    ws = prWorkspace p
    langs = catalogLanguages (prCatalog p)
    unitsIn rel = case confined rel of
      Nothing -> pure []
      Just r | Nothing <- languageFor langs r -> pure []
      Just r | Just lang <- languageFor langs r -> safely [] $ readText ws r >>= \case
        Nothing -> pure []
        Just t -> cachedUnits r lang t
    cachedUnits r lang t = do
      let k = (langName lang, r)
      known <- M.lookup k <$> readIORef (ssUnitCache s)
      case known of
        Just (t', d) | t' == t, Just us <- fromDynamic d -> pure us
        _ -> do
          us <- either (const []) id <$> unitsOf (pSyntax (ssPorts s)) lang (Just r) t
          modifyIORef' (ssUnitCache s) (M.insert k (t, toDyn us))
          pure us
    subseq needle hay = go (T.unpack (T.toLower needle)) (T.unpack (T.toLower hay))
      where
        go [] _ = True
        go _ [] = False
        go (x : xs) (y : ys) = if x == y then go xs ys else go (x : xs) ys
    blank kind label insert = CompletionItem label Nothing (Just kind) Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just insert) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    item = blank
    unitItem f n =
      let full = T.pack f <> ":" <> namedName n
       in blank CompletionItemKind_Function full full & L.filterText ?~ T.unwords [T.pack (takeFileName f), namedName n, T.pack f]
    fileItem f =
      -- a file with units inserts `path:` and completes again; one without is just `path`
      if isJust (languageFor langs f)
        then
          blank CompletionItemKind_File (T.pack f) (T.pack f <> ":")
            & L.filterText ?~ T.unwords [T.pack (takeFileName f), T.pack f]
            & L.command ?~ Command "complete the unit" "editor.action.triggerSuggest" Nothing
        else blank CompletionItemKind_File (T.pack f) (T.pack f) & L.filterText ?~ T.unwords [T.pack (takeFileName f), T.pack f]

-- | How many anchor completions at most, and how many files are searched
-- for units in one request.
anchorLimit, fileScan :: Int
anchorLimit = 50
fileScan = 400

-- | A new step as a snippet (holes: the step, the anchor) and as plain text
-- (the same line with its holes filled by their placeholders).
newStepSnippet, newStepText :: Text
newStepSnippet = "- [ ] ${1:step} @${2:anchor}"
newStepText = "- [ ] step @anchor"

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
    , let r = fromMaybe (Resolved (anPath a) (anPath a) Nothing Nothing Nothing Nothing) (M.lookup (key a) resolved)
    ]

-- | An errand's answer: its blocks, each ghost line with its anchor's
-- fingerprint when that resolved (Nothing otherwise); the lines the
-- grammar dropped, the anchored lines that did not resolve, the run's
-- seconds and cost, and (a `do`) the files changed in the tree and the
-- facts its step's tests left (their marks, its tick). 'eaId' is the id of
-- the errand's record (outcomes name it).
data ErrandAnswer = ErrandAnswer
  { eaId :: Text
  , eaBlocks :: [(Block, [Maybe Text])]
  , eaDropped :: Int
  , eaUnresolved :: Int
  , eaSeconds :: Double
  , eaCost :: Maybe Double
  , eaChanged :: [FilePath]
  , eaFacts :: [Fact]
  , eaReview :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | Ask, validate or do one line of a plan (its path, its 0-based row): the
-- line grounded in the units its section points at, the runner run from the
-- project's root, its answer read into ghost blocks and logged. Left is one
-- sentence: the line does not fit the verb, the runner failed, or it
-- answered nothing in the grammar. Nothing is written to the plan. Every
-- errand that reaches the runner, however it ends, appends a record to
-- `.olifant/state/errands.jsonl` and a line to the step log; a line the
-- verb refuses writes neither. A `do` that answered runs AFTER over the
-- plan's lines and its row (the step's tests) before its record. A `do`
-- in a git repository takes the tree's reference first; one that answered,
-- or changed the tree however it ended, leaves a review (its id to
-- REVIEWED, and in the answer).
-- | A do that continues an earlier one's thread: its id, its session, and
-- what the user asked for.
data Reopen = Reopen {roParent :: Text, roSession :: Text, roAsk :: Text}
  deriving stock (Eq, Show)

runErrand :: (ServerState -> Project -> [Line] -> Int -> IO [Fact]) -> (Text -> IO ()) -> Maybe Reopen -> Cancel -> Verb -> FilePath -> Int -> ServerState -> IO (ServerState, Either Text ErrandAnswer)
runErrand after reviewed reopen token verb path row s0 =
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
              asked = maybe "" (\ro -> "Further change asked: " <> roAsk ro <> "\n\n") reopen
              prompt = asked <> errandPrompt verb (projectUnitWords (catalogLanguages (prCatalog p))) (if verb == Do then listToMaybe' (mapMaybe doTestCommand (projectTests p)) else Nothing) text ls target grounded
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
                    , erChanged = changed, erDenials = maybe 0 rrDenials mrr, erParent = roParent <$> reopen }
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
          let onDisk f = safely Nothing (readText (pFiles (ssPorts s)) f)
              -- the state and project over the files as a do left them: the
              -- editor's text of a file it wrote is older than the disk (the
              -- editor reloads only after the answer), so it is set aside for
              -- the step's tests
              landed before = do
                wrote <- filterM (\(f, was) -> (/= was) <$> onDisk f) before
                projectFor path (foldr (closeDoc . fst) s wrote) <&> \case
                  (s', Just (_, p')) -> (s', p')
                  (_, Nothing) -> (s, p)
          -- before a do, the disk under each file open in the editor (but
          -- the plan), to tell which ones it wrote
          before <- if verb == Do then mapM (\f -> (f,) <$> onDisk f) [f | f <- M.keys (ssDocs s), f /= normalisePath path, (normalisePath root <> "/") `isPrefixOf` f] else pure []
          -- a do's tree before it: what its review is the change since
          ref <- if verb == Do then safely Nothing (takeRef root) else pure Nothing
          let -- a do's review, when it answered or changed the tree (its id);
              -- every do is archived and journaled, however it ended
              review rid status why mrr ghosts facts answered
                | verb /= Do = pure Nothing
                | otherwise = do
                    changes <- maybe (pure []) (safely [] . changeSince root [rel]) ref
                    let said = [l | Just rr <- [mrr], l <- map T.strip (T.lines (rrText rr)), any (`T.isPrefixOf` l) ["DONE", "BLOCKED", "- ? "]]
                        r = Review
                          { rvId = rid, rvErrand = rid, rvPlan = rel, rvRow = lnRow target, rvStep = lnText target
                          , rvAt = now, rvModel = pcModel cfg, rvSeconds = maybe 0 rrSeconds mrr, rvCost = mrr >>= rrCost
                          , rvStatus = status, rvWhy = why
                          , rvSaid = said
                          , rvLines = map ghText ghosts
                          , rvFacts = [(n, maybe "" lnText (find ((== n) . lnRow) ls), t) | Replace n t <- facts]
                          , rvTests = testsOfFacts [t | Replace _ t <- facts]
                          , rvDenials = maybe [] rrDenied mrr, rvTools = tools ++ pcDoTools cfg, rvAllowed = []
                          , rvRef = ref, rvPrompt = prompt, rvAnswer = maybe "" rrText mrr, rvSession = mrr >>= rrSession
                          , rvParent = roParent <$> reopen, rvDiff = keptDiff changes, rvChoice = Nothing }
                        end = case status of
                          "answered" -> DoAnswered (case [T.strip d | l <- said, Just d <- [T.stripPrefix "DONE" l]] of
                            d : _ | not (T.null d) -> d
                            _ -> T.pack (show (length changes)) <> (if length changes == 1 then " file changed" else " files changed"))
                          "blocked" -> DoBlocked (fromMaybe "no reason given" why)
                          "empty" -> DoEmpty
                          "timeout" -> DoTimeout
                          "cancelled" -> DoCancelled
                          -- the runner by its name, not its path
                          _ -> DoError (T.replace (T.pack (pcRunner cfg)) (T.pack (takeFileName (pcRunner cfg))) (fromMaybe status why))
                    safely () (archiveErrand (ssPorts s) root r)
                    zone <- getCurrentTimeZone
                    _ <- safely [] (Journal.record (ssPorts s) zone root rel [doEvent rid (lnText target) (lnRow target) now (isJust reopen) (pcModel cfg) end])
                    case ref of
                      Just _ | answered || not (null changes) -> do
                        saved <- safely False (saveReview (ssPorts s) root r >> pure True)
                        if saved then reviewed rid >> pure (Just rid) else pure Nothing
                      _ -> pure Nothing
          runClaude token cfg verb tools (roSession <$> reopen) root prompt >>= \case
            Left f -> do
              changed <- if verb == Do then changedFiles root else pure []
              rid <- record (rfStatus f) (Just (rfWhy f)) (rfSession f) Nothing 0 0 0 0 0 (length changed)
              _ <- review rid (statusName (rfStatus f)) (Just (rfWhy f)) Nothing [] [] False
              pure (Left (rfWhy f))
            Right rr -> case parseAnswer verb ls target (rrText rr) of
              ([], dropped) | verb /= Do || not (doAnswered (rrText rr)) -> do
                changed <- if verb == Do then changedFiles root else pure []
                let why = verbName <> " answered nothing in the grammar"
                rid <- record Empty (Just why) (rrSession rr) (Just rr) 0 dropped 0 0 0 (length changed)
                _ <- review rid "empty" (Just why) (Just rr) [] [] False
                pure (Left why)
              (blocks, dropped) -> do
                fps <- mapM (mapM (fpOf s p) . blLines) blocks
                changed <- if verb == Do then changedFiles root else pure []
                facts <- if verb == Do then safely [] (landed before >>= \(s', p') -> after s' p' ls row) else pure []
                let ghosts = concatMap blLines blocks
                    resolved = length (filter isJust (concat fps))
                    unresolved = length (filter (isJust . ghAnchor) ghosts) - resolved
                    blockedWhy = listToMaybe' [T.strip w | l <- T.lines (rrText rr), Just w <- [T.stripPrefix "BLOCKED" (T.strip l)]]
                rid <- record Ok Nothing (rrSession rr) (Just rr) (length blocks) dropped (length ghosts) resolved unresolved (length changed)
                rv <- review rid (maybe "answered" (const "blocked") blockedWhy) blockedWhy (Just rr) ghosts facts True
                -- after its own line, the do's tests journaled against the plan as the do read it
                when (verb == Do && not (null facts)) $ safely () $ do
                  fp <- maybe (pure Nothing) (fmap rsFingerprint . resolveAnchor s p) (lnAnchor target)
                  zone <- getCurrentTimeZone
                  Journal.recordTests (ssPorts s) zone root rel ls row facts fp now
                pure (Right (ErrandAnswer rid (zip blocks fps) dropped unresolved (rrSeconds rr) (rrCost rr) changed facts rv))
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

-- | A read-only git command's whole output; Nothing when it fails.
gitText :: FilePath -> [String] -> IO (Maybe Text)
gitText root args =
  runBytes "git" (["-C", root] ++ args) (Just root) "" <&> \case
    Right (ExitSuccess, out, _) -> Just (TE.decodeUtf8Lenient out)
    _ -> Nothing

-- | A `do`'s test tool: the first two words of the project's test command
-- (past an `env` and its assignments), e.g. `Bash(uv run*)`; none without
-- a `[test]`.
testTools :: Project -> [Text]
testTools p = take 1 ["Bash(" <> T.unwords (take 2 ws) <> "*)" | ws <- map testWords (projectTests p), not (null ws)]

-- | The project's test commands, the first language's first.
projectTests :: Project -> [[Text]]
projectTests p = [testCommand td | l <- catalogLanguages (prCatalog p), Just td <- [langTest l]]

tshow :: Show a => a -> Text
tshow = T.pack . show
