-- | The engine's operations, as both front ends use them: the CLI presents
-- them as JSON or text, the language server as LSP. A 'Project' is a
-- catalog plus the workspace it reads (disk, or open buffers over disk).
module Olifant.App
  ( Ports (..)
  , newnessOf
  , newnessWith
  , baseRefOf
  , legacyOf
  , migrationFor
  , findingsWith
  , Project (..)
  , projectRoot
  , nearestProject
  , openProject
  , openProjectWith
  , generatorsOf
  , subsOf
  , listGenerators
  , ShowFailure (..)
  , showGenerator
  , doctorChecks
  , projectModel
  , projectTools
  , projectQueries
  , queriesAt
  , actionsAt'
  , actionsIn
  , completeAt
  , findings
  , chooseFixInputs
  , toPascal
  , resolveChoicesFor
  , choiceViewsFor
  , placeAt
  , snippetAtCursor
  , PlanOptions (..)
  , planOptions
  , planWith
  , noFill
  , modelFiller
  , modelFillerWith
  , FixFailure (..)
  , fixSteps
  ) where

import Data.Set (Set)
import Control.Applicative ((<|>))
import Control.Monad (forM)
import Data.Aeson qualified as J
import Olifant.Glob (matchesGlob)
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Char (isAlphaNum)
import Data.List (find, sortOn, sort)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Olifant.Actions (Action, Completion, FileAction, Nearby, actionsAt, actionsInFile, completionsAt)
import Olifant.Anchor (astQueries)
import Olifant.Catalog
import Olifant.ProjectModel (ProjectModel, buildModel)
import Olifant.Cursor (afterIndent, afterRule, atCursor, atCursorIndented, cursorIndent, isSnippet)
import Olifant.Fill (ModelFiller (..))
import Olifant.Generation
import Olifant.Generator (AppliesAt (..), ChoiceView (..), Choices (..), Generator, GeneratorDef, GeneratorId (..), GeneratorWith (..), InputSpec (..), InputType (..), Scalar (..), UseStep (..), choiceViews, generatorIdText, generatorName, needsChoices, replaceGenerator, withChoices)
import Olifant.Holes (HoleSite (..))
import Olifant.Language (Checker, Language (..), Match (..), Syntax, languageFor, querySyntax)
import Olifant.Process (Cancel, cancelled, neverCancel)
import Olifant.Model (ModelConfig (..), backendModel, curlFillerWith)
import Olifant.Query (Queries (..), QueryFailure (..), processCommand)
import Olifant.Newness (NewLines (..), newSince, wholeNew)
import Olifant.Migration (MigrationReport (..), Switch (..), bigChanges, readRegistry, switchMarkers, unregistered)
import Olifant.Rules (Exception, FixDef (..), FixOffer (..), Finding (..), Replacement (..), RuleDef (..), Severity (..), checkFiles, definedName, fingerprint, suggestChoice)
import Olifant.Snippet (Snippet, snippetOf)
import Olifant.Template (describeTemplateError, render)
import Olifant.Template qualified as Tm
import Olifant.Template.Syntax (Segment (..), Template (..))
import Olifant.Uncovered (uncoveredQuery)
import Olifant.Workspace (PathState (..), Workspace (..), within)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, takeFileName, (</>))
import Olifant.Doctor (Check, DoctorInput (..), DoctorPorts, doctor)

-- | What the engine needs from the world: the file system (absolute
-- paths), the syntax and compiler adapters, the clock, and where user
-- configuration lives.
data Ports = Ports
  { pFiles :: Workspace IO
  , pSyntax :: Syntax IO
  , -- | checks and verifies, stopped (their processes killed) when the token trips
    pChecker :: Cancel -> Checker IO
  , pVerifier :: Cancel -> Verifier IO
  , pNow :: IO UTCTime
  , pUserConfigDir :: Maybe FilePath
  , pHomeDir :: Maybe FilePath
  , -- | what `doctor` looks at: installed tools, the model backends
    pDoctor :: DoctorPorts IO
  , -- | the files version control tracks under a folder (relative to it);
    -- nothing outside version control
    pTracked :: FilePath -> IO (Maybe (Set FilePath))
  , -- | a file's text at the base (the merge base of HEAD and a ref) and that
    -- commit's id: Right Nothing when the file is not there (new); Left why
    -- when there is no base (root, ref, project-relative path)
    pBaseText :: FilePath -> Text -> FilePath -> IO (Either Text (Text, Maybe Text))
  , -- | the files at the base (root, ref), project-relative; Left when
    -- there is no base
    pBaseFiles :: FilePath -> Text -> IO (Either Text [FilePath])
  , -- | what the base depends on (root, ref): equal stamps, the same base
    -- (the server's base cache is good while it holds)
    pBaseStamp :: FilePath -> Text -> IO Text
  }

-- | An open project: its root (absolute), its catalog, and the workspace
-- its operations read, relative to the root (open buffers over disk in the
-- server, disk or one overlaid file in the CLI).
data Project = Project
  { prRoot :: FilePath
  , prCatalog :: Catalog
  , prWorkspace :: Workspace IO
  }

-- | The nearest folder at or above @start@ holding `.olifant/` (not above
-- the home directory); @start@ itself when there is none.
projectRoot :: Monad m => Workspace m -> Maybe FilePath -> FilePath -> m FilePath
projectRoot ws home start = fromMaybe start <$> nearestProject ws home start

-- | The nearest folder at or above @start@ holding `.olifant/` (not above
-- the home directory), if any.
nearestProject :: Monad m => Workspace m -> Maybe FilePath -> FilePath -> m (Maybe FilePath)
nearestProject ws home = go
  where
    go dir
      | Just dir == home = pure Nothing
      | otherwise =
          inspectPath ws (dir </> ".olifant") >>= \case
            Directory -> pure (Just dir)
            _ | takeDirectory dir == dir -> pure Nothing
              | otherwise -> go (takeDirectory dir)

-- | The project whose root is @root@, its catalog loaded, reading disk.
openProject :: Ports -> FilePath -> IO (Either CatalogError Project)
openProject ports root =
  openProjectCatalog (pFiles ports) (CatalogPaths root (pUserConfigDir ports) (pHomeDir ports))
    <&> fmap (\cat -> Project root cat (within root (pFiles ports)))

-- | 'openProject', its packs from a cache shared with other projects.
openProjectWith :: PackCache -> Ports -> FilePath -> IO (Either CatalogError Project)
openProjectWith cache ports root =
  openProjectCatalogCached cache (pFiles ports) (CatalogPaths root (pUserConfigDir ports) (pHomeDir ports))
    <&> fmap (\cat -> Project root cat (within root (pFiles ports)))

generatorsOf :: Project -> [(Text, GeneratorDef)]
generatorsOf project = concat [legacyPaths (generatorIdText (entryId e), entryDefinition e) | e <- catalogEntries (prCatalog project)]
  where
    -- `applies_at.path = "{legacy}"`: the project's legacy globs (a copy per
    -- glob; none without [migrate])
    legacyPaths (gid, d) = case genAppliesAt d of
      Just ap | apPath ap == "{legacy}" -> [(gid, d {genAppliesAt = Just ap {apPath = g}}) | g <- legacyOf project]
      _ -> [(gid, d)]

-- | A generator's [[uses]] steps with the entries they resolve to.
subsOf :: Catalog -> CatalogEntry -> [(UseStep, Maybe CatalogEntry)]
subsOf cat e = [(u, either (const Nothing) Just (resolveEntry cat (packOfEntry e) (useGenerator u))) | u <- genUses (entryDefinition e)]

-- | The generators (`list`): the valid ones (with their `choices_from`
-- resolved and each one's choices, when @detail@), and the broken ones
-- with why — sources and fixed targets checked, so a broken generator
-- shows before anyone runs it.
listGenerators :: Ports -> Project -> Bool -> IO ([CatalogEntry], [(GeneratorId, FilePath, Text)], CatalogEntry -> M.Map Text ChoiceView)
listGenerators ports project detail = do
  let cat = prCatalog project
  checked <- traverse (\e -> (e,) <$> checkEntry (pFiles ports) (pSyntax ports) cat e) (catalogEntries cat)
  let broken =
        catalogBroken cat
          ++ [(entryId e, entryDir e </> "generator.toml", why) | (e, Just why) <- checked]
          ++ [(GeneratorId ("language " <> T.pack (takeBaseName path)), path, why) | (path, why) <- catalogBrokenLanguages cat]
      ok = [e | (e, Nothing) <- checked]
  entries <-
    if detail
      then traverse (\e -> either (const e) (\d -> e {entryDefinition = d}) <$> resolveChoicesFor ports project M.empty (entryDefinition e)) ok
      else pure ok
  viewList <- if detail then traverse (\e -> (entryId e,) . either (const M.empty) M.fromList <$> choiceViewsFor ports project M.empty (entryDefinition e)) ok else pure []
  let views e = M.findWithDefault M.empty (entryId e) (M.fromList viewList)
  pure (entries, broken, views)

-- | Why `show` could not describe a generator.
data ShowFailure = ShowCatalog CatalogError | ShowInvalid Text

-- | One generator with its choices, given the inputs set so far (`show`).
showGenerator :: Ports -> Project -> Text -> M.Map Text Text -> IO (Either ShowFailure (CatalogEntry, [(Text, ChoiceView)]))
showGenerator ports project ref sets = case findEntry (prCatalog project) ref of
  Left err -> pure (Left (ShowCatalog err))
  Right e0 ->
    checkEntry (pFiles ports) (pSyntax ports) (prCatalog project) e0 >>= \case
      Just why -> pure (Left (ShowInvalid (generatorIdText (entryId e0) <> ": " <> why)))
      Nothing -> do
        resolved <- resolveChoicesFor ports project sets (entryDefinition e0)
        views <- choiceViewsFor ports project sets (entryDefinition e0)
        pure $ case (resolved, views) of
          (Left why, _) -> Left (ShowInvalid why)
          (_, Left why) -> Left (ShowInvalid why)
          (Right d, Right vs) -> Right (e0 {entryDefinition = d}, vs)

-- | The project in the shared vocabulary (spec §1b), by its packs'
-- `model.toml`; its ids are the project's `name` (config.toml; else its
-- root folder's name) and a path-like name.
projectModel :: Ports -> Project -> IO ProjectModel
projectModel ports project = do
  let ws = prWorkspace project
  paths <- listFiles ws "."
  buildModel (projectQueries ports project) paths (readText ws) (catalogModelMaps (prCatalog project)) (fromMaybe (T.pack (takeFileName (prRoot project))) (catalogName (prCatalog project)))

-- | What generators need here (`doctor`): tools, packs, the model.
doctorChecks :: Ports -> Project -> IO [Check]
doctorChecks ports project =
  doctor (pDoctor ports) $
    DoctorInput
      (catalogProject cat)
      (catalogPacks cat)
      (catalogLanguages cat)
      [(T.pack p, why) | (p, why) <- catalogBrokenLanguages cat]
      [(generatorIdText g, why) | (g, _, why) <- catalogBroken cat]
      (catalogModel cat)
      (catalogDecideModel cat)
  where
    cat = prCatalog project

-- | The tools a command in this project uses: the ports' syntax, checker
-- and verifier, the catalog's languages and regions, and the project's
-- queries.
projectTools :: Ports -> Project -> Tools IO
projectTools ports project =
  (basicTools (pSyntax ports) (pChecker ports neverCancel) (catalogLanguages cat)) {toolRegions = catalogRegions cat, toolQueries = projectQueries ports project, toolVerifier = pVerifier ports neverCancel}
  where
    cat = prCatalog project

-- | Queries run in the project: commands in its root, AST queries over its
-- files with the catalog's language adapters.
projectQueries :: Ports -> Project -> Queries IO
projectQueries ports project =
  Queries
    { queryCommand = processCommand (prRoot project)
    , queryAst = astQueries (pSyntax ports) (catalogLanguages (prCatalog project))
    , queryUncovered = \_ -> pure (Right [])
    }

-- | 'projectQueries' with a cursor: `uncovered` queries ask the compiler
-- about the match there (spec §4.9), reading the project's workspace.
queriesAt :: Ports -> Project -> FilePath -> Int -> Queries IO
queriesAt = queriesAtWith neverCancel

-- | 'queriesAt', its compiler checks stopped when the token trips.
queriesAtWith :: Cancel -> Ports -> Project -> FilePath -> Int -> Queries IO
queriesAtWith token ports project file line =
  (projectQueries ports project) {queryUncovered = uncoveredQuery (pChecker ports token) (catalogLanguages (prCatalog project)) (prWorkspace project) file line}

-- | The ⚡ actions at a cursor in a project file (spec §4.8).
actionsAt' :: Ports -> Project -> FilePath -> (Int, Int) -> IO [Action]
actionsAt' ports project rel pos = do
  let ws = prWorkspace project
  content <- readText ws rel
  paths <- listFiles ws "."
  actionsAt (pSyntax ports) (catalogLanguages (prCatalog project)) (queriesAt ports project rel (fst pos)) paths (readText ws) (generatorsOf project) rel pos (fromMaybe "" content)

-- | Every action a file offers, with where, and its near misses.
actionsIn :: Ports -> Project -> FilePath -> IO ([FileAction], [Nearby])
actionsIn ports project rel = do
  let ws = prWorkspace project
  content <- readText ws rel
  paths <- listFiles ws "."
  actionsInFile (pSyntax ports) (catalogLanguages (prCatalog project)) (projectQueries ports project) paths (readText ws) (generatorsOf project) rel (fromMaybe "" content)

-- | The snippets a cursor may complete (spec §4.9).
completeAt :: Ports -> Project -> FilePath -> (Int, Int) -> IO [Completion]
completeAt ports project rel pos = do
  let ws = prWorkspace project
  content <- readText ws rel
  paths <- listFiles ws "."
  completionsAt (pSyntax ports) (catalogLanguages (prCatalog project)) (queriesAt ports project rel (fst pos)) paths (readText ws) (generatorsOf project) rel pos (fromMaybe "" content)

-- | Which lines of a project file (its current TEXT) are new since the
-- base: against the merge base of HEAD and the base ref.
newnessOf :: Ports -> Project -> FilePath -> Text -> IO NewLines
newnessOf ports = newnessWith (pBaseText ports)

-- | 'newnessOf' with the base text from BASE (the server caches it).
newnessWith :: (FilePath -> Text -> FilePath -> IO (Either Text (Text, Maybe Text))) -> Project -> FilePath -> Text -> IO NewLines
newnessWith base project rel text =
  base (prRoot project) (baseRefOf project) rel <&> \case
    Left why -> NoBase why
    Right (_, Nothing) -> AllNew
    Right (_, Just b) -> newSince b text

-- | How far the migration has come ([migrate]; none without it): legacy
-- definitions now and at the base, those new since (not delegates, which
-- carry their allowance), the switches now and at the base, and markers the
-- registry lacks.
migrationFor :: Ports -> Project -> IO (Maybe MigrationReport)
migrationFor ports project = case catalogMigrate cat of
  Nothing -> pure Nothing
  Just mig -> do
    paths <- listFiles ws "."
    let legacy = [p | p <- paths, any (`matchesGlob` p) (mgLegacy mig)]
        kinds = J.object ["any" J..= [J.object ["kind" J..= k] | k <- mgCount mig]]
        defs text p = case languageFor (catalogLanguages cat) p of
          Nothing -> pure []
          Just lang -> either (const []) id <$> querySyntax (pSyntax ports) lang kinds text
    perFile <- forM legacy $ \p -> do
      text <- fromMaybe "" <$> readText ws p
      now <- defs text p
      base <- pBaseText ports (prRoot project) (mgBase mig) p
      before <- case base of
        Left _ -> pure Nothing
        Right (_, Nothing) -> pure (Just 0)
        Right (_, Just b) -> Just . length <$> defs b p
      let newness = case base of
            Left why -> NoBase why
            Right (_, Nothing) -> AllNew
            Right (_, Just b) -> newSince b text
          ls = T.lines text
          allowed m = let l = fst (matchStart m) in l > 0 && "olifant:allow legacy-bug-fix-only" `T.isInfixOf` (ls !! (l - 1))
          added = [nameOf m | m <- now, wholeNew newness (lineRange m), not (allowed m)]
      pure (length now, before, added, switchMarkers text)
    -- the registry: [migrate] switches, else where the switch generator
    -- writes it (a switches.py outside the legacy code)
    let registryPath = case mgSwitches mig of
          Just r -> Just r
          Nothing -> listToMaybe [p | p <- paths, takeFileName p == "switches.py", p `notElem` legacy]
    registry <- maybe (pure Nothing) (readText ws) registryPath
    registryBase <- case registryPath of
      Nothing -> pure (Just [])
      Just r ->
        pBaseText ports (prRoot project) (mgBase mig) r <&> \case
          Left _ -> Nothing
          Right (_, Nothing) -> Just []
          Right (_, Just b) -> Just (map swName (readRegistry b))
    -- the base counted over the base's own legacy files: a file deleted
    -- (migrated away) or renamed since still counts there
    baseCount <-
      pBaseFiles ports (prRoot project) (mgBase mig) >>= \case
        Left _ -> pure Nothing
        Right baseFiles -> fmap (fmap sum . sequence) . forM [p | p <- baseFiles, any (`matchesGlob` p) (mgLegacy mig)] $ \p ->
          pBaseText ports (prRoot project) (mgBase mig) p >>= \case
            Right (_, Just b) -> Just . length <$> defs b p
            Right (_, Nothing) -> pure (Just 0)
            Left _ -> pure Nothing
    let switches = maybe [] readRegistry registry
        befores = [b | (_, b, _, _) <- perFile]
    pure . Just $
      MigrationReport
        { mrLegacyNow = sum [n | (n, _, _, _) <- perFile]
        , mrLegacyBase = maybe (sum <$> sequence befores) Just baseCount
        , mrSwitches = switches
        , mrSwitchesBase = registryBase
        , mrAddedLegacy = concat [a | (_, _, a, _) <- perFile]
        , mrUnregistered = unregistered (concat [m | (_, _, _, m) <- perFile]) switches
        }
  where
    cat = prCatalog project
    ws = prWorkspace project
    lineRange m = ((fst (matchStart m) + 1, 1), (fst (matchEnd m) + 1, 1))
    -- a definition's name: the identifier after `def`/`class`/`fn`
    nameOf m = case T.words (T.takeWhile (/= '(') (T.takeWhile (/= ':') (matchText m))) of
      _ : name : _ -> name
      _ -> T.strip (matchText m)

-- | New work in disguise ([migrate]): a legacy function changed by at least
-- `new_work_lines` new lines, not wholly new — a quiet hint, with the
-- extract fixes (a configured decider may sharpen it: the server's job).
newWorkHints :: Ports -> Project -> (FilePath -> Text -> IO NewLines) -> [FilePath] -> IO [Finding]
newWorkHints ports project newness files = case catalogMigrate cat of
  Nothing -> pure []
  Just mig -> fmap concat . forM [f | f <- files, any (`matchesGlob` f) (mgLegacy mig)] $ \f -> do
    text <- fromMaybe "" <$> readText ws f
    case languageFor (catalogLanguages cat) f of
      Nothing -> pure []
      Just lang -> do
        ms <- either (const []) id <$> querySyntax (pSyntax ports) lang (J.object ["any" J..= [J.object ["kind" J..= k] | k <- mgCount mig]]) text
        n <- newness f text
        let ranged = [(((fst (matchStart m) + 1, 1), (fst (matchEnd m) + 1, snd (matchEnd m) + 1)), m) | m <- ms]
            big = bigChanges (mgNewWorkLines mig) n (map fst ranged)
        pure
          [ Finding "migrate-new-work" f (Just rng) Hint ("a big change (" <> T.pack (show k) <> " new lines): is this new work? New work goes in the new architecture") (extractFixes f) (matchText m) (fingerprint ["migrate-new-work", matchText m])
          | (rng, k) <- big
          , Just m <- [lookup rng ranged]
          ]
  where
    cat = prCatalog project
    ws = prWorkspace project
    extractFixes f =
      [ FixOffer ("python-strangler/extract-" <> kind) (Just ("Extract this as a " <> kind)) [("file", T.pack f)] Nothing []
      | kind <- ["command", "query"]
      ]

-- | Whether a node's text is a definition (def, class, fn, …).
isDefinition :: Text -> Bool
isDefinition t = case T.words (T.stripStart t) of
  kw : _ -> kw `elem` ["def", "async", "class", "fn", "pub", "struct", "enum", "trait"]
  [] -> False

-- | Where NAME is called (`NAME(`, not part of a longer name), in files
-- with REL's extension, its own definition (LINES of REL) left out:
-- `path:line`, in path order.
callersOf :: Workspace IO -> FilePath -> (Int, Int) -> Text -> IO [Text]
callersOf ws rel (l1, l2) name
  | T.null name = pure []
  | otherwise = do
      paths <- listFiles ws "."
      fmap concat . forM (sort [p | p <- paths, takeExtension p == takeExtension rel]) $ \p -> do
        text <- fromMaybe "" <$> readText ws p
        pure
          [ T.pack p <> ":" <> T.pack (show i)
          | (i, l) <- zip [1 :: Int ..] (T.lines text)
          , not (p == rel && i >= l1 && i <= l2)
          , calls l
          ]
  where
    calls l = any starts (T.breakOnAll (name <> "(") l)
    starts (before, _) = case T.unsnoc before of
      Just (_, c) -> not (isAlphaNum c || c == '_') && not ("def " `T.isSuffixOf` before)
      Nothing -> True

-- | The plan's note about a definition's callers.
callersNote :: Text -> [Text] -> [Text]
callersNote _ [] = []
callersNote name cs =
  [ name <> " stays, so its " <> T.pack (show (length cs)) <> " caller(s) keep working: " <> T.intercalate ", " (take 10 cs) <> (if length cs > 10 then ", …" else "") ]

-- | A node's text with its indentation taken off the lines after the
-- first (ast-grep gives the first line from the node's column, the rest
-- whole): the text as if the node were at column 0.
relativeTo :: Text -> Text -> Text
relativeTo indent found = case T.lines found of
  [] -> found
  l : ls -> T.intercalate "\n" (l : [fromMaybe x (T.stripPrefix indent x) | x <- ls])

-- | A definition's signature: its lines up to the colon that ends it, the
-- first `:` outside brackets and strings (`def f(\n    a,\n) -> dict:`);
-- the first line when there is none.
signatureHead :: Text -> Text
signatureHead found = case go (0 :: Int) Nothing (T.unpack found) 0 of
  Just n -> T.intercalate "\n" (take (n + 1) (T.lines found))
  Nothing -> fromMaybe "" (listToMaybe (T.lines found))
  where
    go _ _ [] _ = Nothing
    go depth (Just q) (c : cs) line
      | c == '\\' = case cs of
          d : rest -> go depth (Just q) rest (if d == '\n' then line + 1 else line)
          [] -> Nothing
      | c == q = go depth Nothing cs line
      | otherwise = go depth (Just q) cs (if c == '\n' then line + 1 else line)
    go depth Nothing (c : cs) line
      | c == '\n' = go depth Nothing cs (line + 1)
      | c `elem` ("\"'" :: String) = go depth (Just c) cs line
      | c `elem` ("([{" :: String) = go (depth + 1) Nothing cs line
      | c `elem` (")]}" :: String) = go (max 0 (depth - 1)) Nothing cs line
      | c == ':' && depth == 0 = Just line
      | otherwise = go depth Nothing cs line

-- | A switched function's new path (the strangler's retire): the lines
-- inside its `if switches.on(…):` block, a level less indented — what the
-- function becomes once its legacy body goes.
switchPath :: Text -> Text
switchPath found =
  let ls = T.lines found
      indentOf = T.length . T.takeWhile (== ' ')
   in case break (T.isPrefixOf "if switches.on(" . T.stripStart) ls of
        (_, guard : rest) ->
          let level = indentOf guard
              body = takeWhile (\l -> T.null (T.strip l) || indentOf l > level) rest
              inner = case filter (not . T.null . T.strip) body of
                l : _ -> indentOf l - level
                [] -> 0
           in T.intercalate "\n" [T.drop inner l | l <- body, not (T.null (T.strip l))]
        _ -> ""

-- | What "new" is measured from: the base ref ([migrate] base; main).
baseRefOf :: Project -> Text
baseRefOf project = maybe "main" mgBase (catalogMigrate (prCatalog project))

-- | The project's legacy globs ([migrate] legacy; none).
legacyOf :: Project -> [Text]
legacyOf project = maybe [] mgLegacy (catalogMigrate (prCatalog project))

-- | The rules' findings in some project files, each fix's open choices
-- resolved ('chooseFixInputs'), and the allowances that applied.
findings :: Ports -> Project -> [FilePath] -> IO ([Finding], [Exception])
findings ports project = findingsWith (newnessOf ports project) ports project

-- | 'findings' with a file's newness given (the server caches it).
findingsWith :: (FilePath -> Text -> IO NewLines) -> Ports -> Project -> [FilePath] -> IO ([Finding], [Exception])
findingsWith newness ports project files = do
  let cat = prCatalog project
      ws = prWorkspace project
  paths <- listFiles ws "."
  now <- pNow ports
  hints <- newWorkHints ports project newness files
  (\(fs, exs) -> (fs ++ hints, exs)) <$> checkFiles (pSyntax ports) (catalogLanguages cat) (catalogRulesConfig cat) (legacyOf project) (\f -> readText ws f >>= newness f . fromMaybe "") (catalogRules cat) paths (readText ws) files
    >>= chooseFixInputs ports project paths now

-- | A fix's enum inputs left open are pre-chosen when the finding's code
-- names exactly one choice (`invoices::table` → invoice_repository), and the
-- label says which (beat 4 feedback).
chooseFixInputs :: Ports -> Project -> [FilePath] -> UTCTime -> ([Finding], a) -> IO ([Finding], a)
chooseFixInputs ports project paths now (fs, exs) = (,exs) <$> traverse (\f -> (\os -> f {fFixes = os}) <$> traverse (choose f) (fFixes f)) fs
  where
    cat = prCatalog project
    nameOf gid = maybe gid (generatorName . entryDefinition) (find ((== gid) . generatorIdText . entryId) (catalogEntries cat))
    choose f o = case find ((== foGenerator o) . generatorIdText . entryId) (catalogEntries cat) of
      Nothing -> pure o
      Just e ->
        withChoices (projectQueries ports project) paths (readText (prWorkspace project)) (M.fromList (foSets o)) now (entryDefinition e) <&> \case
          Left _ -> o
          Right g ->
            let open = [(inputName i, cs) | i <- genInputs g, inputName i `notElem` map fst (foSets o), ScalarInput (EnumOf _ _ (Resolved cs _)) <- [inputType i]]
                picks = [(k, c) | (k, cs) <- open, Just c <- [suggestChoice (fText f) cs]]
             in case picks of
                  (k, c) : _ -> o {foSets = sortOn fst ((k, c) : foSets o), foLabel = Just (fromMaybe (nameOf (foGenerator o)) (foLabel o) <> ": " <> toPascal c), foChoices = [oc | oc@(k', _) <- open, k' /= k]}
                  -- what the code does not name, the person picks (review I2)
                  [] -> o {foChoices = open}

-- | `invoice_repository` → `InvoiceRepository`.
toPascal :: Text -> Text
toPascal = T.concat . map (\w -> T.toUpper (T.take 1 w) <> T.drop 1 w) . T.splitOn "_"

-- | A generator with its `choices_from` enums resolved in the project.
resolveChoicesFor :: Ports -> Project -> M.Map Text Text -> GeneratorWith s -> IO (Either Text (GeneratorWith s))
resolveChoicesFor ports project sets gen
  | not (needsChoices gen) = pure (Right gen)
  | otherwise = do
      paths <- listFiles (prWorkspace project) "."
      now <- pNow ports
      first failureWhy <$> withChoices (projectQueries ports project) paths (readText (prWorkspace project)) sets now gen

-- | `show`'s view of each enum's choices (spec §4.1); the project is listed
-- only when a generator has a `one_of`.
choiceViewsFor :: Ports -> Project -> M.Map Text Text -> GeneratorWith s -> IO (Either Text [(Text, ChoiceView)])
choiceViewsFor ports project sets gen
  | not (needsChoices gen) = pure (Right [(inputName i, Flat cs) | i@InputSpec {inputType = ScalarInput (EnumS cs)} <- genInputs gen])
  | otherwise = do
      paths <- listFiles (prWorkspace project) "."
      now <- pNow ports
      first failureWhy <$> choiceViews (projectQueries ports project) paths (readText (prWorkspace project)) sets now gen

-- | Snippets (spec §5.3): each cursor edit becomes a replacement of the
-- cursor's line (as the project's workspace has it), indented to fit.
placeAt :: Ports -> Project -> (FilePath, Int) -> [Generator] -> IO (Either Text [Generator])
placeAt ports project (rel, line) gens = do
  let lang = languageFor (catalogLanguages (prCatalog project)) rel
      unit = maybe "    " langIndent lang
  readText (prWorkspace project) rel >>= \case
    Nothing -> pure (Left (T.pack rel <> " does not exist"))
    Just t
      | let n = length (T.splitOn "\n" t), line < 1 || line > n ->
          pure (Left ("--at " <> T.pack rel <> ":" <> T.pack (show line) <> ": line " <> T.pack (show line) <> " is outside " <> T.pack rel <> " (lines 1–" <> T.pack (show n) <> ")"))
      | otherwise -> do
          let ls = T.splitOn "\n" t
          sequence <$> traverse (place ls lang unit) gens
  where
    -- a snippet anticipated `after` a node takes that node's indentation
    place ls lang unit gen
      | not (isSnippet gen) = pure (Right gen)
      | Just after <- afterRule =<< apAfter =<< genAppliesAt gen, Just l <- lang =
          afterIndent (pSyntax ports) l after ls line <&> \case
            Left why -> Left ("cannot place `" <> generatorIdText (genId gen) <> "`: " <> why <> " (run `olifant doctor`)")
            Right ind -> Right (atCursorIndented (fromMaybe (cursorIndent unit ls line) ind) rel line ls gen)
      | otherwise = pure (Right (atCursor unit rel line ls gen))

-- | The snippet a cursor plan placed (spec §4.9): the lines that replaced
-- the cursor's line, holes as tab stops, model fills where there are any.
snippetAtCursor :: (FilePath, Int) -> Plan -> Maybe Snippet
snippetAtCursor (rel, line) plan = do
  text <- maybe (M.lookup rel (planWrites plan)) Just (M.lookup rel (planMarked plan))
  -- as many lines as the replacement of the cursor's line has
  k <- listToMaybe [max 1 (length (T.lines new)) | e <- planEdits plan, editPath e == rel, WillReplace l _ new <- [editAction e], l == line]
  let lines' = take k (drop (line - 1) (T.splitOn "\n" text))
      fills = [(T.takeWhile (/= '[') (holeName (phSite h)), f) | h <- planHoles plan, holeFile (phSite h) == rel, Just f <- [phFill h]]
  Just (snippetOf rel line (T.intercalate "\n" lines') (`lookup` fills))

-- | How to plan: apply or only plan, the cursor (project-relative), the
-- model, the files shown to it, and where progress goes.
data PlanOptions = PlanOptions
  { poMode :: Mode
  , poAt :: Maybe (FilePath, Int)
  , poFiller :: Maybe (ModelFiller IO)
  , poNoFill :: Bool
  , poContext :: [(FilePath, Text)]
  , poProgress :: ProgressEvent -> IO ()
  , poNow :: Maybe UTCTime
  , -- | stops the plan (between fills) and its checks, verifies and model
    -- calls, their processes killed (superseded work in the server)
    poCancel :: Cancel
  }

-- | Plan only, no cursor, no model, no progress.
planOptions :: PlanOptions
planOptions = PlanOptions PlanOnly Nothing Nothing False [] (const (pure ())) Nothing neverCancel

-- | Run plan steps (model, fills, regions): the plan, with the snippet at
-- the cursor and the catalog's load warnings.
planWith :: Ports -> Project -> PlanOptions -> [Step] -> IO (Either GenerationError Plan)
planWith ports project po steps = do
  now <- maybe (pNow ports) pure (poNow po)
  let cat = prCatalog project
      tools =
        (projectTools ports project)
          { toolQueries = maybe (projectQueries ports project) (uncurry (queriesAtWith (poCancel po) ports project)) (poAt po)
          , toolFiller = poFiller po
          , toolFillContext = poContext po
          , toolProgress = poProgress po
          , toolChecker = pChecker ports (poCancel po)
          , toolVerifier = pVerifier ports (poCancel po)
          , toolCancelled = cancelled (poCancel po)
          }
  generate (prWorkspace project) (resolver (pFiles ports) (pSyntax ports) cat) tools steps now (poMode po) <&> fmap finish
  where
    finish plan0 =
      (if poNoFill po then noFill plan0 else plan0)
        { planSnippet = (`snippetAtCursor` plan0) =<< poAt po
        , planWarnings =
            planWarnings plan0
              ++ ["regions are not refreshed from " <> T.pack path <> ": " <> err | (path, err) <- catalogBrokenRegions (prCatalog project)]
              ++ ["language adapter " <> T.pack path <> " failed to load: " <> err | (path, err) <- catalogBrokenLanguages (prCatalog project)]
        }

-- | `--no-fill`: holes were left on purpose.
noFill :: Plan -> Plan
noFill p = p {planHoles = [h {phStatus = HoleSkipped "--no-fill"} | h <- planHoles p]}

-- | The model a plan fills with: the one named, else the project's
-- default; none when neither is set.
modelFiller :: Catalog -> Maybe Text -> Either Text (Maybe (ModelFiller IO))
modelFiller = modelFillerWith neverCancel

-- | 'modelFiller', its requests dropped when the token trips.
modelFillerWith :: Cancel -> Catalog -> Maybe Text -> Either Text (Maybe (ModelFiller IO))
modelFillerWith token cat named = case named <|> mcDefault config of
  Nothing -> Right Nothing
  Just name -> case lookup name (mcBackends config) of
    Nothing -> Left ("--model " <> name <> ": no such backend under [model.backends] (backends: " <> (if null (mcBackends config) then "none" else T.intercalate ", " (map fst (mcBackends config))) <> ")")
    Just b -> Right (Just (ModelFiller (backendModel b) (mcRetries config) (curlFillerWith token (mcTimeout config) b)))
  where
    config = catalogModel cat

-- | Why a fix could not be planned.
data FixFailure
  = -- | no finding of that rule at that line
    FixNotFound Text
  | -- | no such fix
    FixUsage Text
  | FixCatalog CatalogError
  | -- | its replacement did not render
    FixFailed Text

-- | The steps of a rule's fix (spec §5.9) at a line: the replacement
-- first, while its lines are where the rule saw them, then the fix's
-- generator; and the files the fix shows the model.
fixSteps :: Ports -> Project -> Text -> FilePath -> Int -> Int -> [(Text, Text)] -> IO (Either FixFailure ([(FilePath, Text)], [Step], [Text]))
fixSteps ports project rule rel line n given = do
  let cat = prCatalog project
      ws = prWorkspace project
  (fs, _) <- findings ports project [rel]
  let here = [f | f <- fs, fRule f == rule, Just ((l1, _), (l2, _)) <- [fRange f], l1 <= line, line <= l2]
  case here of
    [] -> pure (Left (FixNotFound ("no `" <> rule <> "` finding at " <> T.pack rel <> ":" <> T.pack (show line))))
    f : _ -> case drop (n - 1) (fFixes f) of
      [] -> pure (Left (FixUsage ("`" <> rule <> "` offers " <> T.pack (show (length (fFixes f))) <> " fix(es); there is no fix " <> T.pack (show n))))
      o : _ -> case findEntry cat (foGenerator o) of
        Left err -> pure (Left (FixCatalog err))
        Right entry ->
          loadGenerator (pFiles ports) (pSyntax ports) cat entry >>= \case
            Left err -> pure (Left (FixCatalog err))
            Right gen -> do
              let sets = M.union (M.fromList given) (M.fromList (foSets o))
                  -- the found node's text relative to its own indentation (its
                  -- first line has none; the replacement adds it back to every line);
                  -- `finding_head`: a definition's whole signature, to its colon
                  found = maybe "" (\rp -> relativeTo (rpIndent rp) (rpFinding rp)) (foReplace o)
                  env = M.map Tm.VText (M.insert "finding_name" (definedName found) (M.insert "finding_switch_path" (switchPath found) (M.insert "finding_head" (signatureHead found) (M.insert "finding" found sets))))
                  defs = [d | r <- catalogRules cat, ruleId r == rule, d <- drop (n - 1) (ruleFixes r)]
                  replacement = case foReplace o of
                    Nothing -> Right []
                    Just rp -> do
                      rendered <- first describeTemplateError (render env (rpText rp))
                      let text = T.intercalate "\n" [if T.null l then l else rpIndent rp <> l | l <- T.lines rendered]
                      pure [Step (replaceGenerator rel (rpLines rp) (rpOld rp) (Template text [Lit text])) M.empty]
              context <- fmap catMaybes . traverse (\t -> either (const (pure Nothing)) (\p -> fmap (T.unpack p,) <$> readText ws (T.unpack p)) (render env t)) $ concatMap fdContext (take 1 defs)
              -- a definition rewritten in place keeps its callers: say who they are
              warnings <- case foReplace o of
                Just rp | isDefinition found -> callersNote (definedName found) <$> callersOf ws rel (rpLines rp) (definedName found)
                _ -> pure []
              pure $ case replacement of
                Left why -> Left (FixFailed why)
                Right first' -> Right (context, first' ++ [Step gen sets], warnings)
