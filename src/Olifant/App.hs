-- | The engine's operations, as both front ends use them: the CLI presents
-- them as JSON or text, the language server as LSP. A 'Project' is a
-- catalog plus the workspace it reads (disk, or open buffers over disk).
module Olifant.App
  ( Ports (..)
  , Project (..)
  , projectRoot
  , openProject
  , generatorsOf
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
  , FixFailure (..)
  , fixSteps
  ) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.List (find, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Olifant.Actions (Action, Completion, FileAction, Nearby, actionsAt, actionsInFile, completionsAt)
import Olifant.Anchor (astQueries)
import Olifant.Catalog
import Olifant.Cursor (afterIndent, afterRule, atCursor, atCursorIndented, cursorIndent, isSnippet)
import Olifant.Fill (ModelFiller (..))
import Olifant.Generation
import Olifant.Generator (AppliesAt (..), ChoiceView (..), Choices (..), Generator, GeneratorDef, GeneratorWith (..), InputSpec (..), InputType (..), Scalar (..), choiceViews, generatorIdText, generatorName, needsChoices, replaceGenerator, withChoices)
import Olifant.Holes (HoleSite (..))
import Olifant.Language (Checker, Language (..), Syntax, languageFor)
import Olifant.Model (ModelConfig (..), backendModel, curlFiller)
import Olifant.Query (Queries (..), QueryFailure (..), processCommand)
import Olifant.Rules (Exception, FixDef (..), FixOffer (..), Finding (..), Replacement (..), RuleDef (..), checkFiles, suggestChoice)
import Olifant.Snippet (Snippet, snippetOf)
import Olifant.Template (describeTemplateError, render)
import Olifant.Template qualified as Tm
import Olifant.Template.Syntax (Segment (..), Template (..))
import Olifant.Uncovered (uncoveredQuery)
import Olifant.Workspace (PathState (..), Workspace (..), within)
import System.FilePath (takeDirectory, (</>))

-- | What the engine needs from the world: the file system (absolute
-- paths), the syntax and compiler adapters, the clock, and where user
-- configuration lives.
data Ports = Ports
  { pFiles :: Workspace IO
  , pSyntax :: Syntax IO
  , pChecker :: Checker IO
  , pVerifier :: Verifier IO
  , pNow :: IO UTCTime
  , pUserConfigDir :: Maybe FilePath
  , pHomeDir :: Maybe FilePath
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
projectRoot ws home start = go start
  where
    go dir
      | Just dir == home = pure start
      | otherwise =
          inspectPath ws (dir </> ".olifant") >>= \case
            Directory -> pure dir
            _ | takeDirectory dir == dir -> pure start
              | otherwise -> go (takeDirectory dir)

-- | The project whose root is @root@, its catalog loaded, reading disk.
openProject :: Ports -> FilePath -> IO (Either CatalogError Project)
openProject ports root =
  openProjectCatalog (pFiles ports) (CatalogPaths root (pUserConfigDir ports) (pHomeDir ports))
    <&> fmap (\cat -> Project root cat (within root (pFiles ports)))

generatorsOf :: Project -> [(Text, GeneratorDef)]
generatorsOf project = [(generatorIdText (entryId e), entryDefinition e) | e <- catalogEntries (prCatalog project)]

-- | The tools a command in this project uses: the ports' syntax, checker
-- and verifier, the catalog's languages and regions, and the project's
-- queries.
projectTools :: Ports -> Project -> Tools IO
projectTools ports project =
  (basicTools (pSyntax ports) (pChecker ports) (catalogLanguages cat)) {toolRegions = catalogRegions cat, toolQueries = projectQueries ports project, toolVerifier = pVerifier ports}
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
queriesAt ports project file line =
  (projectQueries ports project) {queryUncovered = uncoveredQuery (pChecker ports) (catalogLanguages (prCatalog project)) (prWorkspace project) file line}

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

-- | The rules' findings in some project files, each fix's open choices
-- resolved ('chooseFixInputs'), and the allowances that applied.
findings :: Ports -> Project -> [FilePath] -> IO ([Finding], [Exception])
findings ports project files = do
  let cat = prCatalog project
      ws = prWorkspace project
  paths <- listFiles ws "."
  now <- pNow ports
  checkFiles (pSyntax ports) (catalogLanguages cat) (catalogRulesConfig cat) (catalogRules cat) paths (readText ws) files
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
  , -- | whether the plan was cancelled (superseded work in the server)
    poCancelled :: IO Bool
  }

-- | Plan only, no cursor, no model, no progress.
planOptions :: PlanOptions
planOptions = PlanOptions PlanOnly Nothing Nothing False [] (const (pure ())) Nothing (pure False)

-- | Run plan steps (model, fills, regions): the plan, with the snippet at
-- the cursor and the catalog's load warnings.
planWith :: Ports -> Project -> PlanOptions -> [Step] -> IO (Either GenerationError Plan)
planWith ports project po steps = do
  now <- maybe (pNow ports) pure (poNow po)
  let cat = prCatalog project
      tools =
        (projectTools ports project)
          { toolQueries = maybe (projectQueries ports project) (uncurry (queriesAt ports project)) (poAt po)
          , toolFiller = poFiller po
          , toolFillContext = poContext po
          , toolProgress = poProgress po
          , toolCancelled = poCancelled po
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
modelFiller cat named = case named <|> mcDefault config of
  Nothing -> Right Nothing
  Just name -> case lookup name (mcBackends config) of
    Nothing -> Left ("--model " <> name <> ": no such backend under [model.backends] (backends: " <> (if null (mcBackends config) then "none" else T.intercalate ", " (map fst (mcBackends config))) <> ")")
    Just b -> Right (Just (ModelFiller (backendModel b) (mcRetries config) (curlFiller (mcTimeout config) b)))
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
fixSteps :: Ports -> Project -> Text -> FilePath -> Int -> Int -> [(Text, Text)] -> IO (Either FixFailure ([(FilePath, Text)], [Step]))
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
                  env = M.map Tm.VText (M.insert "finding" (maybe "" rpFinding (foReplace o)) sets)
                  defs = [d | r <- catalogRules cat, ruleId r == rule, d <- drop (n - 1) (ruleFixes r)]
                  replacement = case foReplace o of
                    Nothing -> Right []
                    Just rp -> do
                      rendered <- first describeTemplateError (render env (rpText rp))
                      let text = T.intercalate "\n" [if T.null l then l else rpIndent rp <> l | l <- T.lines rendered]
                      pure [Step (replaceGenerator rel (rpLines rp) (rpOld rp) (Template text [Lit text])) M.empty]
              context <- fmap catMaybes . traverse (\t -> either (const (pure Nothing)) (\p -> fmap (T.unpack p,) <$> readText ws (T.unpack p)) (render env t)) $ concatMap fdContext (take 1 defs)
              pure $ case replacement of
                Left why -> Left (FixFailed why)
                Right first' -> Right (context, first' ++ [Step gen sets])
