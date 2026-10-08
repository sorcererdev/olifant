-- | Internal to Olifant.Server: the generate picker's preview. A picker's
-- steps (a generator and the values set for it; a `+ New` step first, the
-- picked generator last) as a form — each input's value and where it came
-- from — and the plan they would make, missing names shown as ⟨name⟩.
module Olifant.Server.Preview (PickStep (..), Preview (..), previewFor, stepsPlanWith) where

import Control.Monad (foldM)
import Data.List (nub)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Actions (Action (..))
import Olifant.App (PlanOptions (..), Ports (..), Project (..), placeAt, planOptions, planWith, showGenerator)
import Olifant.Catalog (CatalogEntry (..), describeCatalogError, findEntry, loadGenerator, packOfEntry, resolveEntry)
import Olifant.Generation (GenerationError, Plan (..), ProgressEvent, Step (..), describeConflict, describeGenerationError)
import Olifant.Generator (ChoiceView (..), EditWith (..), Generator, GeneratorDef, GeneratorWith (..), InputSpec (..), InputType (..), Scalar (..), buildEnv, describeEnvError, generatorIdText)
import Olifant.Holes (Marker (..), splitMarkers)
import Olifant.Naming qualified as N
import Olifant.Preview (Field (..), Source (..), fieldsFor, markValue, unmark)
import Olifant.Recognize (invertPath, pathPattern)
import Olifant.Server.Actions (actionsFor, fillerFor)
import Olifant.Server.State (deciderFor, ServerState (..), projectFor)
import Olifant.Decide (Answer (..), Decider (..), Question (..))
import Olifant.Migration (functionText, tablesTouched)
import Olifant.Rules (fingerprint)
import Olifant.Workspace (Workspace (..))
import Control.Concurrent (forkIO)
import Control.Monad (forM, void, when)
import Data.IORef (atomicModifyIORef', modifyIORef', readIORef)
import Data.Set qualified as S
import Olifant.Template (render)
import System.FilePath (makeRelative, splitDirectories, takeBaseName, takeDirectory, takeExtension)

-- | One step of a picker's plan: a generator, the values the user set, and
-- (a `+ New` step) which later step's input its value fills.
data PickStep = PickStep {psGenerator :: Text, psSets :: [(Text, Text)], psMakes :: Maybe (Int, Text)}

-- | The form and what it would write: per step its fields; the plan (marks
-- shown as ⟨name⟩); ready when every field of every step is valid; or why
-- it cannot plan.
data Preview = Preview {pvFields :: [[Field]], pvPlan :: Maybe Plan, pvReady :: Bool, pvError :: Maybe Text}

-- | A step worked out: its generator (loaded), its fields, and the values
-- to plan it with (marks for missing names, in a preview).
data Worked = Worked {wGenerator :: Generator, wFields :: [Field], wValues :: [(Text, Text)]}

-- | The picker's preview of these steps for a document at a position
-- (0-based line and character).
previewFor :: FilePath -> (Int, Int) -> [PickStep] -> ServerState -> IO (ServerState, Preview)
previewFor path (line, col) steps s =
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', Preview [] Nothing False (Just (T.pack path <> " is in no Olifant project")))
    (s', Just (root, project)) -> do
      (s'', acts) <- actionsFor path (line + 1, col + 1) s'
      let rel = makeRelative root path
      workSteps s'' project rel acts True steps >>= \case
        Left why -> pure (s'', Preview [] Nothing False (Just why))
        Right ws -> do
          let fields = map wFields ws
              missing = nub [fName f | w <- ws, f <- wFields w, isNothing (fValue f), isName (wGenerator w) (fName f)]
              complete = not (null ws) && and [fValid f || optional (wGenerator w) (fName f) | w <- ws, f <- wFields w]
          planned <-
            placeAll (ssPorts s'') project rel line ws >>= \case
              Left why -> pure (Left why)
              Right gens -> checked <$> planWith (ssPorts s'') project planOptions {poAt = Just (rel, line + 1), poCancel = ssCancel s''} (zipWith (\g w -> Step g (M.fromList (wValues w))) gens ws)
          -- choices not made yet were planned with a stand-in (their first
          -- option): a failure is theirs to resolve, not a conflict to show
          let standIns = nub [fName f | w <- ws, f <- wFields w, isNothing (fValue f), not (isName (wGenerator w) (fName f)), isJust (fChoices f)]
          pure $ case planned of
            Left _ | not (null standIns) -> (s'', Preview fields Nothing False (Just ("choose " <> T.intercalate ", " standIns <> " to see what it writes")))
            Left why -> (s'', Preview fields Nothing False (Just (unmark missing why)))
            Right plan -> (s'', Preview fields (Just (unmarked missing plan)) complete Nothing)
  where
    isName g n = case [inputType i | i <- genInputs g, inputName i == n] of
      ScalarInput IdentS : _ -> True
      _ -> False
    optional g n = maybe False inputOptional (listToMaybe [i | i <- genInputs g, inputName i == n])
    -- holes as ⟨name⟩ (what you or the model will write): the text with
    -- its hole markers, where the plan kept one
    shown plan p t = maybe t (T.concat . map (either (\m -> "⟨" <> markerName m <> "⟩") id) . splitMarkers) (M.lookup p (planMarked plan))
    unmarked names plan =
      plan
        { planWrites = M.fromList [(back names p, unmark names (shown plan p t)) | (p, t) <- M.toList (planWrites plan)]
        , planBefore = M.mapKeys (back names) (planBefore plan)
        }
    back names = T.unpack . unmark names . T.pack

-- | A plan's outcome as the picker reads it: the plan, or why not (its
-- conflicts are reasons too: a file that already exists).
checked :: Either GenerationError Plan -> Either Text Plan
checked = \case
  Left err -> Left (describeGenerationError err)
  Right plan
    | null (planConflicts plan) -> Right plan
    | otherwise -> Left (T.intercalate "; " (map describeConflict (planConflicts plan)))

-- | The steps planned for real (the picker's apply) at a position (0-based
-- line and character: where a snippet goes, as the preview placed it):
-- the values set and the values inferred, no marks.
stepsPlanWith :: (ProgressEvent -> IO ()) -> Bool -> FilePath -> (Int, Int) -> [PickStep] -> ServerState -> IO (ServerState, Either Text (FilePath, Plan))
stepsPlanWith progress fill path (line, _) steps s =
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', Left (T.pack path <> " is in no Olifant project"))
    (s', Just (root, project)) -> do
      let rel = makeRelative root path
      fmap (s',) $
        workSteps s' project rel [] False steps >>= \case
          Left why -> pure (Left why)
          Right ws ->
            placeAll (ssPorts s') project rel line ws >>= \case
              Left why -> pure (Left why)
              Right gens ->
                fillerFor fill s' project >>= \case
                  Left why -> pure (Left why)
                  Right filler -> fmap (root,) . checked <$> planWith (ssPorts s') project planOptions {poAt = Just (rel, line + 1), poFiller = filler, poProgress = progress, poCancel = ssCancel s'} (zipWith (\g w -> Step g (M.fromList (wValues w))) gens ws)

-- | Each step's generator, placed at the cursor where it applies there (a
-- snippet), as the ⚡ menu places it.
placeAll :: Ports -> Project -> FilePath -> Int -> [Worked] -> IO (Either Text [Generator])
placeAll ports project rel line ws = fmap concat . sequence <$> traverse (\w -> placeAt ports project (rel, line + 1) [wGenerator w]) ws

-- | Work out each step in order. A `+ New` step's value lands in the input
-- it makes (source "new"). With @marks@, missing inputs are planned with
-- placeholders (a preview); without, they stay missing (a real plan
-- refuses them).
workSteps :: ServerState -> Project -> FilePath -> [(Action, Bool)] -> Bool -> [PickStep] -> IO (Either Text [Worked])
workSteps s project rel acts marks steps = fmap (reverse . snd) <$> foldM one (Right (M.empty, [])) (zip [0 :: Int ..] steps)
  where
    ports = ssPorts s
    one (Left why) _ = pure (Left why)
    one (Right (made, done)) (k, st) =
      case findEntry (prCatalog project) (psGenerator st) of
        Left err -> pure (Left (describeCatalogError err))
        Right entry ->
          loadGenerator (pFiles ports) (pSyntax ports) (prCatalog project) entry >>= \case
            Left err -> pure (Left (describeCatalogError err))
            Right g -> do
              let newValues = M.findWithDefault [] k made
                  user = [(n, v) | (n, v) <- psSets st, n `notElem` map fst newValues]
                  fromPath = folderValues (entryDefinition entry) rel
                  fromCursor = [(n, v) | (a, _) <- acts, actGenerator a == generatorIdText (genId g), (n, v) <- actSets a]
              (d, fields0) <- settle entry user newValues fromPath fromCursor
              fields <- if isExtract (psGenerator st) then suggestService fields0 else pure fields0
              -- the plan takes the values you set as you set them (the form may
              -- show one cleared, the plan's own checks decide), then the rest
              let known = map inputName (genInputs g) ++ map fst (genDerived g)
                  given = [(k, v) | (k, v) <- newValues ++ user, k `elem` known]
                  values = given ++ [(fName f, v) | f <- fields, fName f `notElem` map fst given, Just v <- [fValue f]] ++ (if marks then placeholders g [f | f <- fields, fName f `notElem` map fst given] else [])
                  w = Worked g fields values
              case psMakes st >>= \target -> (target,) <$> newTemplate target (psGenerator st) of
                Nothing -> pure (Right (made, w : done))
                Just ((into, input), t) -> do
                  now <- pNow ports
                  -- the definition with its choices resolved (the loaded one has none yet)
                  pure $ case buildEnv d (M.fromList values) now of
                    Left err -> Left (describeEnvError err)
                    Right env -> case render env t of
                      Left _ -> Right (made, w : done)
                      Right v -> Right (M.insertWith (++) into [(input, v)] made, w : done)
    isExtract g = any (`T.isSuffixOf` g) ["/extract-command", "/extract-query"]
    -- decision 3: the service whose models the legacy function's SQL names
    -- (the tables it touches); unsure, a decider's answer (asked once, in
    -- the background); a suggestion, always yours to change
    suggestService fields = case (lookupField "service", lookupField "function", lookupField "file") of
      (Just svc, Just (Field {fValue = Just fn}), Just (Field {fValue = Just file}))
        | fValue svc == Nothing || fSource svc `notElem` [Just FromYou, Just FromPath, Just FromNew] -> do
            content <- fromMaybe "" <$> readText (prWorkspace project) (T.unpack file)
            let text = fromMaybe "" (functionText content fn)
                tables = tablesTouched text
                services = fromMaybe [] (fChoices svc)
            owned <- forM services $ \service -> do
              models <- listFiles (prWorkspace project) ("src/" <> T.unpack (maybe "" id (fValue =<< lookupField "app")) <> "/" <> T.unpack service <> "/models")
              let stems = [T.pack (takeBaseName m) | m <- models, takeExtension m == ".py", takeBaseName m /= "__init__"]
              pure (service, any (\t -> any (\m -> singular t == singular m) stems) tables)
            pick <- case [svc' | (svc', True) <- owned] of
              [one] -> pure (Just one)
              _ -> case deciderFor s project of
                Nothing -> pure Nothing
                Just decider -> do
                  let key = (rel, fingerprint ["service", text])
                  known <- M.lookup key <$> readIORef (ssDecisions s)
                  case known of
                    Just (Chosen c _) | c `elem` services -> pure (Just c)
                    Just _ -> pure Nothing
                    Nothing -> do
                      fresh <- atomicModifyIORef' (ssAsking s) (\a -> (S.insert key a, S.notMember key a))
                      when fresh . void . forkIO $ do
                        answer <- ask decider (Question "service" (services ++ ["new service"]) [] [("function", text), ("services", T.unwords services)])
                        modifyIORef' (ssDecisions s) (M.insert key answer)
                      pure Nothing
            pure $ case pick of
              Just one | length services > 1 || fValue svc == Nothing -> [if fName f == "service" then f {fValue = Just one, fSource = Just FromSuggested, fValid = True, fWhy = Nothing} else f | f <- fields]
              _ -> fields
      _ -> pure fields
      where
        lookupField n = listToMaybe [f | f <- fields, fName f == n]
        singular t = T.toLower (fromMaybe t (T.stripSuffix "s" t))
    -- the template the served step's input gives for this `+ New` generator
    newTemplate (into, input) generator = do
      st <- listToMaybe (drop into steps)
      entry <- either (const Nothing) Just (findEntry (prCatalog project) (psGenerator st))
      spec <- listToMaybe [i | i <- genInputs (entryDefinition entry), inputName i == input]
      listToMaybe [t | (n, t) <- inputNew spec, n == generator || ("/" <> n) `T.isSuffixOf` generator]
    -- resolve the choices with what is known, until nothing more is known
    settle entry user newValues fromPath fromCursor = go (3 :: Int) []
      where
        go n extra = do
          let known = M.fromList (extra ++ fromCursor ++ fromPath ++ newValues ++ user)
          shown <- showGenerator ports project (generatorIdText (entryId entry)) known
          let (d, views) = either (const (entryDefinition entry, [])) (\(e, vs) -> (entryDefinition e, vs)) shown
              -- a choice that waits on inputs says which
              needs f = case lookup (fName f) views of
                Just (DependsOn ns) -> f {fNeeds = ns}
                _ -> f
              -- `new` as full ids (a bare name could be another pack's, or a local one)
              ids f = f {fNew = [either (const n) (generatorIdText . entryId) (resolveEntry (prCatalog project) (packOfEntry entry) n) | n <- fNew f]}
              fields = map (ids . needs . fromNew newValues) (fieldsFor d (newValues ++ user) fromPath fromCursor)
              more = [(fName f, v) | f <- fields, Just v <- [fValue f], fSource f `elem` [Just OnlyChoice, Just FromDefault], fName f `M.notMember` known]
          if n <= 0 || null more then pure (d, fields) else go (n - 1) (extra ++ more)
    -- a value a `+ New` step makes is not among the choices yet: the plan's
    -- earlier step makes it
    fromNew newValues f = case lookup (fName f) newValues of
      Just v -> f {fValue = Just v, fSource = Just FromNew, fValid = True, fWhy = Nothing}
      Nothing -> f
    -- a missing input planned as: a mark for a name, ⟨name⟩ for text,
    -- false, an enum's first choice
    placeholders g fields =
      [ (fName f, v)
      | f <- fields
      , isNothing (fValue f)
      , i <- take 1 [i | i <- genInputs g, inputName i == fName f]
      , Just v <- [placeholder f (inputType i)]
      ]
    placeholder f = \case
      ScalarInput IdentS -> Just (markValue (fName f))
      ScalarInput TextS -> Just ("⟨" <> fName f <> "⟩")
      ScalarInput BoolS -> Just "false"
      _ -> listToMaybe (fromMaybe [] (fChoices f))

-- | The inputs a file's folders give back through the generator's create
-- paths: only those a folder names (never the file's own name, which names
-- the thing already there).
folderValues :: GeneratorDef -> FilePath -> [(Text, Text)]
folderValues d rel =
  nub
    [ (k, v)
    | Create {editTarget = t} <- genEdits d
    , Just pat <- [pathPattern (genDerived d) t]
    , Just found <- [invertPath pat rel]
    , (k, v) <- found
    , v `elem` folders
    ]
  where
    folders = [either (const (T.pack f)) (N.render N.Snake) (N.parseName (T.pack f)) | f <- splitDirectories (takeDirectory rel)]
