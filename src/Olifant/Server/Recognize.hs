-- | Internal to Olifant.Server: recognitions as suggestions (convention
-- recognition). Code someone is writing by hand that a project convention
-- produces is offered as that convention: the file converted (when it is
-- short) or only its companions (the test, the wiring), never a file that
-- already is the convention.
module Olifant.Server.Recognize
  ( recognitionFor
  , recognitionPlan
  , suggestOrRecognize
  , recognitionFeedback
  , recognitionUpgrade
  , recognitionSuggestion
  , quietCounts
  , indexFor
  ) where

import Control.Monad (forM)
import Data.Aeson (Result (..), Value (..), decodeStrict, encode, fromJSON, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.Set qualified as S
import Data.Text.Encoding qualified as TE
import Data.Char (isSpace)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.App (PlanOptions (..), Ports (..), Project (..), planOptions, planWith)
import Olifant.Catalog (Catalog (..), CatalogEntry (..), RecognizeConfig (..), describeCatalogError, findEntry, loadGenerator)
import Olifant.Generation (Plan (..), ProgressEvent, Step (..), describeConflict, describeGenerationError)
import Olifant.Generator (EditWith (..), Generator, GeneratorId (..), GeneratorWith (..), InputSpec (..))
import Olifant.Language (Language (..), TestDef (..), defaultUnitNames, languageFor, regexName)
import Olifant.Naming (Case (Title), parseName, render)
import Olifant.Recognize
import Olifant.Server.Actions (fillerFor)
import Olifant.Server.State
import Olifant.Server.Suggest (Suggestion (..), suggestionFor)
import Olifant.Workspace (Workspace (..), hidePath, within)
import System.FilePath (makeRelative, takeFileName)

-- | What follows from an edit: a follow-up or a ghost first; a recognition
-- when there is none.
suggestOrRecognize :: FilePath -> Int -> ServerState -> IO (ServerState, Maybe Suggestion)
suggestOrRecognize path line s =
  suggestionFor path line s >>= \case
    (s', Just found) -> pure (s', Just found)
    (s', Nothing) -> recognitionFor path line s'

-- | The recognition of a file (as it is, unsaved), if any: an offer with
-- its plan, or a hint (unsure, or inputs it could not read).
recognitionFor :: FilePath -> Int -> ServerState -> IO (ServerState, Maybe Suggestion)
recognitionFor path line = memo "recognize" path (T.pack (show line)) $ \s ->
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', Nothing)
    (s', Just (root, project))
      | not (recEnabled (catalogRecognize (prCatalog project))) -> pure (s', Nothing)
      -- a file the project already had is what it is: recognition is for new ones
      | makeRelative root path `S.member` M.findWithDefault S.empty root (ssEstablished s') -> pure (s', Nothing)
      | otherwise -> case languageFor (catalogLanguages (prCatalog project)) rel of
          Nothing -> pure (s', Nothing)
          Just lang -> do
            (s'', ix, gens) <- indexFor root project lang s'
            text <- fromMaybe "" <$> readText (prWorkspace project) rel
            counts <- quietCounts root s''
            let shape = shingles (shapes (ixKeywords ix) text)
                -- dismissed: quiet until its shape changes (more than 30% new)
                stillDismissed = case M.lookup path (ssDismissed s'') of
                  Just old -> fromIntegral (S.size (shape `S.difference` old)) <= 0.3 * (fromIntegral (S.size shape) :: Double)
                  Nothing -> False
                -- earned quietness: three dismissals in a row make a generator a hint
                quieted r = if M.findWithDefault 0 (generatorIdText (rcGenerator r)) counts >= 3 then r {rcBand = Hint} else r
            if stillDismissed
              then pure (s'', Nothing)
              else case quieted <$> recognizeIn ix gens (Change rel (region text)) of
                Nothing -> pure (s'', Nothing)
                Just r -> (s'',) <$> recognitionSuggestion path line s'' root project gens r text
      where
        rel = makeRelative root path
  where
    region text =
      let ls = T.lines text
       in if length ls <= 200 then text else T.unlines (take 200 (drop (max 0 (line - 1 - 100)) ls))

-- | A recognition as a suggestion: an offer with its plan (a hint when it
-- is unsure, or an input it could not read is missing); nothing for a file
-- that already is the convention.
recognitionSuggestion :: FilePath -> Int -> ServerState -> FilePath -> Project -> [Generator] -> Recognition -> Text -> IO (Maybe Suggestion)
recognitionSuggestion path line s root project gens r text = suggestion
  where
    suggestion = do
        let rel = makeRelative root path
            gen = listToMaybe [g | g <- gens, genId g == rcGenerator r]
            missing = [inputName i | Just g <- [gen], i <- genInputs g, not (inputOptional i), inputDefault i == Nothing, inputName i `notElem` map fst (rcInputs r)]
            start = declarationLine text line
            label = "This looks like a " <> conventionName (rcGenerator r) <> maybe "" (\v -> " (" <> v <> ")") (mainInput gen (rcInputs r))
            base plan = Suggestion "recognition" label "" start [] root plan (generatorIdText (rcGenerator r)) (rcInputs r) start Nothing
        now <- pNow (ssPorts s)
        let empty = Plan (rcGenerator r) [] now [] [] [] M.empty [] M.empty M.empty Nothing
            hint = Just ((base empty) {sgRecognition = Just r {rcBand = Hint}})
        if rcBand r == Hint || not (null missing)
          then pure hint
          else
            snd <$> recognitionPlan False (const (pure ())) path (generatorIdText (rcGenerator r)) (rcInputs r) s >>= \case
              Left _ -> pure Nothing
              Right (_, plan) -> do
                onDisk <- fileExists (within root (pFiles (ssPorts s))) rel
                let elsewhere = [p | p <- M.keys (planWrites plan), p /= rel]
                pure $
                  if null elsewhere && onDisk
                    then Nothing
                    else Just ((base plan) {sgSummary = summary rel plan, sgRecognition = Just r})
    summary rel plan =
      let others = [T.pack (takeFileName p) | p <- M.keys (planWrites plan), p /= rel]
          converts = M.member rel (planWrites plan)
          -- off its place, the convention's file is written where it belongs:
          -- the user's stays, and they are told so (it is theirs to remove)
          kept = not converts && rcPlace r < 0.25
       in T.intercalate "; " (["converts " <> T.pack (takeFileName rel) | converts] <> ["adds " <> T.intercalate ", " others | not (null others)] <> ["your " <> T.pack (takeFileName rel) <> " here stays: remove it once its logic has moved" | kept])
    -- the first line declaring something (where the construct starts): a
    -- line the language's `[unit] name` names, or one its `[test] above`
    -- says belongs to the unit below (a decorator)
    declarationLine src fallback = case languageFor (catalogLanguages (prCatalog project)) (makeRelative root path) of
      Just lang | langUnitNames lang /= defaultUnitNames ->
        let named l = any (\re -> isJust (regexName re l)) (langUnitNames lang ++ maybe [] (maybe [] pure . testAbove) (langTest lang))
         in fromMaybe fallback (listToMaybe [n | (n, l) <- zip [1 ..] (T.lines src), named l])
      _ -> fallback
    -- the convention's own name: the last ident input it read (a command's
    -- name, not its service)
    mainInput gen inputs =
      listToMaybe
        [ render Title n
        | Just g <- [gen]
        , i <- reverse (genInputs g)
        , Just v <- [lookup (inputName i) inputs]
        , Right n <- [parseName v]
        ]


-- | A pushed recognition sharpened by the project's decider, when it is
-- unsure and [decide] model (or its alias [recognize] model) is set (a
-- recorded answer in tests), asked under this work's token: the better
-- suggestion, or nothing.
recognitionUpgrade :: FilePath -> Suggestion -> ServerState -> IO (ServerState, Maybe Suggestion)
recognitionUpgrade path sg s = case sgRecognition sg of
  Just r | unsure r ->
    projectFor path s >>= \case
      (s', Just (root, project)) | Just d <- deciderUnder (ssCancel s') s' project, Just lang <- languageFor (catalogLanguages (prCatalog project)) (makeRelative root path) -> do
        (s'', _, gens) <- indexFor root project lang s'
        let rel = makeRelative root path
        text <- fromMaybe "" <$> readText (prWorkspace project) rel
        counts <- quietCounts root s''
        -- earned quietness holds: the model never makes a quieted generator loud
        let quiet g = M.findWithDefault 0 (generatorIdText g) counts >= 3
        if quiet (rcGenerator r) && isNothing (rcOr r) && isNothing (rcRival r)
          then pure (s'', Nothing)
          else
            consult d gens (Change rel text) r >>= \case
              Just r' | rcBand r' == Offer, not (quiet (rcGenerator r')) -> (s'',) <$> recognitionSuggestion path (sgAt sg) s'' root project gens r' text
              _ -> pure (s'', Nothing)
      (s', _) -> pure (s', Nothing)
  _ -> pure (s, Nothing)

-- | A convention as people say it: its generator's id, without its pack
-- (rust-htmx/service-command → "service command").
conventionName :: GeneratorId -> Text
conventionName (GeneratorId i) = T.replace "-" " " (T.replace "_" " " (snd (T.breakOnEnd "/" i)))

-- | A generator planned for a file someone is writing: the file treated as
-- absent, so the convention writes it; then, when the file is short (at most
-- 10 non-blank lines) the plan converts it (against its current text), else
-- the plan leaves it alone and keeps only the companions.
recognitionPlan :: Bool -> (ProgressEvent -> IO ()) -> FilePath -> Text -> [(Text, Text)] -> ServerState -> IO (ServerState, Either Text (FilePath, Plan))
recognitionPlan fill progress path generator sets s =
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', Left (T.pack path <> " is in no Olifant project"))
    (s', Just (root, project)) -> fmap (s',) $ do
      let ports = ssPorts s'
          rel = makeRelative root path
      text <- fromMaybe "" <$> readText (prWorkspace project) rel
      case findEntry (prCatalog project) generator of
        Left err -> pure (Left (describeCatalogError err))
        Right entry ->
          loadGenerator (pFiles ports) (pSyntax ports) (prCatalog project) entry >>= \case
            Left err -> pure (Left (describeCatalogError err))
            Right gen ->
              fillerFor fill s' project >>= \case
                Left why -> pure (Left why)
                Right filler ->
                  planWith ports project {prWorkspace = hidePath rel (prWorkspace project)} planOptions {poFiller = filler, poProgress = progress, poCancel = ssCancel s'} [Step gen (M.fromList sets)] >>= \case
                    Left err -> pure (Left (describeGenerationError err))
                    Right plan
                      | not (null (planConflicts plan)) -> pure (Left (T.intercalate "; " (map describeConflict (planConflicts plan))))
                      | length (filter (not . T.null . T.strip) (T.lines text)) <= 10 -> pure (Right (root, plan {planBefore = M.insert rel (Just text) (planBefore plan)}))
                      | otherwise -> pure (Right (root, plan {planWrites = M.delete rel (planWrites plan), planBefore = M.delete rel (planBefore plan), planMarked = M.delete rel (planMarked plan)}))

-- | What the editor did with a recognition: dismissed (one more in a row
-- for its generator; the file quiet until its shape changes), accepted or
-- reset (the generator speaks again). Kept in the project's
-- .olifant/state/recognition.json.
recognitionFeedback :: FilePath -> Text -> Text -> ServerState -> IO ServerState
recognitionFeedback path generator outcome s =
  projectFor path s >>= \case
    (s', Nothing) -> pure s'
    (s', Just (root, project)) -> do
      counts <- quietCounts root s'
      let n = M.findWithDefault 0 generator counts
          counts' = M.insert generator (if outcome == "dismissed" then n + 1 else 0) counts
          rel = makeRelative root path
      shape <- case (outcome, languageFor (catalogLanguages (prCatalog project)) rel) of
        ("dismissed", Just lang) -> do
          (_, ix, _) <- indexFor root project lang s'
          text <- fromMaybe "" <$> readText (prWorkspace project) rel
          pure (Just (shingles (shapes (ixKeywords ix) text)))
        _ -> pure Nothing
      writeText (within root (pFiles (ssPorts s'))) stateFile (TE.decodeUtf8 (BL.toStrict (encode (object ["dismissed" .= counts']))))
      pure
        s'
          { ssQuiet = M.insert root counts' (ssQuiet s')
          , ssDismissed = maybe (M.delete path) (M.insert path) shape (ssDismissed s')
          , ssGeneration = ssGeneration s' + 1
          }

stateFile :: FilePath
stateFile = ".olifant/state/recognition.json"

-- | A project's dismissals in a row, per generator: known, or read from its
-- state file (none when it has none).
quietCounts :: FilePath -> ServerState -> IO (M.Map Text Int)
quietCounts root s = case M.lookup root (ssQuiet s) of
  Just counts -> pure counts
  Nothing -> do
    src <- readText (within root (pFiles (ssPorts s))) stateFile
    pure $ case src >>= decodeStrict . TE.encodeUtf8 of
      Just (Object o) | Just v <- KM.lookup "dismissed" o, Success counts <- fromJSON v -> counts
      _ -> M.empty

-- | The project's recognition index for a language, for the current file
-- stamp: its generators of that language (loaded) and its files of it.
indexFor :: FilePath -> Project -> Language -> ServerState -> IO (ServerState, Index, [Generator])
indexFor root project lang s =
  case M.lookup (root, langName lang) (ssRecognition s) of
    Just (stamp, ix, gens) | stamp == ssFilesStamp s -> pure (s, ix, gens)
    _ -> do
      let cat = prCatalog project
          ws = prWorkspace project
      gens <- fmap concat . forM [e | e <- catalogEntries cat, genLanguage (entryDefinition e) == langName lang] $ \e ->
        either (const []) pure <$> loadGenerator (pFiles (ssPorts s)) (pSyntax (ssPorts s)) cat e
      paths <- filter (\p -> fmap langName (languageFor (catalogLanguages cat) p) == Just (langName lang)) <$> listFiles ws "."
      files <- fmap concat . forM paths $ \p -> maybe [] (\t -> [(p, t)]) <$> readText ws p
      let kw = keywordsFrom (langComment lang) [t | g <- gens, Create {editSource = t} <- genEdits g] (langKeywords lang)
          ix = buildIndex kw gens files
      pure (s {ssRecognition = M.insert (root, langName lang) (ssFilesStamp s, ix, gens) (ssRecognition s)}, ix, gens)
