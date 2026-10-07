-- | Internal to Olifant.Server: recognitions as suggestions (convention
-- recognition). Code someone is writing by hand that a project convention
-- produces is offered as that convention: the file converted (when it is
-- short) or only its companions (the test, the wiring), never a file that
-- already is the convention.
module Olifant.Server.Recognize
  ( recognitionFor
  , recognitionPlan
  , suggestOrRecognize
  ) where

import Control.Monad (forM)
import Data.Char (isSpace)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.App (PlanOptions (..), Ports (..), Project (..), planOptions, planWith)
import Olifant.Catalog (Catalog (..), CatalogEntry (..), RecognizeConfig (..), describeCatalogError, findEntry, loadGenerator)
import Olifant.Generation (Plan (..), ProgressEvent, Step (..), describeConflict, describeGenerationError)
import Olifant.Generator (EditWith (..), Generator, GeneratorId (..), GeneratorWith (..), InputSpec (..))
import Olifant.Language (Language (..), languageFor)
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
      | otherwise -> case languageFor (catalogLanguages (prCatalog project)) rel of
          Nothing -> pure (s', Nothing)
          Just lang -> do
            (s'', ix, gens) <- indexFor root project lang s'
            text <- fromMaybe "" <$> readText (prWorkspace project) rel
            case recognizeIn ix gens (Change rel (region text)) of
              Nothing -> pure (s'', Nothing)
              Just r -> (s'',) <$> suggestion s'' root project gens r text
      where
        rel = makeRelative root path
  where
    region text =
      let ls = T.lines text
       in if length ls <= 200 then text else T.unlines (take 200 (drop (max 0 (line - 1 - 100)) ls))
    suggestion s root project gens r text = do
      let rel = makeRelative root path
          gen = listToMaybe [g | g <- gens, genId g == rcGenerator r]
          missing = [inputName i | Just g <- [gen], i <- genInputs g, not (inputOptional i), inputDefault i == Nothing, inputName i `notElem` map fst (rcInputs r)]
          start = declarationLine text line
          label = "This looks like a " <> T.toLower (rcName r) <> maybe "" (\v -> " (" <> v <> ")") (mainInput gen (rcInputs r))
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
       in T.intercalate "; " (["converts " <> T.pack (takeFileName rel) | converts] <> ["adds " <> T.intercalate ", " others | not (null others)])
    -- the first line declaring something (where the construct starts)
    declarationLine text fallback =
      fromMaybe fallback (listToMaybe [n | (n, l) <- zip [1 ..] (T.lines text), any (`T.isPrefixOf` T.dropWhile isSpace l) ["class ", "def ", "struct ", "pub struct ", "fn ", "pub fn ", "trait ", "pub trait ", "enum ", "pub enum ", "@"]])
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
