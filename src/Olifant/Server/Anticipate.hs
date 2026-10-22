-- | Internal to Olifant.Server: anticipation (Jev in olifant). At a tie
-- (two actions apply at the line) or a silence (none does, and recognition
-- is unsure), one question from the prefix at the cursor: which generator
-- is being typed. A sure enough answer becomes the suggestion the editor
-- already draws; anything else, nothing.
module Olifant.Server.Anticipate
  ( Moment (..)
  , Anticipation (..)
  , tieGate
  , silenceGate
  , prefixAt
  , anticipateQuestion
  , anticipateCandidates
  , anticipationAsk
  , sureAnswer
  , anticipationFrom
  , anticipateAt
  ) where

import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Actions (Action (..), FileAction (..), Nearby (..), Range (..))
import Olifant.Catalog (Catalog (..))
import Olifant.Cursor (isSnippet)
import Olifant.Decide (Answer (..), Confidence (..), Decider (..), Question, choice, confident)
import Olifant.Generator (AppliesAt (..), Generator, GeneratorWith (..), InputSpec (..), generatorIdText, generatorName)
import Olifant.App (Project (..))
import Olifant.Language (languageFor)
import Olifant.Recognize
import Olifant.Rules (fingerprint)
import Olifant.Server.Hints (fileActionsFor)
import Olifant.Server.Recognize (indexFor, recognitionFor, recognitionSuggestion)
import Olifant.Server.State (ServerState, projectFor)
import Olifant.Server.Suggest (Suggestion (..), suggestionOf)
import Olifant.Workspace (Workspace (..))
import System.FilePath (makeRelative, takeDirectory)
import Data.Set qualified as S
import Data.Map.Strict qualified as M

-- | Two or more actions apply at the line, or none does.
data Moment = Tie | Silence deriving (Eq, Show)

-- | A sure answer, as the suggestion it makes.
data Anticipation = Anticipation {anGenerator :: Text, anConfidence :: Double, anMoment :: Moment, anSuggestion :: Suggestion}

-- | How sure an answer must be: choosing among what applies, or offering
-- what nothing suggested. Anticipate early and often: an offer can be
-- ignored. Lowered from 0.9 (tie) and 0.95 (silence) on 2026-10-20 after
-- the demo: Jev's 0.5-0.9 band is "confirm", and an offer is a confirm
-- surface. Task 5's corpus run reports precision at 0.6, 0.8, 0.9 and
-- 0.95, so the value can be revisited.
tieGate, silenceGate :: Double
tieGate = 0.6
silenceGate = 0.6

-- | The prefix at a line (1-based): the unit so far (from the nearest
-- non-blank line at column 0, at most 120 lines up to the cursor's), the
-- cursor's line and the file's import lines.
prefixAt :: Text -> Int -> (Text, Text, [Text])
prefixAt text line = (T.intercalate "\n" (drop (start - 1) upTo), cursor, imports)
  where
    ls = T.lines text
    upTo = take line ls
    cursor = fromMaybe "" (listToMaybe (drop (line - 1) ls))
    top l = maybe False (not . isSpace . fst) (T.uncons l)
    start = max (line - 119) (fromMaybe 1 (listToMaybe [n | (n, l) <- reverse (zip [1 ..] upTo), top l]))
    imports = [l | l <- ls, any (`T.isPrefixOf` l) ["import ", "from "]]

-- | The `anticipate` choice: the candidates (generator id, what it makes)
-- (the backend adds unsure), from the path, the cursor's line, the unit
-- and the imports; the instructions say what each is for.
anticipateQuestion :: Moment -> FilePath -> (Text, Text, [Text]) -> [(Text, Text)] -> Question
anticipateQuestion moment path (unit, cursor, imports) candidates =
  choice "anticipate" instructions candidates $
    [("path", T.pack path), ("cursor_line", cursor), ("unit", unit), ("imports", T.intercalate "\n" imports)]
  where
    instructions = case moment of
      Tie -> "Several generators apply at cursor_line in the file at path. Which one is the person about to write next, from the code in unit so far? Use the names in unit and the shape of the code around cursor_line."
      Silence -> "The person is writing the code in unit by hand, at cursor_line, in the file at path. Which generator produces what they are writing? Use the directory in path and the names in unit."

-- | What may be asked at a line: a tie (the actions covering it), or a
-- silence (none does, and recognition offers nothing sure: the suggested
-- generators that nearly apply, and those the file is at the place of).
anticipateCandidates :: FilePath -> Int -> ServerState -> IO (ServerState, Maybe (Moment, [(Text, Text)]))
anticipateCandidates path line s =
  fileActionsFor path s >>= \case
    (s', Nothing) -> pure (s', Nothing)
    (s', Just (gens, acts, near)) -> do
      let described gid = (gid, maybe gid (\g -> fromMaybe (generatorName g) (genSummary g)) (lookup gid gens))
          covering = nub (map (actGenerator . faAction) (coveringAt line acts))
          suggested gid = maybe False (maybe False apSuggest . genAppliesAt) (lookup gid gens)
      case covering of
        _ : _ : _ -> pure (s', Just (Tie, map described covering))
        _ : _ -> pure (s', Nothing)
        [] ->
          recognitionFor path line s' >>= \case
            (s'', Just sg) | fmap rcBand (sgRecognition sg) /= Just Hint -> pure (s'', Nothing)
            (s'', _) -> do
              (s''', placed) <- placedAt s''
              let cands = nub ([nbGenerator n | n <- near, suggested (nbGenerator n)] <> map generatorIdText placed)
              pure (s''', if null cands then Nothing else Just (Silence, map described cands))
  where
    placedAt st =
      indexAt path st <&> \case
        (st', Just (root, _, ix, _, _)) -> (st', placedGenerators ix (makeRelative root path))
        (st', Nothing) -> (st', [])

-- | An answer as an anticipation: an option chosen at least as surely as
-- the moment's gate. At a tie, the chosen action's suggestion (a snippet's
-- as one); at a silence, the chosen convention recognised, planned when
-- the file gives every input it needs, else a hint (Review Focus 3).
anticipationFrom :: FilePath -> Int -> Moment -> Answer -> ServerState -> IO (ServerState, Maybe Anticipation)
anticipationFrom path line moment answer s = case sureAnswer moment answer of
  Just (g, c) -> fmap (fmap (Anticipation g c moment)) <$> suggestion g c
  Nothing -> pure (s, Nothing)
  where
    suggestion g c = case moment of
      Tie ->
        fileActionsFor path s >>= \case
          (s', Just (gens, acts, _)) | fa : _ <- [fa | fa <- coveringAt line acts, actGenerator (faAction fa) == g] -> do
            let snippet = maybe False isSnippet (lookup g gens)
            fmap (fmap (\sg -> if snippet then sg {sgKind = "snippet"} else sg)) <$> suggestionOf fa path line s'
          (s', _) -> pure (s', Nothing)
      Silence ->
        indexAt path s >>= \case
          (s', Just (root, project, ix, gens, text)) | gen : _ <- [gen | gen <- gens, generatorIdText (genId gen) == g] ->
            (s',) <$> recognitionSuggestion path line s' root project gens (recognised c ix gen (makeRelative root path) text) text
          (s', _) -> pure (s', Nothing)
    recognised c ix gen rel text =
      let conventions = [cv | cv <- ixConventions ix, cvGenerator cv == genId gen]
          placed cv = isJust (invertPath (cvPattern cv) rel)
          convention = listToMaybe (filter placed conventions <> conventions)
          inputs = maybe [] (\cv -> validInputs gen (inputsFor gen cv rel text)) convention
          complete = and [inputName i `elem` map fst inputs | i <- genInputs gen, not (inputOptional i), inputDefault i == Nothing]
          place
            | any placed conventions = 0.25
            | takeDirectory rel `S.member` M.findWithDefault S.empty (genId gen) (ixSiblingDirs ix) = 0.15
            | otherwise = 0
       in Recognition
          { rcGenerator = genId gen
          , rcName = maybe (generatorName gen) cvName convention
          , rcBand = if complete then Offer else Hint
          , rcScore = c
          , rcShape = 0
          , rcPlace = place
          , rcLike = "the template"
          , rcInputs = inputs
          , rcOr = Nothing
          , rcRival = Nothing
          , rcReasons = ["anticipated at " <> T.pack (show c)]
          }

-- | The generator an answer chose, and how surely, when at least as surely
-- as the moment's gate.
sureAnswer :: Moment -> Answer -> Maybe (Text, Double)
sureAnswer moment answer = case answer of
  Chosen g _ (Confidence c) | confident (if moment == Tie then tieGate else silenceGate) answer -> Just (g, c)
  _ -> Nothing

-- | What to ask at a line, if anything: the moment, a fingerprint of what
-- the answer depends on (the unit, the cursor's line and the candidates)
-- and the question.
anticipationAsk :: FilePath -> Int -> ServerState -> IO (ServerState, Maybe (Moment, Text, Question))
anticipationAsk path line s =
  anticipateCandidates path line s >>= \case
    (s', Nothing) -> pure (s', Nothing)
    (s', Just (moment, cands)) -> do
      prefix@(unit, cursor, _) <- prefixAt <$> textOf s' <*> pure line
      pure (s', Just (moment, fingerprint (unit : cursor : map fst cands), anticipateQuestion moment path prefix cands))
  where
    textOf st =
      projectFor path st >>= \case
        (_, Just (root, project)) -> fromMaybe "" <$> readText (prWorkspace project) (makeRelative root path)
        _ -> pure ""

-- | Candidates, the question from the prefix, one ask, its anticipation.
anticipateAt :: FilePath -> Int -> ServerState -> Decider IO -> IO (ServerState, Maybe Anticipation)
anticipateAt path line s d =
  anticipationAsk path line s >>= \case
    (s', Nothing) -> pure (s', Nothing)
    (s', Just (moment, _, q)) -> do
      answer <- ask d q
      anticipationFrom path line moment answer s'

-- | A file's project, its language's recognition index and generators, and
-- its text (as it is, unsaved).
indexAt :: FilePath -> ServerState -> IO (ServerState, Maybe (FilePath, Project, Index, [Generator], Text))
indexAt path s =
  projectFor path s >>= \case
    (s', Just (root, project)) | Just lang <- languageFor (catalogLanguages (prCatalog project)) (makeRelative root path) -> do
      (s'', ix, gens) <- indexFor root project lang s'
      text <- fromMaybe "" <$> readText (prWorkspace project) (makeRelative root path)
      pure (s'', Just (root, project, ix, gens, text))
    (s', _) -> pure (s', Nothing)

-- | The actions whose place covers a line.
coveringAt :: Int -> [FileAction] -> [FileAction]
coveringAt line acts = [fa | fa@(FileAction _ (Just (Range (l1, _) (l2, _)))) <- acts, l1 <= line && line <= l2]
