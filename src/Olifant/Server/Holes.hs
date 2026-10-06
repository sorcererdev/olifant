-- | Internal to Olifant.Server: the holes left in a project — placeholders
-- an unfilled plan wrote (`olifant: NAME: instruction` in every language's
-- placeholder) — and filling one on request: the placeholder becomes its
-- hole again, planned with the model.
module Olifant.Server.Holes (Hole (..), holesFor, holeAt, fillHoleEdits, fillHolePlan) where

import Control.Monad (forM)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.App (PlanOptions (..), Project (..), planOptions, planWith)
import Olifant.Catalog (catalogLanguages)
import Olifant.Generation (Plan (..), ProgressEvent, Step (..), describeConflict, describeGenerationError)
import Olifant.Generator (replaceGenerator)
import Olifant.Language (HoleKind (..), Language (..), languageFor)
import Olifant.Server.Actions (fillerFor)
import Olifant.Server.Edits (FileEdit, editsOfPlan)
import Olifant.Server.State (ServerState (..), memo, projectFor)
import Olifant.Template (Value (..), describeTemplateError, parseTemplate, render)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (makeRelative, (</>))
import Text.Regex.TDFA (Regex, makeRegex, matchM)

-- | A hole: its file (absolute), line (1-based), name, kind, instruction,
-- and the placeholder's text on the line.
data Hole = Hole {hoPath :: FilePath, hoLine :: Int, hoName :: Text, hoKind :: Text, hoInstruction :: Text, hoPlaceholder :: Text}
  deriving stock (Eq, Show)

-- | Every hole in the project of a file (in the files of languages that
-- have hole kinds); none in no project.
holesFor :: FilePath -> ServerState -> IO (ServerState, [Hole])
holesFor path s =
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', [])
    (s', Just (root, project)) -> do
      let langs = [l | l <- catalogLanguages (prCatalog project), not (null (langHoles l))]
      paths <- listFiles (prWorkspace project) "."
      found <- forM [(p, l) | p <- paths, Just l <- [languageFor langs p]] $ \(p, l) ->
        maybe [] (holesIn (normalisePath (root </> p)) l) <$> readText (prWorkspace project) p
      pure (s', concat found)

-- | The hole on a line of a file, if there is one.
holeAt :: FilePath -> Int -> ServerState -> IO (ServerState, Maybe (FilePath, Project, Hole))
holeAt path line = memo "hole" path (T.pack (show line)) $ \s ->
  projectFor path s >>= \case
    (s', Nothing) -> pure (s', Nothing)
    (s', Just (root, project)) -> do
      let rel = makeRelative root path
      text <- readText (prWorkspace project) rel
      pure (s', (root,project,) <$> listToMaybe [h | Just t <- [text], Just l <- [languageFor (catalogLanguages (prCatalog project)) rel], h <- holesIn path l t, hoLine h == line])

-- | Fill the hole on a line with the model: its edits.
fillHoleEdits :: (ProgressEvent -> IO ()) -> FilePath -> Int -> ServerState -> IO (ServerState, Either Text [FileEdit])
fillHoleEdits progress path line s = fillHolePlan progress path line s <&> \(s', planned) -> (s', (\(root, plan) -> editsOfPlan s' root plan) <$> planned)

-- | 'fillHoleEdits' as the plan itself, with the project's root.
fillHolePlan :: (ProgressEvent -> IO ()) -> FilePath -> Int -> ServerState -> IO (ServerState, Either Text (FilePath, Plan))
fillHolePlan progress path line s =
  holeAt path line s >>= \case
    (s', Nothing) -> pure (s', Left ("no hole on line " <> T.pack (show line) <> " (it was filled, or the code changed)"))
    (s', Just (root, project, hole)) -> fmap (s',) $ do
      let rel = makeRelative root path
      old <- maybe "" (\t -> maybe "" id (listToMaybe (drop (line - 1) (T.splitOn "\n" t)))) <$> readText (prWorkspace project) rel
      let bare = T.takeWhile (/= '[') (hoName hole)
          marker = "{= ? " <> bare <> ": " <> hoKind hole <> " | " <> hoInstruction hole <> " =}"
      case parseTemplate (T.replace (hoPlaceholder hole) marker old) of
        Left err -> pure (Left (describeTemplateError err))
        Right template ->
          fillerFor True s' project >>= \case
            Left why -> pure (Left why)
            Right filler ->
              planWith (ssPorts s') project planOptions {poFiller = filler, poProgress = progress} [Step (replaceGenerator rel (line, line) old template) M.empty] >>= \case
                Left err -> pure (Left (describeGenerationError err))
                Right plan
                  | not (null (planConflicts plan)) -> pure (Left (T.intercalate "; " (map describeConflict (planConflicts plan))))
                  | otherwise -> pure (Right (root, plan))

-- | The holes in a file's text: lines holding `olifant: NAME: instruction`
-- inside one of the language's placeholders.
holesIn :: FilePath -> Language -> Text -> [Hole]
holesIn path lang text = mapMaybe hole (zip [1 ..] (T.splitOn "\n" text))
  where
    marked = makeRegex ("olifant: ([A-Za-z_][A-Za-z0-9_]*(\\[[^]]*\\])?): (.*)$" :: String) :: Regex
    hole (n, l) = do
      (_, _, _, [name, _, rest]) <- matchM marked (T.unpack l) :: Maybe (String, String, String, [String])
      let instruction = unescape (T.pack (takeWhileLastQuote rest))
      listToMaybe
        [ Hole path n (T.pack name) kind instruction placeholder
        | (kind, k) <- langHoles lang
        , Right placeholder <- [render (M.fromList [("hole", VText (T.pack name)), ("instruction", VText instruction)]) (kindPlaceholder k)]
        , "olifant: " `T.isInfixOf` placeholder
        , placeholder `T.isInfixOf` l
        ]
    -- the instruction ends at the string's closing quote
    takeWhileLastQuote r = case [i | (i, '"') <- zip [0 :: Int ..] r, i == 0 || r !! (i - 1) /= '\\'] of
      [] -> r
      quotes -> take (last quotes) r
    unescape = T.replace "\\\\" "\\" . T.replace "\\\"" "\""
