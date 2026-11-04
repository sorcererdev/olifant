-- | @olifant init --language NAME@, the pure parts (the IO edge is
-- 'Olifant.Cli.runInitLanguage'): the model's survey of the project, the
-- sample of its files, the errand's inputs and holes, which query files to
-- write, the answers' checks, and the failing rows of doctor.
--
-- The model writes text; olifant checks it and decides where it goes. The
-- grammar's repository is the model's proposal, but its revision is olifant's
-- ('Olifant.Language.Grammar.remoteHead').
module Olifant.LanguageInit
  ( Survey (..)
  , parseSurvey
  , surveyRequest
  , sampleFor
  , cutTo
  , unfence
  ) where

import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Fill (FillHole (..), FillRequest (..), Stance (..))
import Olifant.Glob (matchesGlob)
import Olifant.Toml qualified as Toml
import System.FilePath (splitDirectories)

-- | The survey's answer: the grammar's repository (and, in a monorepo, the
-- folder and the name the loader reads), the globs of source and test files,
-- the manifest, and the test runners the project could use.
data Survey = Survey
  { svGrammar :: Text
  , svPath :: Maybe FilePath
  , svName :: Maybe Text
  , svFiles :: [Text]
  , svTests :: [Text]
  , svManifest :: Maybe FilePath
  , svRunners :: [Text]
  }
  deriving stock (Eq, Show)

-- | The survey's TOML (inside one markdown fence at most): @grammar@ and
-- @files@ are required.
parseSurvey :: Text -> Either Text Survey
parseSurvey reply = do
  top <- either (\e -> Left ("the survey is not TOML: " <> e)) Right (Toml.parseToml (unfence reply))
  grammar <- Toml.required "grammar" Toml.text top
  if T.null (T.strip grammar) then Left "the survey's `grammar` is empty" else pure ()
  files <- Toml.required "files" (Toml.list Toml.text) top
  if null files then Left "the survey's `files` is empty" else pure ()
  path <- Toml.optional "path" Toml.text top
  name <- Toml.optional "name" Toml.text top
  tests <- fromMaybe [] <$> Toml.optional "tests" (Toml.list Toml.text) top
  manifest <- Toml.optional "manifest" Toml.text top
  runners <- fromMaybe [] <$> Toml.optional "runners" (Toml.list Toml.text) top
  pure (Survey (T.strip grammar) (T.unpack <$> path) name files tests (T.unpack <$> manifest) runners)

-- | A reply's text with one leading and one trailing markdown fence line
-- (@```toml@, @```@) dropped; anything else is left as it is.
unfence :: Text -> Text
unfence reply =
  case T.lines (T.strip reply) of
    first : rest@(_ : _)
      | "```" `T.isPrefixOf` T.strip first
      , T.strip (last rest) == "```" ->
          T.unlines (init rest)
    _ -> reply

-- | The survey: one hole, @survey@, whose answer is TOML naming the grammar
-- and the project's files. Its inputs are the language, the top-level
-- listing and the first 200 tracked paths.
surveyRequest :: Text -> [FilePath] -> FillRequest
surveyRequest language tracked =
  FillRequest
    [FillHole "survey" "toml" instruction "survey.toml" "\10214survey\10215" [] False Nothing]
    [ ("language", language)
    , ("top level", T.intercalate ", " (nub [T.pack (head' p) | p <- tracked]))
    , ("tracked files", T.intercalate "\n" (map T.pack (take 200 tracked)))
    ]
    []
    FileStance
  where
    head' p = case splitDirectories p of
      [f] -> f
      d : _ -> d <> "/"
      [] -> p
    instruction =
      T.unlines
        [ "TOML with these keys, about the " <> language <> " code in this project:"
        , "grammar = \"URL\" (the git repository of the tree-sitter grammar for " <> language <> ", https)"
        , "path = \"DIR\" (only when the grammar is one folder of a repository holding several)"
        , "name = \"NAME\" (only when the grammar's C symbol is not tree_sitter_<language>)"
        , "files = [globs] (the source files; ** spans folders)"
        , "tests = [globs] (the test files)"
        , "manifest = \"FILE\" (the project's manifest, if it has one)"
        , "runners = [programs] (the test runners this project could use, by command name)"
        ]

-- | The sample the errand shows the model: the first two source files (not
-- tests) and the first two tests, shallowest folder first, then by path;
-- and the manifest when the project has it.
sampleFor :: Survey -> [FilePath] -> ([FilePath], [FilePath], Maybe FilePath)
sampleFor sv tracked = (take 2 sources, take 2 tests, manifest)
  where
    ordered = sortOn (\p -> (length (splitDirectories p), p)) tracked
    isTest p = any (`matchesGlob` p) (svTests sv)
    sources = [p | p <- ordered, any (`matchesGlob` p) (svFiles sv), not (isTest p)]
    tests = filter isTest ordered
    manifest = case svManifest sv of
      Just m | m `elem` tracked -> Just m
      _ -> Nothing

-- | The first N lines.
cutTo :: Int -> Text -> Text
cutTo n = T.unlines . take n . T.lines
