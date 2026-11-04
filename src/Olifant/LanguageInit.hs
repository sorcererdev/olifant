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
  , providersSection
  , captureNamesIn
  , Upstream (..)
  , queriesToWrite
  , errandRequest
  , languageFile
  , toolchainFile
  , toolchainName
  , sourcesFile
  ) where

import Data.Char (isAlphaNum)
import Data.List (nub, sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Fill (FillHole (..), FillRequest (..), Stance (..))
import Olifant.Glob (matchesGlob)
import Olifant.Language.TreeSitter (readCaptures)
import Olifant.Library (providersSection, shippedLanguages, shippedToolchains)
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

-- | The capture names a query uses (@\@definition.function@ gives
-- @definition.function@), in order, each once.
captureNamesIn :: Text -> [Text]
captureNamesIn q = nub [T.takeWhile capChar rest | chunk <- drop 1 (T.splitOn "@" (stripComments q)), let rest = chunk, not (T.null (T.takeWhile capChar rest))]
  where
    capChar c = isAlphaNum c || c `elem` ("._-" :: String)
    stripComments = T.unlines . map (fst . T.breakOn ";") . T.lines

-- | What the grammar's checkout holds for the errand: its label (URL @SHA),
-- its @queries/*.scm@, and its licence (file name, text).
data Upstream = Upstream {upLabel :: Text, upQueries :: [(FilePath, Text)], upLicence :: Maybe (FilePath, Text)}
  deriving stock (Eq, Show)

-- | The query files a grammar needs, given its shipped set: none when olifant
-- ships some; else upstream's @tags.scm@ copied (a header line naming where
-- it is from and its licence) or a @tags@ hole, and @imports@ and
-- @textobjects@ holes, sorted.
queriesToWrite :: Text -> [(FilePath, Text)] -> Upstream -> ([(FilePath, Text)], [Text])
queriesToWrite _ shipped up
  | not (null shipped) = ([], [])
  | otherwise = case lookup "tags.scm" (upQueries up) of
      Just tags -> ([("tags.scm", header <> "\n" <> tags)], ["imports", "textobjects"])
      Nothing -> ([], ["imports", "tags", "textobjects"])
  where
    header = "; from " <> upLabel up <> maybe "" licence (upLicence up)
    licence (f, t) = " (" <> T.pack f <> case filter (not . T.null) (map T.strip (T.lines t)) of
      l : _ -> ": " <> l <> ")"
      [] -> ")"

-- | The toolchain preset a new language's tests run with: @<language>-test@.
toolchainName :: Text -> Text
toolchainName l = l <> "-test"

-- | Where the errand's files go, under the project.
languageFile, toolchainFile :: Text -> FilePath
languageFile l = ".olifant/languages/" <> T.unpack l <> ".toml"
toolchainFile l = ".olifant/toolchains/" <> T.unpack (toolchainName l) <> ".toml"

-- | The errand: one hole per file (@language@, @toolchain@, and each query
-- file to write), the shipped Rust pair as exemplars, the sample's files,
-- and the grammar's facts (its node kinds, the captures upstream uses and
-- the ones olifant reads), the test runners found on PATH and the
-- providers' protocol.
errandRequest :: Text -> Text -> [Text] -> [Text] -> [Text] -> [(FilePath, Text)] -> [Text] -> FillRequest
errandRequest language grammar kinds captures tools sample queryHoles =
  FillRequest
    ([fileHole "language" (languageFile language) languageInstruction, fileHole "toolchain" (toolchainFile language) toolchainInstruction] ++ map queryHole queryHoles)
    [ ("language", language)
    , ("grammar", grammar)
    , ("node kinds", T.intercalate ", " kinds)
    , ("captures", T.intercalate ", " captures)
    , ("captures olifant reads", T.intercalate ", " (readCaptures ++ ["definition.<kind>"]))
    , ("tools on PATH", T.intercalate "\n" tools)
    , ("protocol", providersSection)
    ]
    ( [("exemplar: rust.toml", t) | Just t <- [lookup "rust" shippedLanguages]]
        ++ [("exemplar: cargo-test.toml", t) | Just t <- [lookup "cargo-test" shippedToolchains]]
        ++ sample
    )
    FileStance
  where
    fileHole name file instruction = FillHole name "file" instruction file ("\10214" <> name <> "\10215") [] False Nothing
    queryHole h = fileHole h (".olifant/queries/" <> T.unpack h <> ".scm") (queryInstruction h)
    languageInstruction =
      T.unlines
        [ "The language file for " <> language <> ", in the exemplar rust.toml's form: its keys, read from the sample files."
        , "Its tests: [test] preset = \"" <> toolchainName language <> "\" (the toolchain hole), tests_for, unit and assertion regexes (POSIX classes, as the exemplar)."
        , "Write grammar = { git = \"URL\", rev = \"REV\" } as given; olifant replaces it with its pin."
        , "Leave out a key the sample does not show you how to answer."
        ]
    toolchainInstruction =
      T.unlines
        [ "The test preset " <> toolchainName language <> ", in the exemplar cargo-test.toml's form: [test] command, format, root, tests_for, install."
        , "Run tests only with a tool listed on PATH; a format the protocol names (junit reads {= report =})."
        ]
    queryInstruction = \case
      "tags" -> "tree-sitter queries (tags.scm) over the node kinds given: each definition @definition.<kind> (function, method, class, …) with its name @name."
      "imports" -> "tree-sitter queries (imports.scm) over the node kinds given: each import declaration @import."
      "textobjects" -> "tree-sitter queries (textobjects.scm) over the node kinds given: @function.outer, @function.inner, @class.outer, @class.inner, @parameter.inner and @call.outer."
      h -> "tree-sitter queries (" <> h <> ".scm) over the node kinds given."

-- | The @SOURCES@ file beside a project's generated queries: each file and
-- where it came from (copied from the grammar's repository, or written by
-- the model), as @library/queries/<g>/SOURCES@ does.
sourcesFile :: Text -> Text -> [(FilePath, Text)] -> [Text] -> Text
sourcesFile g grammar copied holes =
  T.unlines $
    ["# Tree-sitter queries for the " <> g <> " grammar (" <> grammar <> "), written by olifant init --language."]
      ++ sort ([T.pack f <> "  copied from " <> grammar <> " queries/" <> T.pack f | (f, _) <- copied] ++ [h <> ".scm  written by the model, compiled against the grammar" | h <- holes])
