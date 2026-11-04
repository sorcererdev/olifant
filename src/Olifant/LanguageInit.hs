{-# LANGUAGE MultiWayIf #-}


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
  , pinText
  , withPin
  , initHeader
  , ErrandInput (..)
  , Answered (..)
  , checkAnswers
  , errandCore
  , shippedFor
  , failingRows
  , languageRows
  , repairRequest
  , repairHoles
  , readUpstream
  ) where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory (doesDirectoryExist, listDirectory)
import Data.Char (isAlphaNum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.List (nub, sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Doctor (Check (..), Status (..))
import Olifant.Fill (FillHole (..), FillRequest (..), Filler (..), Stance (..))
import Olifant.Language (Grammar (..), GrammarSource (..), Language (..), Layer (..), languageFromLayers, queriesFor, queryError)
import Olifant.Language.Presets (overPresets, presetsFrom, shippedPresets)
import Olifant.Glob (matchesGlob)
import Olifant.Language.TreeSitter (knownPredicates, readCaptures)
import Olifant.Library (providersSection, shippedLanguages, shippedToolchains)
import Olifant.Template.Parse (knownFilters)
import Olifant.Toml qualified as Toml
import System.FilePath (splitDirectories, takeExtension, (</>))

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
-- the ones olifant reads), the test runners found on PATH, the template
-- filters and query predicates olifant knows (closed sets: a model invents
-- @go_string@ or @#strip!@ without them) and the providers' protocol.
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
    , ("template filters", "only these, after a bar ({= message|escaped =}): " <> T.intercalate ", " knownFilters)
    , ("query predicates", "only these; no directives (#set!, #strip!, …): " <> T.intercalate ", " (map ("#" <>) knownPredicates))
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
        , "Leave out [check]: the user writes the check command, not the model (the exemplar's [check] names a preset this language does not have)."
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

-- | The first line of every file @init --language@ writes.
initHeader :: Text
initHeader = "# written by olifant init --language"

-- | The pinned grammar as a language file's @grammar@ line: olifant's URL
-- and revision, never the model's text.
pinText :: Grammar -> Text
pinText g = case grSource g of
  Just (SourceGit url rev path) ->
    "grammar = { git = " <> quoted url <> ", rev = " <> quoted rev <> maybe "" (\p -> ", path = " <> quoted (T.pack p)) path <> ", name = " <> quoted (grName g) <> " }"
  _ -> "grammar = " <> quoted (grName g)
  where
    quoted t = "\"" <> T.concatMap (\c -> if c `elem` ['"', '\\'] then T.pack ['\\', c] else T.singleton c) t <> "\""

-- | A language file's text with its top-level @grammar@ lines (before the
-- first table header) dropped and the pin and 'initHeader' put first.
withPin :: Text -> Text -> Text
withPin pin text =
  T.unlines ([initHeader, pin] ++ filter (not . grammarLine) top ++ rest)
  where
    (top, rest) = break isHeader (T.lines (unfence text))
    isHeader l = "[" `T.isPrefixOf` T.stripStart l
    grammarLine l = case T.stripPrefix "grammar" (T.stripStart l) of
      Just after -> "=" `T.isPrefixOf` T.stripStart after
      Nothing -> l == initHeader

-- | What the errand needs once the grammar is loaded: the language, its
-- pinned grammar and label, the grammar's node kinds, the checkout's
-- queries and licence, the shipped query set by grammar name (production:
-- 'shippedFor'), the test runners found on PATH, the sample's files, and
-- the project folder (a layer's own folder).
data ErrandInput = ErrandInput
  { eiLanguage :: Text
  , eiGrammar :: Grammar
  , eiKinds :: [Text]
  , eiUpstream :: Upstream
  , eiShipped :: Text -> [(FilePath, Text)]
  , eiTools :: [Text]
  , eiSample :: [(FilePath, Text)]
  , eiProjectDir :: FilePath
  }

-- | The query set olifant ships for a grammar.
shippedFor :: Text -> [(FilePath, Text)]
shippedFor = queriesFor

-- | A round's checked answers: the files to write (path under the project,
-- text) and the rows that failed (a file that does not load, a query that
-- does not compile, a hole with no answer).
data Answered = Answered {anFiles :: [(FilePath, Text)], anFailing :: [Check]}
  deriving stock (Eq, Show)

-- | The answers checked before anything is written (by hole name, with
-- the round's suffix: "" or "_repair"): the language must load as a project
-- layer over the toolchain answered (its @grammar@ replaced by the pin), the
-- toolchain must read as a preset, each query must compile against the
-- grammar. Only the holes asked are read; a repair round that does not ask
-- for the toolchain loads the language over the one written (WRITTEN).
checkAnswers :: ErrandInput -> [(FilePath, Text)] -> [Text] -> Text -> Map Text Text -> Answered
checkAnswers ei written asked suffix answers =
  Answered (concat [fs | Right fs <- results]) [c | Left c <- results]
  where
    l = eiLanguage ei
    g = eiGrammar ei
    answer h = M.lookup (h <> suffix) answers
    results = map one asked
    noAnswer h = Left (Check (l <> ": " <> h) Bad ("the model gave no `" <> h <> suffix <> "`") Nothing)
    doesNotLoad h why = Left (Check (l <> ": " <> h) Bad ("does not load: " <> why) (Just "regenerate the key or supply a provider (see chapter 9, Providers)"))
    toolchainText = fmap (\t -> T.unlines [initHeader] <> unfence t) (answer "toolchain")
    presets = case toolchainText <|> lookup (toolchainFile l) written of
      Just t | Right p <- presetsFrom "project" [(toolchainName l, t)] -> overPresets shippedPresets p
      _ -> shippedPresets
    one = \case
      "language" -> case answer "language" of
        Nothing -> noAnswer "language"
        Just t ->
          let text = withPin (pinText g) t
           in case Toml.parseToml text >>= \top -> languageFromLayers presets l [Layer "project" (Just (eiProjectDir ei)) top] of
                Left why -> doesNotLoad "language" why
                Right lang
                  | langGrammar lang /= g -> doesNotLoad "language" "its grammar is not the pinned one"
                  | otherwise -> Right [(languageFile l, text)]
      "toolchain" -> case toolchainText of
        Nothing -> noAnswer "toolchain"
        Just t -> case presetsFrom "project" [(toolchainName l, t)] of
          Left why -> doesNotLoad "toolchain" why
          Right _ -> Right [(toolchainFile l, t)]
      h -> case answer h of
        Nothing -> noAnswer h
        Just t ->
          let file = T.unpack h <> ".scm"
              text = unfence t
           in case queryError g [(file, text)] of
                Just why -> Left (Check (l <> ": queries") Bad (T.pack file <> ": " <> why) Nothing)
                Nothing -> Right [(".olifant" </> "queries" </> T.unpack (grName g) </> file, text)]

-- | The errand after the gate: the query files to write (copied or asked),
-- one request for the language, its toolchain and the asked queries, and
-- the answers checked. The request comes back for a repair round.
errandCore :: Monad m => Filler m -> ErrandInput -> m (Either Text (FillRequest, Answered))
errandCore filler ei = do
  let g = eiGrammar ei
      up = eiUpstream ei
      (copied, queryHoles) = queriesToWrite (grName g) (eiShipped ei (grName g)) up
      captures = nub (concatMap (captureNamesIn . snd) (upQueries up))
      req = errandRequest (eiLanguage ei) (upLabel up) (eiKinds ei) captures (eiTools ei) (eiSample ei) queryHoles
      dir = ".olifant" </> "queries" </> T.unpack (grName g)
      queryFiles =
        [(dir </> f, t) | (f, t) <- copied]
          ++ [(dir </> "SOURCES", sourcesFile (grName g) (upLabel up) copied queryHoles) | not (null copied && null queryHoles)]
  runFill filler req >>= \case
    Left why -> pure (Left why)
    Right answers -> do
      let checked = checkAnswers ei [] (map fhName (frHoles req)) "" answers
      pure (Right (req, checked {anFiles = anFiles checked ++ queryFiles}))

-- | Doctor's rows about a language: its capability, grammar, queries and
-- sibling rows (@<l>: …@), its tool rows (@… (<l>)@) and its own
-- @language <l>@ line.
languageRows :: Text -> [Check] -> [Check]
languageRows l = filter ours
  where
    ours c =
      let n = checkName c
       in (l <> ": ") `T.isPrefixOf` n || (" (" <> l <> ")") `T.isSuffixOf` n || n == "language " <> l || ("languages/" <> l <> ".toml") `T.isInfixOf` n

-- | The rows a repair round answers: the language's errors, its evidence
-- that disproved a key (the @regenerate the key@ fix), and a queries or
-- grammar warn. Not an unproven sibling row, an absent optional capability
-- (conjure shapes, signature, mutation, stub: a new language starts at the
-- floor), nor another language's rows.
failingRows :: Text -> [Check] -> [Check]
failingRows l = filter failing . languageRows l
  where
    failing c =
      checkStatus c == Bad
        || (checkStatus c == Warn && maybe False ("regenerate the key" `T.isInfixOf`) (checkFix c))
        || (checkStatus c == Warn && checkName c `elem` [l <> ": queries", l <> ": grammar"])

-- | The holes of REQ the rows point at: a row named after a hole (a file that
-- did not load), a queries row's file (or every query hole), a test runner
-- or tests row the toolchain, a grammar row none, anything else the
-- language file.
repairHoles :: FillRequest -> [Check] -> [Text]
repairHoles req rows = [h | h <- names, h `elem` wanted]
  where
    names = map fhName (frHoles req)
    queryHoles = filter (`notElem` ["language", "toolchain"]) names
    wanted = concatMap pointsAt rows
    pointsAt c =
      let n = checkName c
          suffix = T.drop 2 (snd (T.breakOn ": " n))
       in if
            | suffix `elem` names -> [suffix]
            | suffix == "queries" -> case [h | h <- queryHoles, (h <> ".scm") `T.isPrefixOf` checkDetail c] of
                [] -> queryHoles
                hs -> hs
            | suffix == "grammar" -> []
            | "test runner " `T.isPrefixOf` n || suffix == "tests" -> ["toolchain"]
            | otherwise -> ["language"]

-- | The repair round: the holes the failing rows point at, renamed
-- @<h>_repair@ (a recorded model answers each name with one text, so the
-- second round needs its own names), with the files as written and the
-- rows (name, detail, fix) in its context.
repairRequest :: FillRequest -> [(FilePath, Text)] -> [Check] -> FillRequest
repairRequest req written rows =
  req
    { frHoles = [h {fhName = fhName h <> "_repair", fhInstruction = fhInstruction h <> "\nThe file below as written fails the rows under \"failing rows\": write it again, whole, so they pass."} | h <- frHoles req, fhName h `elem` asked]
    , frContext = frContext req ++ written ++ [("failing rows", T.unlines [checkName c <> " — " <> checkDetail c <> maybe "" ("; fix: " <>) (checkFix c) | c <- rows])]
    }
  where
    asked = repairHoles req rows

-- | The grammar checkout's @queries/*.scm@ (beside the grammar's folder in a
-- monorepo, else at the checkout's root) and its licence, read from disk,
-- under LABEL (URL @SHA7). No checkout: none.
readUpstream :: Text -> Maybe FilePath -> Maybe FilePath -> IO Upstream
readUpstream label checkout path = case checkout of
  Nothing -> pure (Upstream label [] Nothing)
  Just dir -> do
    found <- filterM doesDirectoryExist ([dir </> p </> "queries" | Just p <- [path]] ++ [dir </> "queries"])
    queries <- case found of
      q : _ -> do
        names <- sort . filter ((== ".scm") . takeExtension) <$> listDirectory q
        mapM (\n -> (n,) <$> readUtf8 (q </> n)) names
      [] -> pure []
    tops <- sort <$> listDirectory dir
    licence <- case [f | f <- tops, "LICENSE" `T.isPrefixOf` T.toUpper (T.pack f)] of
      f : _ -> Just . (f,) <$> readUtf8 (dir </> f)
      [] -> pure Nothing
    pure (Upstream label queries licence)
  where
    readUtf8 f = TE.decodeUtf8With lenientDecode <$> BS.readFile f
