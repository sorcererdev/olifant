module Olifant.Cli.Options
  ( Options (..)
  , Format (..)
  , Command (..)
  , By (..)
  , GenOptions (..)
  , StepSpec (..)
  , SyncMode (..)
  , parseOptions
  ) where

import Control.Monad (unless)
import Data.Aeson (FromJSON (..), Value (..), withObject, (.:), (.:?))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as Aeson
import Data.Foldable (toList)
import Data.Scientific (floatingOrInteger)
import Data.Map.Strict qualified as M
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, UTCTime, defaultTimeLocale, parseTimeM)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Options.Applicative
import Control.Applicative ((<|>))
import System.Exit (ExitCode (..))

data Format = Json | Human
  deriving stock (Eq, Show)

data Options = Options {optFormat :: Maybe Format, optCommand :: Command}
  deriving stock (Eq, Show)

-- | 'ListCmd': --detail. 'AnchorsCmd': a language's `at` targets. 'WhereCmd': file, phrase.
data Command = ListCmd Bool | ShowCmd Text [(Text, Text)] | GenCmd GenOptions | AnchorsCmd Text | WhereCmd FilePath Text | SyncCmd SyncMode | SchemaCmd | NewCmd Text (Maybe Text) | ActionsCmd FilePath (Maybe (Int, Int)) Bool | DoctorCmd Bool | CheckCmd Bool (Maybe FilePath) Bool Bool Bool (Maybe Text) | FixCmd Text FilePath (Int, Int) Int [(Text, Text)] GenOptions | CompleteCmd FilePath (Int, Int) Bool | InitCmd Text Text (Maybe FilePath) Bool | InitMigrateCmd [FilePath] (Maybe Text) (Maybe Text) | InitLanguageCmd Text (Maybe Text) (Maybe Text) Bool Bool (Maybe FilePath) | LspCmd | AnticipateCmd FilePath Int (Maybe FilePath) (Maybe FilePath)
  | -- | `plans cost`: errands against Claude Code sessions, per branch.
    -- 'pcoTranscripts' overrides @~/.claude/projects/<slug>@.
    PlansCostCmd {pcoBranch :: Maybe Text, pcoSince :: Maybe Day, pcoJson :: Bool, pcoTranscripts :: Maybe FilePath, pcoBy :: By, pcoPlan :: Maybe FilePath, pcoChecks :: Bool}
  deriving stock (Eq, Show)

-- | How `plans cost` groups: per plan and the landing (PR) its work reached, or per recorded branch.
data By = ByPlan | ByBranch
  deriving stock (Eq, Show)

-- | `olifant sync` (spec §5.8): refresh regions, check them (write nothing), or create one's fences.
data SyncMode = SyncApply | SyncCheck | SyncInit Text | SyncPlan
  deriving stock (Eq, Show)

-- | One step of a `gen` run: a generator and exactly its inputs (spec §7).
data StepSpec = StepSpec {specGenerator :: Text, specSets :: [(Text, Text)]}
  deriving stock (Eq, Show)

instance FromJSON StepSpec where
  parseJSON = withObject "step" $ \o -> do
    let unknown = filter (`notElem` ["gen", "set"]) (map Key.toText (KM.keys o))
    unless (null unknown) $
      fail ("unknown key(s) " <> show unknown <> "; each step is {\"gen\": GENERATOR, \"set\": {...}}: use \"gen\" and \"set\"")
    gen <- o .: "gen"
    sets <- maybe (pure []) (traverse (\(k, v) -> (k,) <$> asText v) . M.toList) =<< (o .:? "set" :: Aeson.Parser (Maybe (M.Map Text Value)))
    pure (StepSpec gen sets)
    where
      -- JSON values become the same text a --set would carry: numbers and
      -- booleans as written, arrays joined with "," (the CLI list form).
      asText = \case
        String t -> pure t
        Number n -> pure (T.pack (either (show :: Double -> String) (show :: Integer -> String) (floatingOrInteger n)))
        Bool b -> pure (if b then "true" else "false")
        Array xs -> T.intercalate "," <$> traverse scalar (toList xs)
        other -> fail ("expected text, a number, a boolean or a list, got " <> show other)
      scalar = \case
        Array _ -> fail "nested lists are not supported"
        Object _ -> fail "objects are not supported in a list"
        v -> asText v

data GenOptions = GenOptions
  { genSteps :: [StepSpec]
  , genStepsFile :: Maybe FilePath
  , genPlanOnly :: Bool
  , genNow :: Maybe UTCTime
  , genModel :: Maybe Text
  -- ^ `--model NAME`: which `[model.backends.NAME]` fills holes
  , genNoFill :: Bool
  -- ^ `--no-fill`: leave every hole as its placeholder
  , genProgress :: Bool
  -- ^ `--progress`: JSON-line progress events on stderr (for editors)
  , genOverlay :: Maybe FilePath
  -- ^ `--overlay FILE`: FILE's current text comes from stdin (with --plan)
  , genAt :: Maybe (FilePath, Int, Int)
  -- ^ `--at FILE:LINE:COL`: the cursor a snippet goes at (spec §5.3)
  , genFills :: Maybe FilePath
  -- ^ `--fills FILE`: recorded fills (a `[fills]` table) stand in for the model
  }
  deriving stock (Eq, Show)

-- | Left carries the text to print (usage or help) and the exit code.
parseOptions :: [String] -> Either (Text, ExitCode) Options
parseOptions ("gen" : rest)
  | "--help" `notElem` rest && "-h" `notElem` rest = either (\e -> Left (e <> "\n", ExitFailure 2)) Right (parseGen rest)
-- A generator name where a command belongs (`olifant elm/page …`) is a
-- common slip for agents: point at `gen` instead of optparse's generic error.
parseOptions (ref : rest)
  | '/' `elem` ref && take 1 ref /= "-" =
      Left ("unknown command `" <> T.pack ref <> "`; did you mean `olifant gen " <> T.pack (unwords (ref : rest)) <> "`?\n", ExitFailure 2)
parseOptions args = case execParserPure defaultPrefs parserInfo args of
  Success o -> Right o
  Failure f -> let (msg, code) = renderFailure f "olifant" in Left (T.pack msg <> "\n", code)
  CompletionInvoked _ -> Left ("shell completion is not supported\n", ExitFailure 2)
  where
    parserInfo = info (commands <**> helper) (fullDesc <> progDesc "Olifant: Durandal's generator engine" <> footer "Generators live in packs (listed under [packs] in .olifant/config.toml) and in the project: .olifant/generators/NAME/generator.toml is found automatically as NAME. `olifant show GEN` says where a generator is defined.")
    commands =
      hsubparser
        ( cmd "list" (ListCmd <$> switch (long "detail" <> help "Include every generator's inputs, sub-generators and examples")) "List generators"
            <> cmd "show" (ShowCmd <$> strArgument (metavar "GENERATOR") <*> many (option (eitherReader (either (Left . T.unpack) Right . parseSet)) (long "set" <> metavar "K=V" <> help "Narrow dependent choices to these values"))) "Describe a generator's inputs and edits"
            <> cmd "anchors" (AnchorsCmd <$> strArgument (metavar "LANGUAGE")) "List the `at` targets a language supports"
            <> cmd "where" (WhereCmd <$> strArgument (metavar "FILE") <*> strArgument (metavar "PHRASE")) "Show where an `at` phrase would put text in a file"
            <> cmd "sync" (SyncCmd <$> syncP) "Refresh owned regions (--check: report drift and write nothing; --init REGION: create a region's fences)"
            <> cmd "new" (NewCmd <$> strArgument (metavar "NAME") <*> optional (strOption (long "language" <> metavar "LANGUAGE" <> help "The language adapter (needed when the project has more than one)"))) "Write a skeleton project generator at .olifant/generators/NAME/generator.toml"
            <> cmd "schema" (pure SchemaCmd) "Print the generator.toml format as a JSON Schema"
            <> cmd "check" checkP "Check the conventions: regions up to date and rules kept (exit 1 on drift or an error); --file FILE [--content -] for one file; --report lists rules and exceptions"
            <> cmd "fix" fixP "Apply a rule's fix where it found something: the fix's generator, and the code it replaces, as one plan (--plan to preview)"
            <> cmd "doctor" (DoctorCmd <$> switch (long "run" <> help "Also run one sample test file per language, to prove its test command and report format")) "Check what generators need here (tools on this PATH, packs, the model, each language's slots on a sample of this project) and how to fix it"
            <> cmd "actions" (actionsP) "The generators that apply at a position (for editors): --at FILE:LINE:COL [--content -]"
            <> cmd "init" initP "Start a project from a pack's starter in an empty folder: init --pack PACK NAME [--dir DIR] [--plan]; or, in an existing repo, init --migrate PATH [--migrate PATH]… [--pack PACK]: strangle the legacy code under PATH; or init --language NAME [--yes] [--grammar URL] [--model NAME] [--plan]: write a language's file, its test toolchain and its queries from one model errand, proved by doctor"
            <> cmd "lsp" (pure LspCmd) "Run the language server over stdin/stdout (editors start it; every Olifant project in the editor's folders)"
            <> cmd "complete" completeP "Snippets for a completion menu at a position (for editors): --at FILE:LINE:COL [--content -]"
            <> cmd "plans" plansP "Plan errands: `plans cost [--by plan|branch] [--plan PATH] [--branch B] [--since YYYY-MM-DD] [--json]` shows what errands cost against Claude Code sessions, per plan and the PR it landed in (or per branch); `--checks` shows what the plan checker cost and found"
            <> cmd "gen" (GenCmd <$> genP) "Run generators: GEN [--set K=V]... [+ GEN [--set K=V]...]... (applies by default; --plan previews; --steps FILE|- reads the steps as JSON)"
        )
        -- hidden: the one Jev question at a line, for the prefix corpus
        <|> subparser (cmd "anticipate" anticipateP "The anticipation question at a line: PATH --line N [--overlay FILE] [--answers FILE]" <> internal)
    cmd name p desc = command name (info (Options <$> formatP <*> p) (progDesc desc))
    formatP = optional (flag' Json (long "json" <> help "JSON output") <|> flag' Human (long "human" <> help "Human-readable output"))
    -- Used only to render `gen --help`; real `gen` parsing is 'parseGen'.
    genP =
      (\target sets plan now stepsFile model noFill progress overlayF atF -> GenOptions [StepSpec target sets] stepsFile plan now model noFill progress overlayF atF Nothing)
        <$> strArgument (metavar "GENERATOR")
        <*> many (option (eitherReader setP) (long "set" <> metavar "KEY=VALUE" <> help "Set an input or override a derived value"))
        <*> switch (long "plan" <> help "Show what would change without writing anything")
        <*> optional (option (eitherReader timeP) (long "now" <> metavar "ISO8601" <> help "Pin the current time"))
        <*> optional (strOption (long "steps" <> metavar "FILE|-" <> help "Read the steps as JSON: [{\"gen\": ..., \"set\": {...}}]"))
        <*> optional (strOption (long "model" <> metavar "NAME" <> help "Fill holes with [model.backends.NAME] (default: [model] default)"))
        <*> switch (long "no-fill" <> help "Leave every hole as its placeholder")
        <*> switch (long "progress" <> help "Write progress as JSON lines on stderr (checks, fill requests, fills)")
        <*> optional (strOption (long "overlay" <> metavar "FILE" <> help "With --plan: FILE's current (unsaved) text comes from stdin"))
        <*> optional (option (eitherReader cursorP) (long "at" <> metavar "FILE:LINE:COL" <> help "Where a snippet goes (its line is replaced; 1-based)"))
    syncP = flag' SyncCheck (long "check" <> help "Report regions that drifted or are broken; write nothing") <|> (SyncInit <$> strOption (long "init" <> metavar "REGION" <> help "Create a region's fences at its init_at, then fill it")) <|> flag' SyncPlan (long "plan" <> help "Show the refresh as a plan (whole files, for editors); write nothing") <|> pure SyncApply
    setP s = either (Left . T.unpack) Right (parseSet s)
    fixP =
      (\rule (f, pos) n sets plan model noFill progress -> FixCmd (T.pack rule) f pos n sets (GenOptions [] Nothing plan Nothing (T.pack <$> model) noFill progress Nothing Nothing Nothing))
        <$> strArgument (metavar "RULE")
        <*> option (eitherReader positionP) (long "at" <> metavar "FILE:LINE:COL" <> help "Where the finding is (1-based line, UTF-8 byte column)")
        <*> option auto (long "fix" <> metavar "N" <> value 1 <> help "Which of the rule's fixes (1 = the first, the default)")
        <*> many (option (eitherReader setP) (long "set" <> metavar "K=V" <> help "An input the fix leaves open (check --json lists them as `missing`)"))
        <*> switch (long "plan" <> help "Show what would change without writing anything")
        <*> optional (strOption (long "model" <> metavar "NAME" <> help "Fill holes with [model.backends.NAME]"))
        <*> switch (long "no-fill" <> help "Leave every hole as its placeholder")
        <*> switch (long "progress" <> help "Write progress as JSON lines on stderr")
    checkP =
      CheckCmd
        <$> switch (long "report" <> help "Also list every rule's severity and every exception (allow comments, [rules] allows), stale ones too")
        <*> optional (strOption (long "file" <> metavar "FILE" <> help "Only this file's rule findings (for editors)"))
        <*> (maybe False (== ("-" :: String)) <$> optional (strOption (long "content" <> metavar "-" <> help "Read the file's current text from stdin")))
        <*> switch (long "baseline" <> help "Record today's findings in .olifant/baseline.toml: from then on only new findings fail (spec §5.10)")
        <*> switch (long "accept-new" <> help "With --baseline: also record findings that are new since the last baseline")
        <*> optional (strOption (long "changed-since" <> metavar "REF" <> help "Only findings on lines changed since REF (git), for CI on a branch"))
    -- one parser for the three forms (their options overlap): --language
    -- NAME writes a language for this project; --migrate PATH… strangles
    -- legacy code in an existing repo; else --pack PACK NAME starts a
    -- project (missing pieces are a usage error at run time)
    initP =
      ( \language model grammar yes fills migrate pack strangler name dir plan -> case (language, migrate) of
          (Just l, _) -> InitLanguageCmd (T.pack l) (T.pack <$> model) (T.pack <$> grammar) yes plan fills
          (Nothing, []) -> InitCmd (maybe "" T.pack pack) (maybe "" T.pack name) dir plan
          (Nothing, paths) -> InitMigrateCmd paths (T.pack <$> pack) (T.pack <$> strangler)
      )
        <$> optional (strOption (long "language" <> metavar "NAME" <> help "In an existing project: write .olifant/languages/NAME.toml, its test toolchain and the grammar's missing queries from one model errand"))
        <*> optional (strOption (long "model" <> metavar "NAME" <> help "With --language: ask [model.backends.NAME] (default: [model] default)"))
        <*> optional (strOption (long "grammar" <> metavar "URL" <> help "With --language: the grammar's git repository (else the model proposes one)"))
        <*> switch (long "yes" <> help "With --language: fetch and build the proposed grammar without asking")
        <*> optional (strOption (long "fills" <> metavar "FILE" <> help "With --language: recorded answers (a [fills] table) stand in for the model"))
        <*> many (strOption (long "migrate" <> metavar "PATH" <> help "In an existing repo: legacy code to strangle (repeat for more)"))
        <*> optional (strOption (long "pack" <> metavar "PACK" <> help "A pack's directory, or a name under [packs] in ~/.config/olifant/config.toml (with --migrate: the target architecture; default: the project's one configured pack)"))
        <*> optional (strOption (long "strangler" <> metavar "PACK" <> help "With --migrate: the strangler pack (one with a migrate.toml); default: the one the project, your [packs] or the target pack's folder holds"))
        <*> optional (strArgument (metavar "NAME" <> help "The project's name (and its folder, unless --dir)"))
        <*> optional (strOption (long "dir" <> metavar "DIR" <> help "Where to make it (default: ./NAME)"))
        <*> switch (long "plan" <> help "Show what it would make; write nothing")
    anticipateP =
      AnticipateCmd
        <$> strArgument (metavar "PATH")
        <*> option auto (long "line" <> metavar "N" <> help "1-based line")
        <*> optional (strOption (long "overlay" <> metavar "FILE" <> help "PATH's current (unsaved) text, read from FILE"))
        <*> optional (strOption (long "answers" <> metavar "FILE" <> help "Recorded answers instead of the model (tests)"))
    plansP =
      hsubparser
        ( command
            "cost"
            ( info
                ( PlansCostCmd
                    <$> optional (T.pack <$> strOption (long "branch" <> metavar "BRANCH" <> help "Only this branch's row (`?` for work with no branch)"))
                    <*> optional (option (eitherReader dayP) (long "since" <> metavar "YYYY-MM-DD" <> help "Only errands and messages from this day on (UTC, the day included)"))
                    <*> switch (long "json" <> help "The rows and the raw errand records as JSON")
                    <*> optional (strOption (long "transcripts" <> metavar "DIR" <> help "Claude Code transcripts (default: ~/.claude/projects/<this project's slug>)"))
                    <*> option (eitherReader byP) (long "by" <> metavar "plan|branch" <> value ByPlan <> showDefaultWith (const "plan") <> help "Group by plan and landing (PR), or by recorded branch")
                    <*> optional (strOption (long "plan" <> metavar "PATH" <> help "Only this plan's errands and reconciles"))
                    <*> switch (long "checks" <> help "What the plan checker cost and found, per plan: triage, claims, steps, automatic and asked")
                )
                (progDesc "What errands cost, against Claude Code sessions, per plan and the PR it landed in, or per branch (list prices)")
            )
        )
    byP "plan" = Right ByPlan
    byP "branch" = Right ByBranch
    byP _ = Left "expected plan or branch"
    dayP s = maybe (Left "expected a date like 2026-10-25") Right (parseTimeM False defaultTimeLocale "%Y-%m-%d" s)
    completeP =
      (\(f, pos) content -> CompleteCmd f pos content)
        <$> option (eitherReader positionP) (long "at" <> metavar "FILE:LINE:COL" <> help "1-based line, UTF-8 byte column")
        <*> (maybe False (== ("-" :: String)) <$> optional (strOption (long "content" <> metavar "-" <> help "Read the file's current text from stdin")))
    actionsP =
      (\target content -> either (\(f, pos) -> ActionsCmd f (Just pos) content) (\f -> ActionsCmd f Nothing content) target)
        <$> ( (Left <$> option (eitherReader positionP) (long "at" <> metavar "FILE:LINE:COL" <> help "1-based line, UTF-8 byte column"))
                <|> (Right <$> strOption (long "file" <> metavar "FILE" <> help "Every place in FILE where a generator applies, and near misses"))
            )
        <*> (maybe False (== ("-" :: String)) <$> optional (strOption (long "content" <> metavar "-" <> help "Read the file's current text from stdin")))
    -- FILE:LINE:COL, split from the right (a path may contain colons).
    positionP s = case reverse (splitOn ':' s) of
      (c : l : rest@(_ : _)) | [(col, "")] <- reads c, [(line, "")] <- reads l -> Right (intercalate ":" (reverse rest), (line, col))
      _ -> Left "expected FILE:LINE:COL (1-based)"
    splitOn sep xs = case break (== sep) xs of
      (a, _ : b) -> a : splitOn sep b
      (a, []) -> [a]
    timeP s = maybe (Left "expected an ISO 8601 time like 2026-09-27T15:30:00Z") Right (iso8601ParseM s)

-- | FILE:LINE:COL (1-based), split from the right: a path may contain colons.
cursorP :: String -> Either String (FilePath, Int, Int)
cursorP s = case reverse (splitColons s) of
  (c : l : rest@(_ : _)) | [(col, "")] <- reads c, [(line, "")] <- reads l -> Right (intercalate ":" (reverse rest), line, col)
  _ -> Left "--at expects FILE:LINE:COL (1-based)"
  where
    splitColons xs = case break (== ':') xs of
      (a, _ : b) -> a : splitColons b
      (a, []) -> [a]

-- | `gen` arguments: global flags anywhere; steps separated by a standalone "+".
parseGen :: [String] -> Either Text Options
parseGen args0 = do
  -- `--model NAME`, `--no-fill` and `--progress` apply to the whole run, like --plan.
  let progress = "--progress" `elem` args0
      (overlayFile, rest00) = takeOverlay (filter (/= "--progress") (concatMap splitEq args0))
      (atArg, rest01) = takeAt rest00
      (fillsArg, rest0) = takeFills rest01
  cursor <- traverse (either (Left . T.pack) Right . cursorP) atArg
  (model, noFill, args) <- pull Nothing False [] rest0
  Options fmt cmd <- go Nothing False Nothing Nothing [] [] args
  pure $ case cmd of
    GenCmd o -> Options fmt (GenCmd o {genModel = model, genNoFill = noFill, genProgress = progress, genOverlay = overlayFile, genAt = cursor, genFills = fillsArg})
    other -> Options fmt other
  where
    takeAt = \case
      "--at" : f : xs -> let (_, rest) = takeAt xs in (Just f, rest)
      x : xs -> let (o, rest) = takeAt xs in (o, x : rest)
      [] -> (Nothing, [])
    takeFills = \case
      "--fills" : f : xs -> let (_, rest) = takeFills xs in (Just f, rest)
      x : xs -> let (o, rest) = takeFills xs in (o, x : rest)
      [] -> (Nothing, [])
    takeOverlay = \case
      "--overlay" : f : xs -> let (_, rest) = takeOverlay xs in (Just f, rest)
      x : xs -> let (o, rest) = takeOverlay xs in (o, x : rest)
      [] -> (Nothing, [])
    pull model noFill acc = \case
      [] -> Right (model, noFill, reverse acc)
      "--model" : m : xs -> pull (Just (T.pack m)) noFill acc xs
      ["--model"] -> Left "--model needs a backend name"
      "--no-fill" : xs -> pull model True acc xs
      x : xs -> pull model noFill (x : acc) xs
    -- `--set=k=v` / `--now=T` / `--steps=F` are the same as the two-word forms.
    splitEq a = case break (== '=') a of
      (opt, '=' : v) | opt `elem` ["--set", "--now", "--steps", "--model"] -> [opt, v]
      _ -> [a]
    go fmt plan now stepsFile cur done = \case
      [] -> finish fmt plan now stepsFile (reverse (reverse cur : done))
      "--json" : xs -> go (Just Json) plan now stepsFile cur done xs
      "--human" : xs -> go (Just Human) plan now stepsFile cur done xs
      "--plan" : xs -> go fmt True now stepsFile cur done xs
      "--now" : t : xs -> case iso8601ParseM t of
        Just time -> go fmt plan (Just time) stepsFile cur done xs
        Nothing -> Left "--now: expected an ISO 8601 time like 2026-09-27T15:30:00Z"
      "--steps" : f : xs -> go fmt plan now (Just f) cur done xs
      "+" : xs -> go fmt plan now stepsFile [] (reverse cur : done) xs
      x : xs -> go fmt plan now stepsFile (x : cur) done xs
    finish fmt plan now stepsFile segments = do
      steps <- case stepsFile of
        Just _
          | all null segments -> Right []
          | otherwise -> Left "use either --steps or positional generator steps, not both"
        Nothing
          | all null segments -> Left "missing GENERATOR: olifant gen GENERATOR [--set KEY=VALUE]... [+ GENERATOR ...]"
          | any null segments -> Left "empty step: each `+` must be followed by a generator"
          | otherwise -> traverse step segments
      pure (Options fmt (GenCmd (GenOptions steps stepsFile plan now Nothing False False Nothing Nothing Nothing)))
    step (g : xs) = StepSpec (T.pack g) <$> sets xs
    step [] = Left "empty step"
    sets = \case
      [] -> Right []
      "--set" : kv : xs -> (:) <$> parseSet kv <*> sets xs
      x : _ -> Left ("unexpected `" <> T.pack x <> "` (expected --set KEY=VALUE)")

parseSet :: String -> Either Text (Text, Text)
parseSet s = case break (== '=') s of
  (k, '=' : v) | not (null k) -> Right (T.pack k, T.pack v)
  _ -> Left "expected KEY=VALUE"
