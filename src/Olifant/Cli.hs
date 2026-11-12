-- | The `olifant` command line: parses arguments, runs catalog/generation
-- use cases, and renders JSON or human output with the spec's exit codes.
module Olifant.Cli (Runtime (..), DoctorPorts (..), runCli, parseDiffHunks) where

import Data.Set (Set)
import Data.Set qualified as S
import Data.Ord (Down (..))
import Data.List (nub, sort, sortOn)
import Numeric (showFFloat)
import Olifant.Server.Errand (Tokens (..))
import Olifant.Server.Errand qualified as Errand
import Olifant.Server.Telemetry (ErrandRecord (..), OutcomeRecord (..), ReconcileRecord (..), Record (..), readRecords, recordJson, recordsPath)
import Olifant.Server.Telemetry qualified as Tel
import Olifant.Transcripts (Message (..), messageCost, readTranscripts, slugOf)
import Control.Applicative ((<|>))
import Control.Exception qualified as E
import Olifant.Language.Grammar (GrammarFailure (..), GrammarInfo (..), GrammarSettings, GrammarUnavailable (..), Loaded (..), checkoutOf, grammarLabel, grammarSettings, loadGrammarWith, remoteHead)
import Olifant.Language.Adapter (Grammar (..), GrammarSource (..), Layer (..), languageFromLayers, shippedLayer)
import Olifant.Language.Presets (shippedPresets)
import Olifant.LanguageInit qualified as LI
import Olifant.Library (shippedLanguages, shippedToolchains)
import Olifant.Fill (FillRequest, Filler (..), ModelFiller (..), Stance (..), parseFills, replayFiller)
import Olifant.Migration (MigrationReport (..), Switch (..), migrationJson, reportLine)
import Olifant.Rules (Exception (..), FixOffer (..), Finding (..), RuleDef (..), RulesConfig (..), checkFiles, markBaselined, ruleCovers)
import Olifant.Rules qualified as R
import Olifant.Toml qualified as Toml
import Olifant.Doctor (Check (..), DoctorPorts (..), Status (..), grammarCheck)
import Olifant.Actions (Action (..), Completion (..), FileAction (..), Nearby (..), Range (..))
import Control.Monad (filterM, forM_, join, unless, when)
import Data.Aeson (Value (..), decodeStrict, eitherDecodeStrict, encode, object, toJSON)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Aeson.Types (Pair, (.=))
import Data.Bifunctor (first)
import Data.Map.Strict qualified as M
import Data.Functor ((<&>))
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Olifant.Template (render)
import Olifant.Template qualified as Tm
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (Day, UTCTime (..), defaultTimeLocale, formatTime)
import Olifant.Anchor (Located (..), RenderedAnchor (..), describeMisses, insertLines, parsePhrase, resolveLayers)
import Olifant.Region (RegionDef, RegionDefWith (..), RegionProblem (..), applyRegion, fenceLines, wholeHeader)
import Olifant.App
import Olifant.Lsp (runLsp)
import Olifant.Catalog
import Olifant.Cli.Options
import Olifant.Output
import Olifant.Generator.Schema (generatorSchema)
import Olifant.Generation
import Olifant.Cursor (isSnippet)
import Olifant.Generator (AppliesAt (..), Generator, generatorName, EnvError (..), GeneratorId (..), GeneratorWith (..), generatorIdText)
import Olifant.Language (Checker, Language (..), Syntax, languageFor)
import Olifant.Landings (Landing (..), landingFor, landingsOf, shortSubject)
import Olifant.Process (Cancel, neverCancel)
import Olifant.Workspace (PathState (..), Workspace (..), ignoring, isSafeRelative, overlay, within)
import System.FilePath (dropTrailingPathSeparator, makeRelative, normalise, splitDirectories, takeBaseName, takeDirectory, takeFileName, (</>))
import System.Exit (ExitCode (..))
import GHC.Clock (getMonotonicTime)
import Olifant.Decide (Answer (..), Confidence (..), Decider (..), Question (..), Shape (..))
import Olifant.Recognize qualified as Rec
import Olifant.Server.Anticipate (Anticipation (..), Moment (..), anticipationAsk, anticipationFrom, minPrefixLines, prefixAt, unitLines)
import Olifant.Server.Recognize (recognitionFor)
import Olifant.Server.State (ServerConfig (..), ServerState (..), deciderFor, newServerState, openDoc, projectFor)
import Olifant.Server.Suggest (Suggestion (..))

data Runtime = Runtime
  { rtRoot :: Workspace IO
  , rtProjectDir :: FilePath
  , rtUserConfigDir :: Maybe FilePath
  , rtHomeDir :: Maybe FilePath
  , rtReadStdin :: IO Text
  , -- | one line of stdin, for a yes/no question on a terminal ("" at its end)
    rtReadLine :: IO Text
  , rtNow :: IO UTCTime
  , rtStdoutIsTty :: Bool
  , rtOut :: Text -> IO ()
  , rtErr :: Text -> IO ()
  , rtSyntax :: Syntax IO
  , rtChecker :: Cancel -> Checker IO
  , rtDoctor :: DoctorPorts IO
  , -- | lines changed since a git ref (file, 1-based inclusive ranges) and
    -- untracked files, for `check --changed-since`
    rtGitChanged :: FilePath -> Text -> IO (Either Text ([(FilePath, [(Int, Int)])], [FilePath]))
  , -- | a first commit of everything in a directory, `git init` first if
    -- needed (`olifant init`)
    rtGitInit :: FilePath -> Text -> IO (Either Text ())
  , -- | runs a hole's `verify` command (its tests) in a scratch copy
    rtVerifier :: Cancel -> Verifier IO
  , -- | runs a hole's tests (`verify = "tests"`) with the language's test runner
    -- (stopped after that many seconds)
    rtTester :: Int -> Cancel -> Runner IO
  , -- | the files git tracks under a folder (what a project already had)
    rtTracked :: FilePath -> IO (Maybe (Set FilePath))
  , -- | a file's text at the base (see 'pBaseText')
    rtBaseText :: FilePath -> Text -> FilePath -> IO (Either Text (Text, Maybe Text))
  , -- | the files at the base (see 'pBaseFiles')
    rtBaseFiles :: FilePath -> Text -> IO (Either Text [FilePath])
  , -- | what the base depends on (see 'pBaseStamp')
    rtBaseStamp :: FilePath -> Text -> IO Text
  , -- | git in a directory, its stdout (`plans cost` reads the log)
    rtGit :: FilePath -> [String] -> IO (Either Text Text)
  }

-- | A grammar that cannot load stops the command with its text (exit 1),
-- not an internal error.
runCli :: Runtime -> [String] -> IO ExitCode
runCli rt args =
  runOptions rt args `E.catch` \(GrammarUnavailable t) -> rtErr rt ("olifant: " <> t <> "\n") >> pure (ExitFailure 1)

runOptions :: Runtime -> [String] -> IO ExitCode
runOptions rt args = case parseOptions args of
  Left (text, ExitSuccess) -> rtOut rt text >> pure ExitSuccess
  Left (text, _)
    | wantsJson -> rtOut rt (encodeJson (errorJson "usage" (T.strip text) []) <> "\n") >> pure (ExitFailure 2)
    | otherwise -> rtErr rt text >> pure (ExitFailure 2)
  -- `init` makes a project: there is none to open yet
  -- the language server finds its projects itself, file by file
  Right opts | LspCmd <- optCommand opts -> (\code -> if code == 0 then ExitSuccess else ExitFailure code) <$> runLsp (portsOf rt)
  Right opts | InitMigrateCmd paths pack strangler <- optCommand opts ->
    runInitMigrate rt (fromMaybe (if rtStdoutIsTty rt then Human else Json) (optFormat opts)) paths pack strangler
  Right opts | InitCmd pack name _ _ <- optCommand opts, T.null pack || T.null name ->
    failWith rt (fromMaybe (if rtStdoutIsTty rt then Human else Json) (optFormat opts)) 2 "usage" "init needs --pack PACK NAME (a new project), or --migrate PATH (an existing repo)" Nothing []
  Right opts | InitCmd pack name dir plan <- optCommand opts ->
    runInit rt (fromMaybe (if rtStdoutIsTty rt then Human else Json) (optFormat opts)) pack name dir plan
  Right opts -> do
    let fmt = fromMaybe (if rtStdoutIsTty rt then Human else Json) (optFormat opts)
        cwd = rtProjectDir rt
    root <- projectRoot (rtRoot rt) (rtHomeDir rt) cwd
    let rt' = rt {rtProjectDir = root}
    openProjectCatalog (rtRoot rt') (CatalogPaths root (rtUserConfigDir rt') (rtHomeDir rt')) >>= \case
      Left err -> catalogFailure rt' fmt err
      Right cat -> runCommand rt' cwd fmt cat (optCommand opts)
  where
    wantsJson = "--json" `elem` args || (not (rtStdoutIsTty rt) && "--human" `notElem` args)

-- | `olifant init` (north-star project 2 §1): a project from a pack's starter,
-- in an empty folder. The pack is a directory, or a name under [packs] in
-- the user config; the user's [model] comes along; regions are fenced; a
-- first commit is made; the starter's `next` lines say what to run.
-- | `init --migrate PATH…` in an existing repo: the target pack and the
-- strangler pack, `[migrate] legacy`, `.olifant/state/` ignored, and today's
-- findings baselined (nothing fails on day one). A strangler pack is one
-- with a `migrate.toml`. The target is `--pack`, else the project's one
-- configured pack that is not a strangler; the strangler is `--strangler`,
-- else the one strangler pack the project configures, else the one under
-- the user's [packs], else the one beside the target pack's folder. Two
-- candidates (or none) are a usage error that names them.
runInitMigrate :: Runtime -> Format -> [FilePath] -> Maybe Text -> Maybe Text -> IO ExitCode
runInitMigrate rt fmt paths packArg stranglerArg = do
  let cwd = rtProjectDir rt
      ws = rtRoot rt
      configPath = cwd </> ".olifant" </> "config.toml"
  existing <- readText ws configPath
  userConfig <- maybe (pure Nothing) (\u -> readText ws (u </> "config.toml")) (rtUserConfigDir rt)
  let userPacks = [(n, normalise (fromMaybe "." (rtUserConfigDir rt) </> p)) | Just src <- [userConfig], Right ps <- [packsFromConfig src], (n, p) <- ps]
      projectPacks = [(n, normalise (cwd </> p)) | Just src <- [existing], Right ps <- [packsFromConfig src], (n, p) <- ps]
      resolve p = do
        let asPath = normalise (cwd </> T.unpack p)
        isDir <- (== Directory) <$> inspectPath ws asPath
        pure (if isDir then Just (T.pack (takeFileName asPath), asPath) else (p,) <$> lookup p (projectPacks ++ userPacks))
      isStrangler (_, dir) = (/= Nothing) <$> readText ws (dir </> "migrate.toml")
      names = T.intercalate ", " . map fst
      one what flag found = case found of
        [x] -> Right x
        [] -> Left ("no " <> what <> ": give " <> flag)
        several -> Left ("several " <> what <> "s (" <> names several <> "): give " <> flag)
  projectStranglers <- filterM isStrangler projectPacks
  target <- case packArg of
    Just p -> maybe (Left ("no pack `" <> p <> "`")) Right <$> resolve p
    Nothing -> pure (one "target pack (one that is not a strangler) in the project's [packs]" "--pack" [p | p <- projectPacks, p `notElem` projectStranglers])
  strangler <- case (stranglerArg, target) of
    (Just p, _) ->
      resolve p >>= \case
        Nothing -> pure (Left ("no pack `" <> p <> "`"))
        Just found -> isStrangler found <&> \yes -> if yes then Right found else Left ("pack `" <> p <> "` is no strangler pack (it has no migrate.toml)")
    (Nothing, _) | not (null projectStranglers) -> pure (one "strangler pack" "--strangler" projectStranglers)
    (Nothing, t) -> do
      users <- filterM isStrangler userPacks
      case (users, t) of
        (_ : _, _) -> pure (one "strangler pack" "--strangler" users)
        ([], Right (_, dir)) -> do
          let parent = takeDirectory (dropTrailingPathSeparator dir)
          beside <- listFiles ws parent
          pure (one "strangler pack" "--strangler" [(T.pack d, parent </> d) | f <- sort beside, [d, "migrate.toml"] <- [splitDirectories f]])
        ([], Left _) -> pure (Left "no strangler pack: give --strangler")
  let hint = Just "give a pack's directory, or add it under [packs] in ~/.config/olifant/config.toml"
  case (existing, target, strangler) of
    (Just c, _, _) | "[migrate]" `T.isInfixOf` c -> failWith rt fmt 2 "usage" "already migrating: edit [migrate] in .olifant/config.toml" Nothing []
    (_, Left why, _) -> failWith rt fmt 2 "usage" why hint []
    (_, _, Left why) -> failWith rt fmt 2 "usage" why hint []
    (_, Right t, Right s) -> do
      let packLine (n, p) = n <> " = { path = \"" <> T.pack p <> "\" }\n"
          migrate = "[migrate]\nlegacy = [" <> T.intercalate ", " ["\"" <> T.pack (dropTrailingPathSeparator p) <> "/**\"" | p <- paths] <> "]\n"
          withPack c (n, p)
            | (n <> " = ") `T.isInfixOf` c = c
            | "[packs]\n" `T.isInfixOf` c = T.replace "[packs]\n" ("[packs]\n" <> packLine (n, p)) c
            | otherwise = c <> "\n[packs]\n" <> packLine (n, p)
          config = case existing of
            Nothing -> "[packs]\n" <> packLine t <> packLine s <> "\n" <> migrate
            Just c -> withPack (withPack c t) s <> (if "\n" `T.isSuffixOf` c then "\n" else "\n\n") <> migrate
      writeText ws configPath config
      -- the language server's local state stays out of git
      let project = within cwd ws
      ignored <- readText project ".gitignore"
      case ignored of
        Nothing -> writeText project ".gitignore" ".olifant/state/\n"
        Just g
          | ".olifant/state/" `elem` map T.strip (T.lines g) -> pure ()
          | otherwise -> writeText project ".gitignore" (g <> (if T.null g || "\n" `T.isSuffixOf` g then "" else "\n") <> ".olifant/state/\n")
      -- today's debt, recorded: nothing fails on day one
      code <- runCli rt (["check", "--baseline"] ++ (if fmt == Human then ["--human"] else ["--json"]))
      when (fmt == Human) $
        rtOut rt ("migrating " <> T.intercalate ", " (map T.pack paths) <> ": legacy is bug-fix-only; new work goes to the new architecture\nnext: olifant check, and open the project in your editor\n")
      pure code

runInit :: Runtime -> Format -> Text -> Text -> Maybe FilePath -> Bool -> IO ExitCode
runInit rt fmt pack name dirArg plan = do
  let cwd = rtProjectDir rt
      ws = rtRoot rt
      dir = normalise (cwd </> fromMaybe (T.unpack name) dirArg)
      shown = T.pack (makeRelative cwd dir)
  -- a folder holding only .git (a person ran `git init` first) counts as empty
  present <- filter (notElem ".git" . splitDirectories) <$> listFiles ws dir
  userConfig <- maybe (pure Nothing) (\u -> readText ws (u </> "config.toml")) (rtUserConfigDir rt)
  let userPacks = [(n, normalise (fromMaybe "." (rtUserConfigDir rt) </> p)) | Just src <- [userConfig], Right ps <- [packsFromConfig src], (n, p) <- ps]
      asPath = normalise (cwd </> T.unpack pack)
  isDir <- (== Directory) <$> inspectPath ws asPath
  let resolved
        | isDir = Just (T.pack (takeFileName asPath), asPath)
        | otherwise = (pack,) <$> lookup pack userPacks
  case (present, resolved) of
    (_ : _, _) -> failWith rt fmt 2 "usage" (shown <> " is not empty (" <> T.intercalate ", " (map T.pack (take 3 present)) <> (if length present > 3 then ", …" else "") <> ")") (Just "init needs an empty folder (or one holding only .git)") []
    (_, Nothing) -> failWith rt fmt 2 "usage" ("no pack `" <> pack <> "`" <> (if null userPacks then "" else " (known: " <> T.intercalate ", " (map fst userPacks) <> ")")) (Just "give a pack's directory, or add it under [packs] in ~/.config/olifant/config.toml") []
    ([], Just (packName, packPath)) -> do
      let config = "[packs]\n" <> packName <> " = { path = \"" <> T.pack packPath <> "\" }\n" <> maybe "" modelTables userConfig
          rt' = rt {rtProjectDir = dir}
      -- the starter is found before anything is written: a refused init
      -- leaves the folder as it was
      openPackCatalog ws [(packName, packPath)] >>= \case
        Left err -> catalogFailure rt fmt err
        Right packOnly -> case [e | e <- catalogEntries packOnly, entryPack e == Just packName, genStarter (entryDefinition e)] of
          [] -> failWith rt fmt 2 "usage" ("pack " <> packName <> " has no starter generator (starter = true)") Nothing []
          entry : more@(_ : _) -> failWith rt fmt 2 "usage" ("pack " <> packName <> " has several starters: " <> T.intercalate ", " (map (generatorIdText . entryId) (entry : more))) Nothing []
          [entry] -> do
            unless plan (writeText ws (dir </> ".olifant" </> "config.toml") config)
            -- the project's own catalog: the user's model, the pack's regions
            opened <- if plan then pure (Right packOnly) else openProjectCatalog ws (CatalogPaths dir (rtUserConfigDir rt) (rtHomeDir rt))
            case opened of
              Left err -> catalogFailure rt fmt err
              Right cat ->
                loadGenerator (rtRoot rt) (rtSyntax rt) cat entry >>= \case
                  Left err -> catalogFailure rt fmt err
                  Right gen -> do
                    when (plan && fmt == Human) (rtOut rt ("would write " <> T.pack (makeRelative cwd (dir </> ".olifant" </> "config.toml")) <> " (pack " <> packName <> ")\n"))
                    -- JSON callers get one document: the plan, held until the
                    -- first commit's outcome can join its warnings
                    held <- newIORef ""
                    let rtRun = if fmt == Json && not plan then rt' {rtOut = \t -> modifyIORef' held (<> t)} else rt'
                        release extra = do
                          out <- readIORef held
                          unless (T.null out) $ rtOut rt $ case (extra, decodeStrict (TE.encodeUtf8 out)) of
                            (Just w, Just (Object o)) ->
                              let ws' = case KM.lookup "warnings" o of
                                    Just (Array a) -> Array (a <> pure (String w))
                                    _ -> Array (pure (String w))
                               in TE.decodeUtf8 (BL.toStrict (encode (Object (KM.insert "warnings" ws' o)))) <> "\n"
                            _ -> out
                    code <- runSteps rtRun dir fmt cat (GenOptions [] Nothing plan Nothing Nothing False False Nothing Nothing Nothing) [] [Step gen (M.fromList [("name", name)])]
                    if code /= ExitSuccess || plan
                      then release Nothing >> pure code
                      else do
                        -- every region whose files the starter wrote gets its fences
                        let project = within dir ws
                            tools = projectTools (portsOf rt') (projectOf rt' cat)
                        universe <- listFiles project "."
                        forM_ (catalogRegions cat) $ \def ->
                          forM_ (regionInstances universe def) (initFences project tools)
                        -- the language server's local state (recognition's quietness) stays out of git
                        ignored <- readText project ".gitignore"
                        case ignored of
                          Nothing -> writeText project ".gitignore" ".olifant/state/\n"
                          Just t
                            | ".olifant/state/" `elem` map T.strip (T.lines t) -> pure ()
                            | otherwise -> writeText project ".gitignore" (t <> (if T.null t || "\n" `T.isSuffixOf` t then "" else "\n") <> ".olifant/state/\n")
                        committed <- rtGitInit rt dir ("olifant init: " <> packName)
                        release (either (\why -> Just ("no first commit: " <> why)) (const Nothing) committed)
                        let nexts = [t | tm <- genNext gen, Right t <- [render (M.fromList [("name", Tm.VText name)]) tm]]
                        when (fmt == Human) $ do
                          either (\why -> rtErr rt ("note: no first commit: " <> why <> "\n")) (const (pure ())) committed
                          rtOut rt (T.unlines (("\n" <> shown <> " is ready. Next:") : ["  cd " <> shown] ++ map ("  " <>) nexts))
                        pure ExitSuccess
  where
    -- the user config's [model] tables, as written
    modelTables src =
      let keep (inModel, acc) l
            | "[" `T.isPrefixOf` T.stripStart l = let m = "[model" `T.isPrefixOf` T.stripStart l in (m, if m then acc ++ [l] else acc)
            | otherwise = (inModel, if inModel then acc ++ [l] else acc)
          ls = snd (foldl keep (False, []) (T.lines src))
          -- copied only when it names the model to use
          hasDefault = any (\l -> "default" `T.isPrefixOf` T.stripStart l && "=" `T.isInfixOf` l) ls
       in if null ls || not hasDefault then "" else "\n" <> T.unlines ls

-- | Where to look when a generator.toml does not load.
formatHint :: Text
formatHint = "`olifant schema` prints the generator.toml format; `olifant new NAME` writes a skeleton that loads"

-- | A generator that loads as written: one ident input, a derived PascalCase
-- name, and the three kinds of edit as commented examples to adapt.
skeletonGenerator :: Text -> Text -> Text
skeletonGenerator name lang =
  T.unlines
    [ "# Run with: olifant gen " <> name <> " --set name=example   (--plan to preview)"
    , "# Format: `olifant schema`. Phrases for `at`: `olifant anchors " <> lang <> "`; test one: `olifant where FILE \"PHRASE\"`."
    , "description = \"One line saying what this adds\""
    , "language = \"" <> lang <> "\""
    , "version = 1"
    , "examples = [\"olifant gen " <> name <> " --set name=example\"]"
    , ""
    , "[inputs.name]"
    , "type = \"ident\""
    , "prompt = \"The name, snake_case (e.g. contact_us)\""
    , ""
    , "[derive]"
    , "P = \"{= name|pascal =}\""
    , ""
    , "# Each edit is one [[edits]] table (double brackets). Uncomment and adapt."
    , "# Template values: {= name =}, {= P =}; filters after a bar: {= name|kebab =}."
    , "#"
    , "# [[edits]]                          # a new file"
    , "# create = \"src/{= P =}.txt\""
    , "# text   = \"{= P =}\\n\""
    , "#"
    , "# [[edits]]                          # text into an existing file"
    , "# insert = \"src/Main.txt\""
    , "# at     = \"end of type Route\""
    , "# text   = \"| {= P =}\""
    , "#"
    , "# [[edits]]                          # case branches the compiler asks for"
    , "# complete = \"Route.{= P =}\""
    , "# where    = \"case in changeRouteTo\""
    , "# text     = \"\"\""
    , "# {= pattern =} ->"
    , "#     ( model, Cmd.none )\"\"\""
    ]

-- | @cwd@ is where Olifant was run: file arguments (`where FILE`, `--steps
-- FILE`) are relative to it, while generators work in the project root.
runCommand :: Runtime -> FilePath -> Format -> Catalog -> Command -> IO ExitCode
runCommand rt cwd fmt cat = \case
  -- handled before a project is opened (runCli); here for completeness
  InitCmd pack name dir plan -> runInit rt fmt pack name dir plan
  InitMigrateCmd paths pack strangler -> runInitMigrate rt fmt paths pack strangler
  InitLanguageCmd name model grammar yes plan fills -> runInitLanguage rt cwd fmt cat name model grammar yes plan fills
  LspCmd -> (\code -> if code == 0 then ExitSuccess else ExitFailure code) <$> runLsp (portsOf rt)
  AnticipateCmd file line overlayFile answers -> runAnticipate rt (normalise (cwd </> file)) line ((cwd </>) <$> overlayFile) ((cwd </>) <$> answers)
  ListCmd detail -> do
    (entries, broken, views) <- listGenerators (portsOf rt) (projectOf rt cat) detail
    emit rt fmt (listJson detail (subsOf cat) views entries broken) (listHuman detail (subsOf cat) views entries broken)
    pure ExitSuccess
  ActionsCmd file at fromStdin -> do
    -- Paths are relative to where olifant runs; matching uses the
    -- project-relative path.
    let project = within (rtProjectDir rt) (rtRoot rt)
        rel = makeRelative (rtProjectDir rt) (normalise (cwd </> file))
    content <- if fromStdin then Just <$> rtReadStdin rt else readText (rtRoot rt) (cwd </> file)
    let here = (projectOf rt cat) {prWorkspace = maybe project (\t -> overlay rel t project) content}
        gens = generatorsOf here
        nameOf gid = maybe gid generatorName (lookup gid gens)
        -- how an editor may offer it: unasked (suggest), as a snippet, on a trigger (spec §4.8, §4.9)
        suggestOf gid =
          let g = lookup gid gens
              ap = genAppliesAt =<< g
           in ["suggest" .= maybe False apSuggest ap, "snippet" .= maybe False isSnippet g, "trigger" .= maybe False (isJust . apTrigger) ap]
        human a = actLabel a <> "  (" <> actGenerator a <> ")" <> (if null (actMissing a) then "" else "  asks: " <> T.intercalate ", " (actMissing a))
    case at of
      Just pos -> do
        acts <- actionsAt' (portsOf rt) here rel pos
        emit rt fmt (toJSON (map (actionJson suggestOf nameOf) acts)) (T.unlines (map human acts))
      Nothing -> do
        (acts, near) <- actionsIn (portsOf rt) here rel
        let where' r = maybe "file  " (\(Range (l, c) _) -> T.pack (show l) <> ":" <> T.pack (show c) <> "  ") r
        emit rt fmt (fileActionsJson gens acts near) (T.unlines ([where' r <> human a | FileAction a r <- acts] ++ ["nearby: " <> nbGenerator n <> " — " <> nbWhy n | n <- near]))
    pure ExitSuccess
  CompleteCmd file pos fromStdin -> do
    let project = within (rtProjectDir rt) (rtRoot rt)
        rel = makeRelative (rtProjectDir rt) (normalise (cwd </> file))
    content <- if fromStdin then Just <$> rtReadStdin rt else readText (rtRoot rt) (cwd </> file)
    let here = (projectOf rt cat) {prWorkspace = maybe project (\t -> overlay rel t project) content}
        described gid = maybe ("", gid) (\g -> (genDescription g, generatorName g)) (lookup gid (generatorsOf here))
    items <- completeAt (portsOf rt) here rel pos
    let json c = object ["generator" .= cpGenerator c, "name" .= snd (described (cpGenerator c)), "label" .= cpLabel c, "sets" .= M.fromList (cpSets c), "detail" .= fst (described (cpGenerator c))]
    emit rt fmt (toJSON (map json items)) (T.unlines ["⚡ " <> cpLabel c <> "  (" <> cpGenerator c <> ")" | c <- items])
    pure ExitSuccess
  CheckCmd report file fromStdin baseline acceptNew changedSince -> do
    let ws = within (rtProjectDir rt) (rtRoot rt)
        baselinePath = ".olifant/baseline.toml"
        key f = (fRule f, T.pack (fFile f), fFingerprint f)
    known <- fmap (maybe [] (either (const []) id . readBaseline)) (readText ws baselinePath)
    -- Each key covers as many findings as its count (review I9).
    let baselinedIn fs = [f | (f, True) <- markBaselined known fs]
        rules = catalogRules cat
        nameOf gid = maybe gid (generatorName . entryDefinition) (find ((== gid) . generatorIdText . entryId) (catalogEntries cat))
        sevText = \case R.Hint -> "hint" :: Text; R.Warn -> "warn"; R.Error -> "error"; R.Off -> "off"
        offerJson o = object ["generator" .= foGenerator o, "name" .= nameOf (foGenerator o), "label" .= fromMaybe (nameOf (foGenerator o)) (foLabel o), "sets" .= M.fromList (foSets o), "missing" .= map fst (foChoices o), "choices" .= M.fromList (foChoices o)]
        findingJson olds f =
          object
            [ "rule" .= fRule f, "file" .= fFile f, "severity" .= sevText (fSeverity f), "message" .= fMessage f
            , "range" .= fmap (\((l1, c1), (l2, c2)) -> object ["start" .= [l1, c1], "end" .= [l2, c2]]) (fRange f)
            , "fixes" .= map offerJson (fFixes f)
            , -- the first fix, as before (one-fix clients)
              "fix" .= fmap foGenerator (listToMaybe (fFixes f)), "fix_name" .= fmap (nameOf . foGenerator) (listToMaybe (fFixes f)), "sets" .= maybe M.empty (M.fromList . foSets) (listToMaybe (fFixes f))
            , "baselined" .= (f `elem` olds) ]
        exceptionJson e = object ["rule" .= exRule e, "where" .= exWhere e, "reason" .= exReason e, "stale" .= exStale e]
        reportJson fs = ["rules" .= [object ["id" .= ruleId r, "severity" .= sevText (fromMaybe (ruleSeverity r) (lookup (ruleId r) (rcSeverity (catalogRulesConfig cat)))), "debt" .= length [f | f <- baselinedIn fs, fRule f == ruleId r]] | r <- rules] | report]
        mark = \case R.Error -> "✗" :: Text; R.Warn -> "!"; _ -> "·"
        findingHuman olds f = (if f `elem` olds then "·" else mark (fSeverity f)) <> " " <> T.pack (fFile f) <> maybe "" (\((l, _), _) -> ":" <> T.pack (show l)) (fRange f) <> "  " <> fRule f <> " — " <> fMessage f <> (if null (fFixes f) then "" else "  (fixes: " <> T.intercalate "; " [fromMaybe (nameOf (foGenerator o)) (foLabel o) | o <- fFixes f] <> ")") <> (if f `elem` olds then "  (existing)" else "")
        exceptionHuman e = "  " <> exRule e <> " at " <> exWhere e <> ": " <> fromMaybe "(no reason)" (exReason e) <> (if exStale e then "  [stale: suppresses nothing]" else "")
    paths <- listFiles ws "."
    now <- rtNow rt
    let chooseInputs = chooseFixInputs (portsOf rt) (projectOf rt cat) paths now
    case file of
      Just f -> do
        let rel = makeRelative (rtProjectDir rt) (normalise (cwd </> f))
        content <- if fromStdin then Just <$> rtReadStdin rt else readText (rtRoot rt) (cwd </> f)
        (fs, exs) <- findings (portsOf rt) (projectOf rt cat) {prWorkspace = maybe ws (\t -> overlay rel t ws) content} [rel]
        let olds = baselinedIn fs
        emit rt fmt (object (["findings" .= map (findingJson olds) fs, "exceptions" .= map exceptionJson exs] ++ reportJson fs)) (T.unlines (map (findingHuman olds) fs))
        pure ExitSuccess
      Nothing | baseline -> do
        let checked = [p | p <- paths, any (\r -> isJust (ruleCovers (legacyOf (projectOf rt cat)) r p)) rules]
        (fs, _) <- checkFiles (rtSyntax rt) (catalogLanguages cat) (catalogRulesConfig cat) (legacyOf (projectOf rt cat)) (\f -> readText ws f >>= newnessOf (portsOf rt) (projectOf rt cat) f . fromMaybe "") rules paths (readText ws) checked
        -- Debt only goes down: once a baseline exists, re-baselining keeps
        -- at most each key's old count, unless --accept-new (review I9).
        let current = M.fromListWith (+) [(key f, 1 :: Int) | f <- fs]
            old = M.fromListWith (+) known
            kept
              | null known || acceptNew = current
              | otherwise = M.intersectionWith min current old
            refused = sum (M.elems current) - sum (M.elems kept)
            entry ((r, f, fp), n) = T.unlines (["[[baseline]]", "rule = " <> quote r, "file = " <> quote f, "fingerprint = " <> quote fp] ++ ["count = " <> T.pack (show n) | n > 1] ++ [""])
            quote t = "\"" <> T.replace "\"" "\\\"" (T.replace "\\" "\\\\" t) <> "\""
            header = "# Findings recorded by `olifant check --baseline` (spec §5.10): known debt.\n# check fails only on findings not listed here. Re-running --baseline only removes\n# paid debt; new findings are added only with --accept-new.\n\n"
            total = sum (M.elems kept)
            plural n w = T.pack (show n) <> " " <> w <> (if n == 1 then "" else "s")
        writeText ws baselinePath (header <> T.concat (map entry (M.toList kept)))
        emit rt fmt (object ["baselined" .= total, "refused" .= refused, "file" .= baselinePath])
          ("baselined " <> plural total "finding" <> " in " <> T.pack baselinePath <> "\n" <> (if refused > 0 then plural refused "new finding" <> " not added (fix it, or --accept-new)\n" else ""))
        pure ExitSuccess
      Nothing -> do
        (edits, conflicts, _) <- case changedSince of
          Just _ -> pure ([], [], M.empty) -- a findings run on the branch's changes
          Nothing -> refreshRegions ReportUnset (projectTools (portsOf rt) (projectOf rt cat)) ws 1 M.empty
        changed <- traverse (rtGitChanged rt (rtProjectDir rt)) changedSince
        let checked = [p | p <- paths, any (\r -> isJust (ruleCovers (legacyOf (projectOf rt cat)) r p)) rules]
            touched f = case changed of
              Nothing -> True
              Just (Left _) -> True
              Just (Right (ranges, untracked)) ->
                fFile f `elem` untracked
                  || case (lookup (fFile f) ranges, fRange f) of
                    (Nothing, _) -> False
                    (Just _, Nothing) -> True
                    (Just rs, Just ((l1, _), (l2, _))) -> any (\(a, b) -> a <= l2 && l1 <= b) rs
        mig <- migrationFor (portsOf rt) (projectOf rt cat)
        (fs0, exs) <- checkFiles (rtSyntax rt) (catalogLanguages cat) (catalogRulesConfig cat) (legacyOf (projectOf rt cat)) (\f -> readText ws f >>= newnessOf (portsOf rt) (projectOf rt cat) f . fromMaybe "") rules paths (readText ws) checked >>= chooseInputs
        let fs = filter touched fs0
            drift = [(editPath e, r) | e <- edits, WillRefresh r _ _ _ <- [editAction e]]
            regionsJson = [object ["file" .= p, "region" .= r, "status" .= ("drift" :: Text)] | (p, r) <- drift] ++ [object ["status" .= ("broken" :: Text), "message" .= describeConflict c] | c <- conflicts]
            broken = [T.pack p <> ": " <> why | (p, why) <- catalogBrokenRules cat ++ catalogBrokenRegions cat]
            gitProblem = [why | Just (Left why) <- [changed]]
            olds = baselinedIn fs
            stale = [e | e <- exs, exStale e]
            failed = not (null drift) || not (null conflicts) || not (null broken) || not (null gitProblem) || any (\f -> fSeverity f == R.Error && f `notElem` olds) fs
            human =
              T.unlines $
                ["region " <> r <> " out of date in " <> T.pack p <> " (run olifant sync)" | (p, r) <- drift]
                  ++ map describeConflict conflicts
                  ++ ["git: " <> g | g <- gitProblem]
                  ++ ["✗ " <> b | b <- broken]
                  ++ map (findingHuman olds) fs
                  ++ ["! stale allow: " <> exRule e <> " at " <> exWhere e <> " — it suppresses nothing; remove it" | not report, e <- stale]
                  ++ (if report then "rules:" : ["  " <> ruleId r <> ": " <> sevText (fromMaybe (ruleSeverity r) (lookup (ruleId r) (rcSeverity (catalogRulesConfig cat)))) | r <- rules] else [])
                  ++ (if report then "exceptions:" : map exceptionHuman exs else [])
                  ++ ["debt: " <> ruleId r <> " " <> T.pack (show n) <> " existing" | report, r <- rules, let n = length [f | f <- olds, fRule f == ruleId r], n > 0]
                  ++ (if report then concat [("switches:" : ["  " <> swName s <> ": " <> (if swOn s then "on" else "off") | s <- mrSwitches m]) ++ ["  " <> u <> ": not in the registry" | u <- mrUnregistered m] | Just m <- [mig]] else [])
                  ++ ["migration: " <> reportLine m | Just m <- [mig]]
                  ++ [ if failed
                         then "check failed"
                         else
                           let n = length fs
                               old = length olds
                            in "conventions hold (" <> T.pack (show n) <> " finding" <> (if n == 1 then "" else "s") <> ", " <> (if old > 0 then T.pack (show old) <> " existing" else "none blocking") <> ")"
                     ]
        emit rt fmt (object (["regions" .= regionsJson, "findings" .= map (findingJson olds) fs, "exceptions" .= map exceptionJson exs, "errors" .= (broken ++ gitProblem)] ++ ["migration" .= migrationJson m | Just m <- [mig]] ++ reportJson fs)) human
        pure (if failed then ExitFailure 1 else ExitSuccess)
  DoctorCmd run -> do
    checks <- doctorChecks (portsOf rt) (projectOf rt cat) run
    let mark = \case Ok -> "✓" :: Text; Warn -> "!"; Bad -> "✗"
    emit rt fmt (doctorJson checks) (T.unlines (concat [[mark (checkStatus c) <> " " <> checkName c <> " — " <> checkDetail c] ++ ["  fix: " <> f | Just f <- [checkFix c]] | c <- checks]))
    pure (if any ((== Bad) . checkStatus) checks then ExitFailure 1 else ExitSuccess)
  SchemaCmd -> do
    emit rt fmt generatorSchema (encodeJson generatorSchema <> "\n")
    pure ExitSuccess
  NewCmd name langFlag -> do
    let path = ".olifant" </> "generators" </> T.unpack name </> "generator.toml"
        ws = within (rtProjectDir rt) (rtRoot rt)
        language = case (langFlag, map langName (catalogLanguages cat)) of
          (Just l, _) -> Right l
          (Nothing, [l]) -> Right l
          (Nothing, ls) -> Left ("pass --language (" <> (if null ls then "no language adapter is configured" else "adapters: " <> T.intercalate ", " ls) <> ")")
    exists <- fileExists ws path
    case language of
      Left why -> failWith rt fmt 2 "usage" why Nothing []
      Right _ | exists -> failWith rt fmt 1 "already_exists" (T.pack path <> " already exists") (Just ("edit it, or pick another name; `olifant show " <> name <> "` shows what it does")) []
      Right lang -> do
        writeText ws path (skeletonGenerator name lang)
        emit rt fmt (object ["created" .= path, "next" .= nextSteps lang]) ("created " <> T.pack path <> "\n" <> nextSteps lang <> "\n")
        pure ExitSuccess
    where
      nextSteps lang = "fill in description and [[edits]]; `olifant anchors " <> lang <> "` lists placement phrases, `olifant where FILE \"PHRASE\"` tests one, `olifant gen " <> name <> " --set name=example --plan` previews"
  ShowCmd ref setList ->
    showGenerator (portsOf rt) (projectOf rt cat) ref (M.fromList setList) >>= \case
      Left (ShowCatalog err) -> catalogFailure rt fmt err
      Left (ShowInvalid why) -> failWith rt fmt 1 "generator_invalid" why Nothing []
      Right (e, vs) -> emit rt fmt (showJson (M.fromList vs) (subsOf cat e) e) (showHuman (M.fromList vs) (subsOf cat e) e) >> pure ExitSuccess
  AnchorsCmd name -> case find ((== name) . langName) (catalogLanguages cat) of
    Nothing ->
      failWith rt fmt 3 "language_not_found" ("no language adapter named `" <> name <> "`" <> T.concat ["; " <> T.pack path <> " failed to load: " <> why | (path, why) <- catalogBrokenLanguages cat, takeBaseName path == T.unpack name]) (Just "packs define languages in languages/<name>.toml") ["available" .= map langName (catalogLanguages cat)]
    Just lang -> emit rt fmt (anchorsJson lang) (anchorsHuman lang) >> pure ExitSuccess
  WhereCmd file phrase -> case parsePhrase phrase of
    Left err -> usageIn rt fmt err
    Right _ ->
      readText (rtRoot rt) (cwd </> file) >>= \case
        Nothing -> failWith rt fmt 3 "file_not_found" ("cannot read " <> T.pack file) Nothing []
        Just content ->
          resolveLayers (rtSyntax rt) (languageFor (catalogLanguages cat) file) (Just (cwd </> file)) [PhraseAnchor phrase] content >>= \case
            Left misses -> failWith rt fmt 5 "anchor_unresolved" (describeMisses misses) (Just "`olifant anchors <language>` lists the targets") []
            Right loc -> emit rt fmt (whereJson file phrase loc content) (whereHuman file loc content) >> pure ExitSuccess
  SyncCmd mode -> do
    now <- rtNow rt
    let ws = within (rtProjectDir rt) (rtRoot rt)
        tools = projectTools (portsOf rt) (projectOf rt cat)
    case mode of
      SyncApply -> syncRegions rt fmt (catalogBrokenRegions cat) ws tools SkipUnset SyncWrite [] now
      SyncCheck -> syncRegions rt fmt (catalogBrokenRegions cat) ws tools ReportUnset SyncReport [] now
      SyncPlan -> syncRegions rt fmt (catalogBrokenRegions cat) ws tools SkipUnset SyncShow [] now
      SyncInit name -> case find ((== name) . regionName) (catalogRegions cat) of
        Nothing -> failWith rt fmt 3 "region_not_found" ("no region named `" <> name <> "`") (Just "regions are declared in regions.toml") ["available" .= map regionName (catalogRegions cat)]
        Just def -> initRegion rt fmt ws tools def now
  GenCmd g
    | genStepsFile g == Just "-", Just _ <- genOverlay g ->
        usageIn rt fmt "--steps - and --overlay both read stdin: pass the steps as a file (--steps FILE)"
  GenCmd g -> do
    let usage = usageIn rt fmt
    specs <- case genStepsFile g of
      Nothing -> pure (Right (Right (genSteps g)))
      Just f -> do
        raw <- if f == "-" then Just <$> rtReadStdin rt else readText (rtRoot rt) (cwd </> f)
        pure $ case raw of
          Nothing -> Left f
          Just text -> Right (first (\e -> "--steps: " <> T.pack e) (eitherDecodeStrict (TE.encodeUtf8 text)))
    case specs of
      Left f -> failWith rt fmt 3 "steps_not_found" ("--steps: cannot read " <> T.pack f <> " (missing, or not UTF-8 text)") Nothing []
      Right (Left msg) -> usage msg
      Right (Right []) -> usage "no steps to run"
      Right (Right ss) -> do
        let label i s = specGenerator s <> if length ss > 1 then " (step " <> T.pack (show i) <> ")" else ""
        loaded <- traverse (\(i, s) -> fmap (first (label i s,)) (either (pure . Left) (loadGenerator (rtRoot rt) (rtSyntax rt) cat) (findEntry cat (specGenerator s)))) (zip [1 :: Int ..] ss)
        case sequence loaded of
          Left (step, err) -> catalogFailureAt rt fmt (if length ss > 1 then Just step else Nothing) err
          -- choices_from is resolved inside generate, step by step, so a
          -- step can choose what an earlier step of the chain creates.
          Right gens0 -> placeCursor rt cwd cat g gens0 >>= \case
            Left why -> failWith rt fmt 2 "usage" why Nothing []
            Right gens -> runStepsWith [] (any isSnippet gens0) rt cwd fmt cat g [] (zipWith (\gen s -> Step gen (M.fromList (specSets s))) gens ss)
  PlansCostCmd onlyBranch since asJson _ _ onlyPlan True -> runChecksCost rt onlyBranch since asJson onlyPlan
  PlansCostCmd onlyBranch since asJson transcripts by onlyPlan False -> runPlansCost rt cat onlyBranch since asJson transcripts by onlyPlan
  FixCmd rule file (line, _) n given g ->
    fixSteps (portsOf rt) (projectOf rt cat) rule (projectPath rt cwd file) line n given >>= \case
      Left (FixNotFound why) -> failWith rt fmt 3 "finding_not_found" why (Just "run `olifant check --file` to see the findings") []
      Left (FixUsage why) -> failWith rt fmt 2 "usage" why Nothing []
      Left (FixCatalog err) -> catalogFailure rt fmt err
      Left (FixFailed why) -> failWith rt fmt 1 "fix_failed" why Nothing []
      Right (context, steps, warnings) -> runStepsWith warnings False rt cwd fmt cat g context steps

-- | `olifant plans cost`: the errand records of the project and Claude
-- Code's transcripts of it, one row per plan and landing (the PR its work
-- reached), or per branch with @--by branch@. Unreadable record lines,
-- unpriced models, a missing transcript directory and no git history are
-- notes on stderr, never failures.
runPlansCost :: Runtime -> Catalog -> Maybe Text -> Maybe Day -> Bool -> Maybe FilePath -> By -> Maybe FilePath -> IO ExitCode
runPlansCost rt cat onlyBranch since asJson transcripts by onlyPlan = do
  let root = rtProjectDir rt
      ws = rtRoot rt
      prices = pcPrices (catalogPlans cat)
      dir = fromMaybe (fromMaybe "~" (rtHomeDir rt) </> ".claude/projects" </> slugOf root) transcripts
      note t = rtErr rt ("plans cost: " <> t <> "\n")
  (records, bad) <- maybe ([], 0) readRecords <$> readText ws (recordsPath root)
  when (bad > 0) $ note ("skipped " <> T.pack (show bad) <> " unreadable line" <> (if bad == 1 then "" else "s") <> " in errands.jsonl")
  present <- (== Directory) <$> inspectPath ws dir
  unless present $ note ("no Claude Code transcripts at " <> T.pack dir)
  messages <- if present then readTranscripts (listFiles ws) (readText ws) dir else pure []
  let mine b = maybe True (== b) onlyBranch
      ofPlan p = maybe True (== p) onlyPlan
      counted = [m | m <- countedMessages since records messages, mine (branchName (msBranch m))]
      unpriced = nub [msModel m | m <- counted, msTokens m /= Tokens 0 0 0 0, isNothing (messageCost prices m)]
  forM_ unpriced $ \m -> note ("no price for " <> m <> "; counted as $0")
  case by of
    ByBranch -> do
      let rows = filter (mine . rowBranch) (costRows prices since records messages)
          errands = [e | AnErrand e <- records, after since (erAt e), mine (branchName (erBranch e))]
      if asJson
        then rtOut rt (encodeJson (object ["rows" .= map rowJson rows, "errands" .= map (recordJson . AnErrand) errands]) <> "\n")
        else rtOut rt (costTable rows)
    ByPlan -> do
      let kept = [r | r <- records, case r of AnErrand e -> mine (branchName (erBranch e)); AReconcile c -> mine (branchName (rcBranch c)); _ -> True]
          planOf = \case AnErrand e -> Just (erPlan e); AReconcile c -> Just (rcPlan c); _ -> Nothing
          recordAt = \case AnErrand e -> Just (erAt e); AReconcile c -> Just (rcAt c); _ -> Nothing
          -- every plan's landings are read, so a session lands in the next landing of any plan
          plans = nub (mapMaybe planOf [r | r <- kept, maybe False (after since) (recordAt r)])
      history <- landingsOf (rtGit rt) root plans
      landings <- case history of
        Left why -> note ("no git history (" <> why <> "); nothing has landed") >> pure [(p, []) | p <- plans]
        Right ls -> pure ls
      let (planRows, trailing) = landingRows prices since [r | r <- kept, maybe True ofPlan (planOf r)] counted landings
          errands = [e | AnErrand e <- kept, after since (erAt e), ofPlan (erPlan e)]
      if asJson
        then
          rtOut rt . (<> "\n") . encodeJson $
            object
              [ "plans" .= map planJson planRows, "after" .= fmap rowFields trailing
              , "errands" .= map (recordJson . AnErrand) errands ]
        else rtOut rt (landingTable (planRows, trailing))
  pure ExitSuccess
  where
    rowJson r = object (("branch" .= rowBranch r) : rowPairs r)
    rowFields r = object (rowPairs r)
    rowPairs r =
      [ "errands" .= rowErrands r, "reconciles" .= rowReconciles r
      , "reconcile_seconds" .= rowReconcileSeconds r, "accepted" .= rowAccepted r, "decided" .= rowDecided r
      , "undecided" .= rowUndecided r, "errand_cost" .= rowErrandCost r, "sessions" .= rowSessions r
      , "session_cost" .= rowSessionCost r, "total_cost" .= (rowErrandCost r + rowSessionCost r), "last" .= stamp (rowLast r) ]
    planJson p =
      object $
        ["plan" .= prPlan p] ++ rowPairs (prTotal p)
          ++ [ "landings" .= [object (["label" .= ldLabel l, "commit" .= ldCommit l, "subject" .= ldSubject l, "at" .= stamp (ldAt l)] ++ rowPairs r) | (l, r) <- prLandings p]
             , "open" .= fmap rowFields (prOpen p) ]
    stamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- | `plans cost --checks`: per plan, from its `pass` and `check` records:
-- the sentences triaged and what triage cost (Jev is unpriced today: 0),
-- the claim checks (a check with no mode is one) and their verdicts, the
-- step checks and theirs, what the automatic pass and the user's asks
-- spent, the failed runs, and the total.
runChecksCost :: Runtime -> Maybe Text -> Maybe Day -> Bool -> Maybe FilePath -> IO ExitCode
runChecksCost rt onlyBranch since asJson onlyPlan = do
  let root = rtProjectDir rt
  (records, _) <- maybe ([], 0) readRecords <$> readText (rtRoot rt) (recordsPath root)
  let wanted plan = maybe True (== plan) onlyPlan
      checks = [e | AnErrand e <- records, erVerb e == "check", after since (erAt e), wanted (erPlan e), maybe True (== branchName (erBranch e)) onlyBranch]
      passes = [r | APass r <- records, after since (Tel.prAt r), wanted (Tel.prPlan r), maybe True (== "?") onlyBranch]
      plans = nub (map Tel.prPlan passes ++ map erPlan checks)
      rows = [checksRow plan [r | r <- passes, Tel.prPlan r == plan] [e | e <- checks, erPlan e == plan] | plan <- plans]
  if asJson
    then rtOut rt (encodeJson (object ["checks" .= map checksJson rows]) <> "\n")
    else rtOut rt (checksTable rows)
  pure ExitSuccess

-- | One plan's checks: what `plans cost --checks` shows.
data ChecksRow = ChecksRow
  { crPlan :: FilePath, crSentences :: Int, crTriageCost :: Double
  , crClaims :: (Int, Int, Int, Int, Double), crSteps :: (Int, Int, Int, Int, Double)
  , crAutomatic :: Double, crAsked :: Double, crFailed :: Int }

checksRow :: FilePath -> [Tel.PassRecord] -> [ErrandRecord] -> ChecksRow
checksRow plan ps es =
  ChecksRow
    { crPlan = plan, crSentences = sum (map Tel.prTriaged ps), crTriageCost = 0
    , crClaims = tally claims ["right", "wrong", "cannot_tell"], crSteps = tally steps ["done", "open", "misfit"]
    , crAutomatic = sum [cost e | e <- es, erAuto e], crAsked = sum [cost e | e <- es, not (erAuto e)]
    , crFailed = length [() | e <- es, erStatus e /= Errand.Ok] }
  where
    cost = fromMaybe 0 . erCost
    claims = [e | e <- es, fromMaybe "verify" (erMode e) == "verify"]
    steps = [e | e <- es, erMode e == Just "progress"]
    tally xs [a, b, c] =
      let ok = [e | e <- xs, erStatus e == Errand.Ok]
          n w = length [() | e <- ok, erVerdict e == Just w]
       in (length ok, n a, n b, n c, sum (map cost xs))
    tally _ _ = (0, 0, 0, 0, 0)

checksJson :: ChecksRow -> Value
checksJson r =
  object
    [ "plan" .= crPlan r, "sentences" .= crSentences r, "triage_cost" .= crTriageCost r
    , "claims" .= (let (n, a, b, c, d) = crClaims r in object ["n" .= n, "right" .= a, "wrong" .= b, "cannot_tell" .= c, "cost" .= d])
    , "steps" .= (let (n, a, b, c, d) = crSteps r in object ["n" .= n, "done" .= a, "open" .= b, "misfit" .= c, "cost" .= d])
    , "automatic" .= crAutomatic r, "asked" .= crAsked r, "failed" .= crFailed r, "total" .= checksTotal r ]

checksTotal :: ChecksRow -> Double
checksTotal r = crTriageCost r + crAutomatic r + crAsked r

checksTable :: [ChecksRow] -> Text
checksTable rows = T.unlines (map line (header : map cells rows))
  where
    header = ["plan", "sentences", "triage $", "claims", "right/wrong/?", "claims $", "steps", "done/open/misfit", "steps $", "automatic $", "asked $", "failed", "total $"]
    cells r =
      [T.pack (crPlan r), tshow (crSentences r), money (crTriageCost r)] ++ kind (crClaims r) ++ kind (crSteps r)
        ++ [money (crAutomatic r), money (crAsked r), tshow (crFailed r), money (checksTotal r)]
    -- a plan with no checks of a kind shows `-`
    kind (n, a, b, c, d)
      | n == 0 && d == 0 = ["-", "-", "-"]
      | otherwise = [tshow n, tshow a <> "/" <> tshow b <> "/" <> tshow c, money d]
    tshow = T.pack . show
    money x = T.pack (showFFloat (Just 2) x "")
    widths = foldr (zipWith max . map T.length) (map (const 0) header) (header : map cells rows)
    line cs = T.stripEnd (T.intercalate "  " (zipWith3 pad [0 :: Int ..] widths cs))
    pad i w c = if i == 0 then T.justifyLeft w ' ' c else T.justifyRight w ' ' c

-- | One line of `plans cost`: a branch, a plan, or a landing. Accepted, decided and undecided
-- count ghost blocks of the bucket's answered (ok) errands.
data Row = Row
  { rowBranch :: Text, rowErrands :: Int, rowReconciles :: Int, rowReconcileSeconds :: Double, rowAccepted :: Int, rowDecided :: Int, rowUndecided :: Int
  , rowErrandCost :: Double, rowSessions :: Int, rowSessionCost :: Double, rowLast :: UTCTime }
  deriving (Eq, Show)

-- | A branch, or @?@ for work with none.
branchName :: Maybe Text -> Text
branchName = fromMaybe "?"

-- | On or after the day's start (UTC), when there is a day.
after :: Maybe Day -> UTCTime -> Bool
after since t = maybe True (\d -> t >= UTCTime d 0) since

-- | The messages of interactive sessions: a session that is some errand's
-- (whatever its ending, and whenever) is that errand's, priced by the runner.
countedMessages :: Maybe Day -> [Record] -> [Message] -> [Message]
countedMessages since records messages = [m | m <- messages, msSession m `S.notMember` errandSessions, after since (msAt m)]
  where
    errandSessions = S.fromList [s | AnErrand e <- records, Just s <- [erSession e]]

-- | Each errand's blocks' outcomes; the later record of a block wins
-- (fromListWith passes the new one first).
outcomesOf :: [Record] -> M.Map Text (M.Map Int Text)
outcomesOf records = M.fromListWith M.union [(orId o, M.singleton (orBlock o) (orOutcome o)) | AnOutcome o <- records]

-- | One line from a bucket's records (which must not all be empty). An
-- errand with no cost (it failed) costs 0 but still counts as an errand.
rowOf :: [(Text, Price)] -> M.Map Text (M.Map Int Text) -> Text -> [ErrandRecord] -> [ReconcileRecord] -> [Message] -> Row
rowOf prices outcomes label es rs ms =
  Row
    { rowBranch = label
    , rowErrands = length es
    , rowReconciles = length rs
    , rowReconcileSeconds = sum [sec | r <- rs, (_, sec) <- rcClimbs r]
    , rowAccepted = length (filter (== "accepted") outs)
    , rowDecided = length outs
    , rowUndecided = sum [max 0 (erBlocks e - length (blocksOf e)) | e <- oks]
    , rowErrandCost = sum (map (fromMaybe 0 . erCost) es)
    , rowSessions = S.size (S.fromList (map msSession ms))
    , rowSessionCost = sum (map (fromMaybe 0 . messageCost prices) ms)
    , rowLast = maximum (map erAt es ++ map rcAt rs ++ map msAt ms)
    }
  where
    oks = filter ((== Errand.Ok) . erStatus) es
    outs = concatMap blocksOf oks
    blocksOf e = [o | (b, o) <- M.toList (M.findWithDefault M.empty (erId e) outcomes), b >= 0, b < erBlocks e]

-- | The rows per branch, newest activity first.
costRows :: [(Text, Price)] -> Maybe Day -> [Record] -> [Message] -> [Row]
costRows prices since records messages = sortOn (Down . rowLast) (map row branches)
  where
    errands = [e | AnErrand e <- records, after since (erAt e)]
    msgs = countedMessages since records messages
    reconciles = [r | AReconcile r <- records, after since (rcAt r)]
    branches = nub (map (branchName . erBranch) errands ++ map (branchName . rcBranch) reconciles ++ map (branchName . msBranch) msgs)
    outcomes = outcomesOf records
    row b =
      rowOf prices outcomes b
        (filter ((== b) . branchName . erBranch) errands)
        (filter ((== b) . branchName . rcBranch) reconciles)
        (filter ((== b) . branchName . msBranch) msgs)

-- | A plan's line, its landings (oldest first) and what has not landed.
data PlanRows = PlanRows {prPlan :: FilePath, prTotal :: Row, prLandings :: [(Landing, Row)], prOpen :: Maybe Row}

-- | Per plan, per landing: an errand or reconcile belongs to the first
-- landing of its plan at or after it (none: not landed), a message (of the
-- already counted sessions given, so not filtered again) to the first
-- landing of any plan at or after it. The second result is the row of
-- messages after the last landing (Nothing when none). A landing with
-- nothing in it is dropped, a plan with no errand or reconcile is absent;
-- plans newest activity first.
landingRows :: [(Text, Price)] -> Maybe Day -> [Record] -> [Message] -> [(FilePath, [Landing])] -> ([PlanRows], Maybe Row)
landingRows prices since records msgs landings = (sortOn (Down . rowLast . prTotal) (map planRows plans), trailing)
  where
    errands = [e | AnErrand e <- records, after since (erAt e)]
    reconciles = [r | AReconcile r <- records, after since (rcAt r)]
    plans = nub (map erPlan errands ++ map rcPlan reconciles)
    outcomes = outcomesOf records
    allLandings = sortOn ldAt (concatMap snd landings)
    ofPlan p = fromMaybe [] (lookup p landings)
    key l = (ldPlan l, ldCommit l)
    msgsAt l = [m | m <- msgs, fmap key (landingFor allLandings (msAt m)) == Just (key l)]
    errandsAt p l = [e | e <- errands, erPlan e == p, fmap key (landingFor (ofPlan p) (erAt e)) == fmap key l]
    reconcilesAt p l = [r | r <- reconciles, rcPlan r == p, fmap key (landingFor (ofPlan p) (rcAt r)) == fmap key l]
    nonEmpty es rs ms = not (null es && null rs && null ms)
    trailing = case [m | m <- msgs, Nothing <- [landingFor allLandings (msAt m)]] of
      [] -> Nothing
      ms -> Just (rowOf prices outcomes "(since the last landing)" [] [] ms)
    planRows p =
      let buckets = [(l, errandsAt p (Just l), reconcilesAt p (Just l), msgsAt l) | l <- ofPlan p]
          shown = [(l, rowOf prices outcomes (ldLabel l <> " " <> shortSubject 50 (ldSubject l)) es rs ms) | (l, es, rs, ms) <- buckets, nonEmpty es rs ms]
          openEs = errandsAt p Nothing
          openRs = reconcilesAt p Nothing
          open = if nonEmpty openEs openRs [] then Just (rowOf prices outcomes "(not landed)" openEs openRs []) else Nothing
          total = rowOf prices outcomes (T.pack p) (concat [es | (_, es, _, _) <- buckets] ++ openEs) (concat [rs | (_, _, rs, _) <- buckets] ++ openRs) (concat [ms | (_, _, _, ms) <- buckets])
       in PlanRows p total shown open

-- | The rows as a table: columns two spaces apart, money to the cent.
costTable :: [Row] -> Text
costTable rows = rowTable "branch" [(rowBranch r, r) | r <- rows]

-- | The plan and landing rows as a table: a plan's line, its landings
-- indented under it, then the messages after the last landing.
landingTable :: ([PlanRows], Maybe Row) -> Text
landingTable (plans, trailing) = rowTable "plan / landing" (concatMap one plans ++ [(rowBranch r, r) | Just r <- [trailing]])
  where
    one p = (T.pack (prPlan p), prTotal p) : [("  " <> rowBranch r, r) | (_, r) <- prLandings p] ++ [("  " <> rowBranch r, r) | Just r <- [prOpen p]]

-- | Rows under a header, the first column their label.
rowTable :: Text -> [(Text, Row)] -> Text
rowTable first labelled = T.unlines (map line (header : map cells labelled))
  where
    header = [first, "errands", "reconciles", "reconcile s", "accepted", "errand $", "sessions", "session $", "total $"]
    cells (label, r) =
      [ label, tshow (rowErrands r), tshow (rowReconciles r), T.pack (showFFloat (Just 1) (rowReconcileSeconds r) ""), tshow (rowAccepted r) <> "/" <> tshow (rowDecided r) <> " (" <> tshow (rowUndecided r) <> " ?)"
      , money (rowErrandCost r), tshow (rowSessions r), money (rowSessionCost r), money (rowErrandCost r + rowSessionCost r) ]
    tshow = T.pack . show
    money x = T.pack (showFFloat (Just 2) x "")
    widths = foldr (zipWith max . map T.length) (map (const 0) header) (header : map cells labelled)
    -- the label and acceptance read left to right; numbers line up right
    line cs = T.stripEnd (T.intercalate "  " (zipWith3 pad [0 :: Int ..] widths cs))
    pad i w c = if i `elem` [0, 4] then T.justifyLeft w ' ' c else T.justifyRight w ' ' c

-- | Snippets (spec §5.3): a snippet needs `--at`; its line is read from the
-- overlay's text when there is one.
placeCursor :: Runtime -> FilePath -> Catalog -> GenOptions -> [Generator] -> IO (Either Text [Generator])
placeCursor rt cwd cat g gens = case (genAt g, filter isSnippet gens) of
  (_, []) -> pure (Right gens)
  (Nothing, s : _) -> pure (Left ("`" <> generatorIdText (genId s) <> "` is a snippet: it needs --at FILE:LINE:COL"))
  (Just (file, line, _), _) -> do
    project <- overlaidProject rt cwd cat g
    placeAt (portsOf rt) project (projectPath rt cwd file, line) gens

-- | Run plan steps as `gen` does (model, fills, progress, regions,
-- output): `gen` itself, and `fix` (a replacement, then the fix's generator,
-- with the fix's context files shown to the model).
runSteps :: Runtime -> FilePath -> Format -> Catalog -> GenOptions -> [(FilePath, Text)] -> [Step] -> IO ExitCode
runSteps = runStepsWith [] False

-- | 'runSteps', with warnings for the plan (a fix's: a definition's
-- callers), and whether a step is a snippet placed at `--at`.
runStepsWith :: [Text] -> Bool -> Runtime -> FilePath -> Format -> Catalog -> GenOptions -> [(FilePath, Text)] -> [Step] -> IO ExitCode
runStepsWith warnings snippet rt cwd fmt cat g context steps = do
  -- `--overlay FILE` (spec §4.8): plan against the editor's unsaved text.
  overlaid <- case genOverlay g of
    Just _ | not (genPlanOnly g) -> pure (Left "--overlay needs --plan (an unsaved buffer is planned, never written)")
    _ -> Right <$> overlaidProject rt cwd cat g
  -- `--fills FILE`: recorded fills stand in for the model
  chosen <- case genFills g of
    Nothing -> pure (modelFor cat g)
    Just f -> readText (rtRoot rt) (normalise (cwd </> f)) <&> \case
      Nothing -> Left ("--fills " <> T.pack f <> ": no such file")
      Just src -> (\fills -> Just (ModelFiller "recorded" 0 (replayFiller fills))) <$> parseFills src
  case (overlaid, chosen) of
    (Left why, _) -> failWith rt fmt 2 "usage" why Nothing []
    (_, Left why) -> failWith rt fmt 2 "usage" why Nothing []
    (Right project, Right filler) -> do
      let po =
            PlanOptions
              { poMode = if genPlanOnly g then PlanOnly else Apply
              , poAt = (\(f, l, _) -> (projectPath rt cwd f, l)) <$> genAt g
              , poSnippet = snippet
              , poFiller = filler
              , poNoFill = genNoFill g
              , poContext = context
              , poProgress = if genProgress g then rtErr rt . (<> "\n") . encodeJson . progressJson else const (pure ())
              , poNow = genNow g
              , poCancel = neverCancel
              , poStance = FillStance
              , poSteering = Nothing
              , poJudged = []
              }
      planWith (portsOf rt) project po steps >>= \case
        Left err -> generationFailure rt fmt err
        Right plan0 -> do
          let plan = plan0 {planWarnings = planWarnings plan0 ++ warnings}
              applied = poMode po == Apply && not (hasConflicts plan)
          emit rt fmt (planJson applied plan) (planHuman applied plan)
          pure (if hasConflicts plan then ExitFailure 5 else ExitSuccess)

-- | The project, with the editor's unsaved text of `--overlay FILE` (read
-- from stdin) over disk.
overlaidProject :: Runtime -> FilePath -> Catalog -> GenOptions -> IO Project
overlaidProject rt cwd cat g = case genOverlay g of
  Nothing -> pure project
  Just f -> rtReadStdin rt <&> \text -> project {prWorkspace = overlay (projectPath rt cwd f) text (prWorkspace project)}
  where
    project = projectOf rt cat

-- | A path given where Olifant runs, relative to the project.
projectPath :: Runtime -> FilePath -> FilePath -> FilePath
projectPath rt cwd f = makeRelative (rtProjectDir rt) (normalise (cwd </> f))

-- | A usage-level failure inside a command: JSON on stdout in JSON mode.
usageIn :: Runtime -> Format -> Text -> IO ExitCode
usageIn rt fmt msg = do
  case fmt of
    Json -> rtOut rt (encodeJson (errorJson "usage" msg []) <> "\n")
    Human -> rtErr rt (msg <> "\n")
  pure (ExitFailure 2)

-- | A generator's [[uses]] steps with the entries they resolve to, so `show`
-- can describe a composite completely in one call.
emit :: Runtime -> Format -> Value -> Text -> IO ()
emit rt fmt json human = rtOut rt $ case fmt of
  Json -> encodeJson json <> "\n"
  Human -> human

failWith :: Runtime -> Format -> Int -> Text -> Text -> Maybe Text -> [Pair] -> IO ExitCode
failWith rt fmt code errCode message hint extra = do
  case fmt of
    Json -> rtOut rt (encodeJson (errorJson errCode message (maybe [] (\h -> ["hint" .= h]) hint ++ extra)) <> "\n")
    Human -> rtErr rt (errorHuman message hint)
  pure (ExitFailure code)

catalogFailure :: Runtime -> Format -> CatalogError -> IO ExitCode
catalogFailure rt fmt = catalogFailureAt rt fmt Nothing

-- | Like 'catalogFailure', naming the chain step it happened in (spec §7).
catalogFailureAt :: Runtime -> Format -> Maybe Text -> CatalogError -> IO ExitCode
catalogFailureAt rt fmt step err = case err of
  GeneratorNotFound _ available ->
    failWith' 3 "generator_not_found" msg (Just "run `olifant list` to see generators") ["available" .= map generatorIdText available]
  GeneratorAmbiguous {} -> failWith' 2 "generator_ambiguous" msg (Just "use the full pack/name") []
  SourceMissing {} -> failWith' 3 "template_not_found" msg Nothing []
  ConfigInvalid {} -> failWith' 1 "config_invalid" msg Nothing []
  PackMissing {} -> failWith' 1 "config_invalid" msg Nothing []
  PackNotConfigured pack _ ->
    failWith' 3 "pack_not_configured" msg (Just (pack <> " = { path = \"/path/to/" <> pack <> "\" }")) ["pack" .= pack]
  GeneratorInvalid {} -> failWith' 1 "generator_invalid" msg (Just formatHint) []
  SourceInvalid {} -> failWith' 1 "template_invalid" msg Nothing []
  where
    msg = maybe "" (<> ": ") step <> describeCatalogError err
    failWith' code c m h extra = failWith rt fmt code c m h (extra ++ maybe [] (\st -> ["step" .= st]) step)

generationFailure :: Runtime -> Format -> GenerationError -> IO ExitCode
generationFailure rt fmt err = case err of
  InvalidInputs path (InputProblems ps) -> failWith rt fmt 4 "invalid_inputs" msg Nothing (inputProblemFields ps ++ ["step" .= showStepPath path])
  InvalidInputs {} -> failWith rt fmt 1 "generator_invalid" msg Nothing []
  TemplateFailed {} -> failWith rt fmt 1 "template_failed" msg Nothing []
  UnknownGenerator {} -> failWith rt fmt 3 "generator_not_found" msg Nothing []
  InvalidGenerator {} -> failWith rt fmt 1 "generator_invalid" msg Nothing []
  QueryFailed {} -> failWith rt fmt 1 "query_failed" msg Nothing []
  CompositionCycle {} -> failWith rt fmt 1 "composition_cycle" msg Nothing []
  where
    msg = describeGenerationError err

-- | Refresh every region (spec §5.8). With @checkOnly@, write nothing and
-- fail (exit 5) if any region drifted, is broken, or is not set up.
-- | What `sync` does with the refresh: write it, only report drift
-- (`--check`), or show it as a plan with whole files (`--plan`, for editors).
data SyncOutput = SyncWrite | SyncReport | SyncShow
  deriving stock (Eq)

syncRegions :: Runtime -> Format -> [(FilePath, Text)] -> Workspace IO -> Tools IO -> RegionMode -> SyncOutput -> [FilePath] -> UTCTime -> IO ExitCode
syncRegions rt fmt rtBrokenRegions ws tools regionMode output absorb now = do
  (edits, conflicts0, current) <- refreshRegions regionMode tools ws 1 M.empty
  unset <- if regionMode == SkipUnset then unsetRegions tools ws else pure []
  let conflicts = [RegionBroken "" path ("cannot read " <> T.pack path <> ": " <> err) | (path, err) <- rtBrokenRegions] ++ conflicts0
      writes = M.fromList [(p, if p `elem` absorb then absorbDuplicates c else c) | (p, Just c) <- M.toList current]
      warnings = ["region `" <> r <> "` is not set up in " <> T.pack p <> " (run `olifant sync --init " <> r <> "`)" | (p, r) <- unset]
  before <- M.fromList <$> traverse (\p -> (p,) <$> readText ws p) (M.keys writes)
  let plan = Plan (GeneratorId "sync") [] now edits conflicts warnings writes [] before M.empty Nothing
      applied = output == SyncWrite && not (hasConflicts plan)
  when applied $ mapM_ (uncurry (writeText ws)) (M.toList writes)
  emit rt fmt (planJson applied plan) (if output == SyncReport then syncCheckHuman plan else planHuman applied plan)
  pure (if hasConflicts plan || (output == SyncReport && not (null edits)) then ExitFailure 5 else ExitSuccess)

-- | `sync --init NAME`: put a region's empty fences at its `init_at`, then
-- fill it. A family (spec §5.9) sets up every matching file that lacks them.
initRegion :: Runtime -> Format -> Workspace IO -> Tools IO -> RegionDef -> UTCTime -> IO ExitCode
initRegion rt fmt ws tools def now = do
  universe <- listFiles ws "."
  let instances = regionInstances universe def
      family = length instances /= 1 || map regionFile instances /= [regionFile def]
  results <- traverse (initFences ws tools) instances
  let fenced = [regionFile i | (i, Right True) <- zip instances results]
      whereText = if family then T.pack (show (length instances)) <> " files" else T.pack (regionFile def)
  case [failure | Left failure <- results] of
    (code, err, msg) : _ -> failWith rt fmt code err msg Nothing []
    []
      | null fenced ->
          emit rt fmt (object ["region" .= regionName def, "status" .= ("already set up" :: Text)]) ("region " <> regionName def <> " is already set up in " <> whereText <> "\n") >> pure ExitSuccess
      | otherwise -> syncRegions rt fmt [] ws tools {toolRegions = [def]} SkipUnset SyncWrite fenced now

-- | Put one region's empty fences at its `init_at`: Right True when added,
-- Right False when already there, Left (exit code, error code, message).
initFences :: Workspace IO -> Tools IO -> RegionDef -> IO (Either (Int, Text, Text) Bool)
initFences ws tools def
  | not (isSafeRelative file) = pure (Left (5, "region_broken", "region `" <> regionName def <> "`: " <> T.pack file <> " is outside the project"))
  | otherwise = case regionComment def <|> (langComment <$> lang) of
      Nothing -> pure (Left (1, "region_broken", "no comment syntax for " <> T.pack file <> " (add `comment` to the region)"))
      Just c ->
        readText ws file >>= \case
          -- A whole-file region is set up by creating its file with the header.
          Nothing
            | regionWholeFile def -> writeText ws file (wholeHeader def c <> "\n") >> pure (Right True)
            | otherwise -> pure (Left (3, "file_not_found", "cannot read " <> T.pack file))
          Just content -> case applyRegion def c [] content of
            Right _ -> pure (Right False)
            Left (FenceBroken why) -> pure (Left (5, "region_broken", "region `" <> regionName def <> "` in " <> T.pack file <> " is broken: " <> why))
            Left FenceMissing -> case regionInitAt def of
              Nothing -> pure (Left (2, "usage", "region `" <> regionName def <> "` has no `init_at`, so Olifant cannot tell where its fences go"))
              Just phrase ->
                resolveLayers (toolSyntax tools) lang (Just file) [PhraseAnchor phrase] content >>= \case
                  Left misses -> pure (Left (5, "anchor_unresolved", T.pack file <> ": " <> describeMisses misses))
                  Right loc -> do
                    let (open, close) = fenceLines def c
                        placed = maybe loc (\i -> loc {locIndent = i}) (regionInitIndent def)
                    writeText ws file (snd (insertLines placed (open <> "\n" <> close) content))
                    pure (Right True)
  where
    file = regionFile def
    lang = languageFor (toolLanguages tools) file

-- | `init --language NAME` (m5): survey, pin, gate, errand, check, write.
-- The model proposes the grammar's repository and writes the files; olifant
-- pins the revision ('remoteHead'), asks before it fetches and builds a
-- proposed repository (`--yes`, or y on a terminal), reads node kinds only
-- from a grammar that loaded, and checks every answer before anything is
-- written ('LI.checkAnswers'). A language olifant ships goes to
-- 'runInitShipped'.
runInitLanguage :: Runtime -> FilePath -> Format -> Catalog -> Text -> Maybe Text -> Maybe Text -> Bool -> Bool -> Maybe FilePath -> IO ExitCode
runInitLanguage rt cwd fmt cat l modelArg grammarArg yes plan fillsArg = do
  let ws = within (rtProjectDir rt) (rtRoot rt)
      project = projectOf rt cat
      stop = initStop rt fmt
  layerThere <- fileExists ws (LI.languageFile l)
  toolchainThere <- fileExists ws (LI.toolchainFile l)
  chosen <- case fillsArg of
    Nothing -> pure (modelFiller cat modelArg)
    Just f -> readText (rtRoot rt) (normalise (cwd </> f)) <&> \case
      Nothing -> Left ("--fills " <> T.pack f <> ": no such file")
      Just src -> (\fills -> Just (ModelFiller "recorded" 0 (replayFiller fills))) <$> parseFills src
  case chosen of
    _ | layerThere -> stop (T.pack (LI.languageFile l) <> " exists: olifant doctor shows its rows; delete it to generate again") Nothing []
    _ | Just shipped <- lookup l shippedLanguages -> runInitShipped rt fmt cat l shipped chosen grammarArg yes plan
    _ | toolchainThere -> stop (T.pack (LI.toolchainFile l) <> " exists: olifant doctor shows its rows; delete it to generate again") Nothing []
    Left why -> failWith rt fmt 2 "usage" why Nothing []
    Right Nothing -> stop "init --language needs a model: add [model] (olifant doctor shows how)" Nothing []
    Right (Just mf) -> do
      t0 <- getMonotonicTime
      tracked <- sort <$> projectListing (portsOf rt) project
      survey <- askSurvey mf l tracked
      t1 <- getMonotonicTime
      case survey of
        Left why -> stop why Nothing []
        Right sv0 -> do
          let sv = maybe sv0 (\u -> sv0 {LI.svGrammar = u}) grammarArg
              gname = fromMaybe l (LI.svName sv)
              (sources, tests, manifest) = LI.sampleFor sv tracked
              queries = if null (LI.shippedFor gname) then [".olifant/queries/" <> gname <> "/"] else []
              files = [T.pack (LI.languageFile l), T.pack (LI.toolchainFile l)] ++ queries
              planOut proposal url sha = do
                emit rt fmt
                  (object ["survey" .= object ["grammar" .= url, "files" .= LI.svFiles sv, "tests" .= LI.svTests sv, "runners" .= map LI.rnProgram (LI.svRunners sv)], "grammar" .= object ["git" .= url, "rev" .= sha], "files" .= files, "sample" .= (sources ++ tests ++ maybe [] pure manifest)])
                  (T.unlines ([proposal] ++ ["would write " <> f | f <- files] ++ ["sample: " <> T.intercalate ", " (map T.pack (sources ++ tests ++ maybe [] pure manifest))]))
                pure ExitSuccess
          gateGrammar rt fmt l gname sv (isJust grammarArg) yes plan planOut >>= \case
            Left code -> pure code
            Right (g, proposal, url, sha, ld, s) -> do
              let info = GrammarInfo (grammarLabel g) (ldAbi ld) (ldLibrary ld) Nothing
              upstream0 <- LI.readUpstream (url <> " @" <> T.take 7 sha) (checkoutOf s g) (LI.svPath sv)
              -- nvim's textobjects for a grammar olifant ships no queries for (its URL and SHA olifant's)
              upstream <-
                if not (null (LI.shippedFor gname))
                  then pure upstream0
                  else
                    LI.nvimTextobjects s gname >>= \case
                      Left why -> upstream0 <$ rtErr rt ("init-language: textobjects: not fetched (" <> why <> "): the model writes them\n")
                      Right (Nothing, _) -> upstream0 <$ rtErr rt ("init-language: textobjects: nvim-treesitter-textobjects has none for " <> gname <> ": the model writes them\n")
                      Right (found@(Just (from, _)), lic) -> upstream0 {LI.upNvim = found, LI.upNvimLicence = lic} <$ rtErr rt ("init-language: textobjects: " <> from <> "\n")
              tools <- mapM (\r -> LI.runnerLine r <$> dpFind (rtDoctor rt) (T.unpack (LI.rnProgram r))) (LI.svRunners sv)
              sample <- fmap concat . mapM (\p -> readText ws p <&> maybe [] (\t -> [(p, LI.cutTo 120 t)])) $ sources ++ tests ++ maybe [] pure manifest
              let ei = LI.ErrandInput l g (ldNodeTypes ld) upstream LI.shippedFor tools sample (rtProjectDir rt) sv tracked
              LI.errandCore (mfFiller mf) ei >>= \case
                Left why -> stop ("the model did not answer the errand: " <> why) Nothing [grammarCheck l gname (Right info)]
                Right (req, first') ->
                  answerRounds rt fmt
                    Rounds
                      { rdLanguage = l
                      , rdFiller = mfFiller mf
                      , rdRequest = req
                      , rdCheck = LI.checkAnswers ei
                      , rdLeave = LI.leaveOut ei
                      , rdFailing = LI.failingRows l
                      , rdHead = [proposal]
                      , rdJson = ["grammar" .= object ["git" .= url, "rev" .= sha]]
                      , rdStart = (t0, t1)
                      , rdBeforeWrite = pure ()
                      }
                    first'

-- | `init --language L` for a language olifant ships (m5b). Doctor runs
-- first, before any model is asked. A language whose layers name no grammar
-- source (elm without the elm pack: 'catalogNoGrammar') gets a one-key
-- layer, the grammar the survey (or `--grammar`) names, pinned, asked about
-- and loaded as for a new language. Otherwise, with no failing row, nothing
-- is written; with failing rows, the model answers only the keys those rows
-- point at ('LI.layerAsk'), the engine writes them as a project layer (and
-- the preset's keys as the project's preset of the shipped name), and the
-- one repair round follows as for a new language ('answerRounds').
runInitShipped :: Runtime -> Format -> Catalog -> Text -> Text -> Either Text (Maybe (ModelFiller IO)) -> Maybe Text -> Bool -> Bool -> IO ExitCode
runInitShipped rt fmt cat l shippedText chosen grammarArg yes plan
  | l `elem` catalogNoGrammar cat = grammarLayer
  | Just (path, why) <- listToMaybe [b | b@(p, _) <- catalogBrokenLanguages cat, takeBaseName p == T.unpack l] = stop (T.pack path <> " does not load: " <> why) (Just "fix the adapter file") []
  | otherwise = case (find ((== l) . langName) (catalogLanguages cat), languageFromLayers shippedPresets l [layer]) of
      (Nothing, Left why) -> stop ("the shipped " <> l <> " does not load: " <> why) Nothing []
      (named, alone) -> do
        when (isJust grammarArg) $ rtErr rt ("init-language: --grammar is ignored: " <> l <> " ships with a grammar\n")
        let unnamed = isNothing named
            doctorCat = if unnamed then cat {catalogLanguages = catalogLanguages cat ++ either (const []) pure alone} else cat
        rows0 <- doctorChecks (portsOf rt) (projectOf rt doctorCat) True
        let rows = LI.languageRows l [] rows0
            failing = LI.shippedFailing l [] rows0
            note = ["nothing in this project names " <> l <> " yet: list it in .olifant/config.toml (languages = [\"" <> l <> "\"]) or add a pack that does" | unnamed]
        if null failing
          then do
            emit rt fmt
              (object ["written" .= ([] :: [Text]), "rows" .= initRowsJson rows, "repaired" .= False])
              (T.unlines ([l <> " ships with olifant; doctor finds nothing to override here"] ++ note ++ map initRowLine rows))
            pure (if any ((== Bad) . checkStatus) rows then ExitFailure 1 else ExitSuccess)
          else layerFor unnamed failing
  where
    ws = within (rtProjectDir rt) (rtRoot rt)
    stop = initStop rt fmt
    layer = fromMaybe (Layer "shipped" Nothing M.empty) (shippedLayer l)
    -- the preset the shipped [test] names, and its shipped text
    preset = case M.lookup "test" (layerTable layer) of
      Just (Toml.TTable t) | Just (Toml.TText p) <- M.lookup "preset" t -> Just (p, fromMaybe "" (lookup p shippedToolchains))
      _ -> Nothing
    presetField name = name `elem` map LI.fdName (LI.toolchainForm l)
    withModel k = case chosen of
      Left why -> failWith rt fmt 2 "usage" why Nothing []
      Right Nothing -> stop "init --language needs a model: add [model] (olifant doctor shows how)" Nothing []
      Right (Just mf) -> k mf
    -- failing rows: the keys they point at, as a layer over the shipped file
    layerFor unnamed failing = do
      presetThere <- maybe (pure False) (\(p, _) -> fileExists ws (LI.presetFile p)) preset
      -- the keys the rows point at need no survey: none asked stops before any model call
      let noSurvey = LI.Survey "" Nothing Nothing [] [] Nothing []
          (preDerived, preAsked) = LI.layerAsk (LI.LayerInput l shippedText layer preset [] [] (rtProjectDir rt) noSurvey [] Nothing) failing
          grammarRow = listToMaybe [c | c <- failing, checkName c == l <> ": grammar"]
      if null preAsked && isNothing preDerived
        then stop ("doctor's failing rows for " <> l <> " name no key a project layer sets") (initRowLine <$> grammarRow) failing
        else withModel $ \mf -> do
          t0 <- getMonotonicTime
          tracked <- sort <$> projectListing (portsOf rt) (projectOf rt cat)
          survey <- askSurvey mf l tracked
          t1 <- getMonotonicTime
          case survey of
            Left why -> stop why Nothing []
            Right sv -> do
              let (sources, tests, manifest) = LI.sampleFor sv tracked
              tools <- mapM (\r -> LI.runnerLine r <$> dpFind (rtDoctor rt) (T.unpack (LI.rnProgram r))) (LI.svRunners sv)
              sample <- fmap concat . mapM (\p -> readText ws p <&> maybe [] (\t -> [(p, LI.cutTo 120 t)])) $ sources ++ tests ++ maybe [] pure manifest
              let li0 = LI.LayerInput l shippedText layer preset tools sample (rtProjectDir rt) sv tracked Nothing
                  (derived, asked) = LI.layerAsk li0 failing
                  li = li0 {LI.liDerived = derived}
                  presetAsked = filter presetField asked
                  files =
                    [T.pack (LI.languageFile l) | isJust derived || any (not . presetField) asked]
                      ++ [T.pack (LI.presetFile p) | Just (p, _) <- [preset], not (null presetAsked)]
                      ++ [T.pack (LI.languageFile l) | isNothing preset, not (null presetAsked), isNothing derived, all presetField asked]
                  header = l <> " ships with olifant; doctor finds rows failing here, so a project layer answers their keys"
              case () of
                _ | presetThere, not (null presetAsked), Just (p, _) <- preset -> stop (T.pack (LI.presetFile p) <> " exists: olifant doctor shows its rows; delete it to generate again") Nothing failing
                _ | null asked, isNothing derived -> stop ("doctor's failing rows for " <> l <> " name no key a project layer sets") Nothing failing
                _ | plan -> do
                  emit rt fmt
                    (object ["asks" .= asked, "files" .= files, "rows" .= initRowsJson failing])
                    (T.unlines ([header] ++ ["would write " <> f | f <- files] ++ ["asks: " <> T.intercalate ", " asked | not (null asked)] ++ map initRowLine failing))
                  pure ExitSuccess
                _ -> do
                  first' <-
                    if null asked
                      then pure (Right (LI.checkLayer li M.empty))
                      else runFill (mfFiller mf) (LI.layerRequest li failing asked) <&> fmap (LI.checkLayer li . (`M.restrictKeys` S.fromList asked))
                  case first' of
                    Left why -> stop ("the model did not answer the errand: " <> why) Nothing failing
                    Right answered ->
                      answerRounds rt fmt
                        Rounds
                          { rdLanguage = l
                          , rdFiller = mfFiller mf
                          , -- a repair may ask any key but a derived tests_for, or one of a preset file that is there
                            rdRequest = LI.layerRequest li [] [n | fd <- LI.layerForm l, let n = LI.fdName fd, not (isJust derived && n == "tests_for"), not (presetThere && presetField n)]
                          , rdCheck = LI.checkLayer li
                          , rdLeave = LI.leaveOutWith (const True) (LI.checkLayer li)
                          , rdFailing = LI.shippedFailing l
                          , rdHead = [header]
                          , rdJson = []
                          , rdStart = (t0, t1)
                          , rdBeforeWrite = when unnamed (nameLanguage ws l)
                          }
                        answered
    -- no grammar source in any layer: one key, the grammar, pinned
    grammarLayer = case (grammarArg, chosen) of
      (Nothing, Left why) -> failWith rt fmt 2 "usage" why Nothing []
      (Nothing, Right Nothing) -> stop "init --language needs a model: add [model] (olifant doctor shows how)" Nothing []
      (_, model) -> do
        t0 <- getMonotonicTime
        tracked <- sort <$> projectListing (portsOf rt) (projectOf rt cat)
        survey <- case (grammarArg, model) of
          (Just u, _) -> pure (Right (LI.Survey u Nothing Nothing [] [] Nothing []))
          (Nothing, Right (Just mf)) -> askSurvey mf l tracked
          _ -> pure (Left "init --language needs a model: add [model] (olifant doctor shows how)")
        t1 <- getMonotonicTime
        case survey of
          Left why -> stop why Nothing []
          Right sv -> do
            let gname = fromMaybe l (LI.svName sv)
                file = LI.languageFile l
                header = l <> " ships with olifant without a grammar source; the project layer gives it one"
                planOut proposal url sha = do
                  emit rt fmt
                    (object ["grammar" .= object ["git" .= url, "rev" .= sha], "files" .= [T.pack file]])
                    (T.unlines [header, proposal, "would write " <> T.pack file])
                  pure ExitSuccess
            gateGrammar rt fmt l gname sv (isJust grammarArg) yes plan planOut >>= \case
              Left code -> pure code
              Right (g, proposal, url, sha, _, _) -> do
                let text = T.unlines [LI.initHeader, "# " <> l <> " ships with olifant without a grammar source: this one, pinned by init", LI.pinText g]
                    loads = Toml.parseToml text >>= \top -> languageFromLayers shippedPresets l [layer, Layer "project" (Just (rtProjectDir rt)) top]
                case loads of
                  Left why -> stop ("the grammar layer does not load: " <> why) Nothing []
                  Right _ -> do
                    writeText ws file text
                    rows <- LI.languageRows l [file] <$> initDoctorRows rt l
                    initFinish rt fmt (t0, t1) [header, proposal] ["grammar" .= object ["git" .= url, "rev" .= sha]] [(file, text)] False rows Nothing

-- | The language named in `.olifant/config.toml`'s `languages` (the file made
-- when there is none), so a shipped language nothing else names is the
-- project's.
nameLanguage :: Workspace IO -> Text -> IO ()
nameLanguage ws l = do
  let path = ".olifant/config.toml"
  current <- fromMaybe "" <$> readText ws path
  writeText ws path (addToLanguages l current)

-- | TEXT, a config, with L in its top-level `languages` list (made when absent).
addToLanguages :: Text -> Text -> Text
addToLanguages l text = case break isLanguages ls of
  (_, []) -> T.unlines (("languages = [" <> q <> "]") : ls)
  (before, line : after)
    | "]" `T.isInfixOf` line ->
        let (open, close) = T.breakOnEnd "]" line
            body = T.dropEnd 1 open
         in T.unlines (before ++ [(if T.isSuffixOf "[" (T.stripEnd body) then body <> q else body <> ", " <> q) <> "]" <> close] ++ after)
    | otherwise -> T.unlines (before ++ [line, "  " <> q <> ","] ++ after)
  where
    q = "\"" <> l <> "\""
    ls = T.lines text
    isLanguages ln = "languages" `T.isPrefixOf` ln && "=" `T.isPrefixOf` T.strip (T.drop 9 ln)

-- | The survey (one hole, `survey`) asked and read.
askSurvey :: ModelFiller IO -> Text -> [FilePath] -> IO (Either Text LI.Survey)
askSurvey mf l tracked =
  runFill (mfFiller mf) (LI.surveyRequest l tracked) <&> \case
    Left why -> Left ("the model did not answer the survey: " <> why)
    Right fills -> maybe (Left "the model gave no `survey`") LI.parseSurvey (M.lookup "survey" fills)

-- | The survey's grammar pinned ('remoteHead'), shown, asked about (unless
-- GIVEN with `--grammar` or YES), fetched, built and loaded: the grammar,
-- the proposal line, its URL and SHA, what loaded and the settings; or the
-- exit, having printed the grammar row. With PLAN, PLANOUT prints what
-- would be written, after the pin and before any fetch.
gateGrammar :: Runtime -> Format -> Text -> Text -> LI.Survey -> Bool -> Bool -> Bool -> (Text -> Text -> Text -> IO ExitCode) -> IO (Either ExitCode (Grammar, Text, Text, Text, Loaded, GrammarSettings))
gateGrammar rt fmt l gname sv given yes plan planOut = do
  let url = LI.svGrammar sv
      stop = initStop rt fmt
  s <- grammarSettings
  remoteHead s url >>= \case
    Left why -> do
      let row = grammarCheck l gname (Left (FetchFailed ("pinning " <> url <> " failed: " <> why)))
      Left <$> stop ("the grammar's repository has no revision to pin: " <> url) (Just (initRowLine row)) [row]
    Right sha -> do
      let g = Grammar gname (Just (SourceGit url sha (LI.svPath sv)))
          label = url <> " @" <> T.take 7 sha
          proposal = "grammar: " <> label <> (if given then " (given with --grammar)" else " (proposed by the model)")
      if plan
        then Left <$> planOut proposal url sha
        else do
          go <-
            if yes || given
              then pure True
              else
                if rtStdoutIsTty rt
                  then do
                    rtErr rt (proposal <> "\nfetch and build it? [y/N] ")
                    answer <- T.toLower . T.strip <$> rtReadLine rt
                    pure (answer `elem` ["y", "yes"])
                  else pure False
          if not go
            then Left <$> stop proposal (Just "pass --yes to fetch and build it") []
            else
              loadGrammarWith s g >>= \case
                Left f -> do
                  let row = grammarCheck l gname (Left f)
                  Left <$> stop ("the grammar does not load: " <> failureLine f) (Just (initRowLine row)) [row]
                Right ld -> pure (Right (g, proposal, url, sha, ld, s))
  where
    failureLine = \case
      FetchFailed why -> why
      NeedsCompiler why -> "needs a C compiler: " <> why
      BuildFailed why -> "building failed: " <> why
      LoadFailed why -> "loading failed: " <> why
      AbiMismatch n _ -> "ABI " <> T.pack (show n) <> " is not one this olifant reads"
      NoSource -> "no source"

-- | What 'answerRounds' needs after the first round: the language, the
-- model, the request a repair asks its holes from, the check of a round's
-- answers, how a key that fails is left out, which of doctor's rows count as
-- failing, the lines printed first and the JSON added, and when the survey
-- started and ended.
data Rounds = Rounds
  { rdLanguage :: Text
  , rdFiller :: Filler IO
  , rdRequest :: FillRequest
  , rdCheck :: M.Map Text Text -> LI.Answered
  , rdLeave :: LI.Answered -> LI.Answered
  , rdFailing :: [FilePath] -> [Check] -> [Check]
  , rdHead :: [Text]
  , rdJson :: [Pair]
  , rdStart :: (Double, Double)
  , rdBeforeWrite :: IO ()
  -- ^ run once before the first file is written (a shipped language nothing
  -- names is listed in the config then)
  }

-- | The first round's checked answers written, doctor, exactly one repair
-- round, and the final rows (Decision 10). A key that fails and may be left
-- out is, so doctor reads the files before the repair and the repair answers
-- the init rows and doctor's at once; a file that does not load is repaired
-- before anything is written; a repair that breaks a row that passed gets
-- the first round's values back for the keys behind it.
answerRounds :: Runtime -> Format -> Rounds -> LI.Answered -> IO ExitCode
answerRounds rt fmt rd first' = do
  let ws = within (rtProjectDir rt) (rtRoot rt)
      l = rdLanguage rd
      stop = initStop rt fmt
      finish = initFinish rt fmt (rdStart rd) (rdHead rd) (rdJson rd)
      doctorRows = initDoctorRows rt l
      repairWith written rows = do
        rtErr rt (T.unlines ["init-language: round 1: " <> checkName c <> " — " <> checkDetail c | c <- rows])
        let rreq = LI.repairRequest (rdRequest rd) written rows
        runFill (rdFiller rd) rreq <&> \case
          Left why -> (LI.Answered [] [Check (l <> ": repair") Bad ("the model did not answer the repair: " <> why) Nothing] (LI.anAnswers first') [], [])
          -- the repair's values over the first round's, the files assembled
          -- again; a key not required that still fails is left out
          Right answers ->
            let repaired = LI.repairAnswers rreq answers
             in (rdLeave rd (rdCheck rd (M.union repaired (LI.anAnswers first'))), M.keys repaired)
      -- FIELDS back to the first round's values (absent there: absent)
      revert answers fields = foldr (\f -> maybe (M.delete f) (M.insert f) (M.lookup f (LI.anAnswers first'))) answers fields
      merge old new' = [(p, t) | (p, t) <- old, p `notElem` map fst new'] ++ new'
      -- the first round's files with a wrong key that is not required left
      -- out: when they load, doctor sees them before the one repair, so it
      -- answers the init rows and doctor's at once
      provisional = rdLeave rd first'
  if not (null (LI.anFailing provisional))
    then do
      -- a required key or a whole file fails: nothing is written until every file loads
      (second, _) <- repairWith (LI.anFiles first') (LI.anFailing first')
      if not (null (LI.anFailing second))
        then stop "the model's files do not load after one repair; nothing was written" Nothing (LI.anFailing second)
        else do
          let written = merge (LI.anFiles first') (LI.anFiles second)
          rdBeforeWrite rd
          forM_ written $ \(p, t) -> writeText ws p t
          rows <- LI.languageRows l (map fst written) <$> doctorRows
          finish written True (rows ++ LI.anWarnings second) Nothing
    else do
      let files1 = merge (LI.anFiles first') (LI.anFiles provisional)
          ownFiles = map fst files1
      rdBeforeWrite rd
      forM_ files1 $ \(p, t) -> writeText ws p t
      rows1 <- doctorRows
      let before = LI.languageRows l ownFiles rows1
      -- a warn row that does not stop the files (a command naming no
      -- {= tests =}) is shown, not repaired: doctor's rows say whether it runs
      case LI.anFailing first' ++ rdFailing rd ownFiles rows1 of
        [] -> finish files1 False (before ++ LI.anWarnings provisional) Nothing
        failing -> do
          (second, repaired) <- repairWith files1 failing
          if null (LI.anFailing second)
            then do
              forM_ (LI.anFiles second) $ \(p, t) -> writeText ws p t
              rows2 <- LI.languageRows l ownFiles <$> doctorRows
              -- the repair may not break a row that passed: its fields go back
              case LI.regressedFields before rows2 repaired of
                [] -> finish (merge files1 (LI.anFiles second)) True (rows2 ++ LI.anWarnings second) Nothing
                back -> do
                  rtErr rt ("init-language: the repair broke a row that passed; the first round's " <> T.intercalate ", " back <> " kept\n")
                  let third = rdLeave rd (rdCheck rd (revert (LI.anAnswers second) back))
                  if null (LI.anFailing third)
                    then do
                      forM_ (LI.anFiles third) $ \(p, t) -> writeText ws p t
                      rows3 <- LI.languageRows l ownFiles <$> doctorRows
                      finish (merge files1 (LI.anFiles third)) True (rows3 ++ LI.anWarnings third) Nothing
                    else finish (merge files1 (LI.anFiles second)) True (rows2 ++ LI.anWarnings second) Nothing
            else finish files1 True (before ++ LI.anFailing second ++ LI.anWarnings provisional) (Just "the repair's files did not load: the first round's files are kept")

-- | `init --language`'s stop: exit 1 with the line, a hint and the rows.
initStop :: Runtime -> Format -> Text -> Maybe Text -> [Check] -> IO ExitCode
initStop rt fmt msg hint rows = failWith rt fmt 1 "init_language" msg hint ["rows" .= initRowsJson rows]

-- | Doctor's rows, run, on the catalog opened again over what init wrote.
initDoctorRows :: Runtime -> Text -> IO [Check]
initDoctorRows rt l = do
  reopened <- openProjectCatalog (rtRoot rt) (CatalogPaths (rtProjectDir rt) (rtUserConfigDir rt) (rtHomeDir rt))
  case reopened of
    Left e -> pure [Check ("language " <> l) Bad (T.pack (show e)) Nothing]
    Right cat' -> doctorChecks (portsOf rt) (projectOf rt cat') True

-- | What init wrote and the final rows (Decision 10), with the stderr line
-- of the survey's and the errand's seconds; exit 1 when a row is an error.
initFinish :: Runtime -> Format -> (Double, Double) -> [Text] -> [Pair] -> [(FilePath, Text)] -> Bool -> [Check] -> Maybe Text -> IO ExitCode
initFinish rt fmt (t0, t1) heads json written repaired rows note = do
  t2 <- getMonotonicTime
  rtErr rt ("init-language: survey " <> secs (t1 - t0) <> ", errand " <> secs (t2 - t1) <> ", repair " <> (if repaired then "yes" else "no") <> "\n")
  let paths = map (T.pack . fst) written
  emit rt fmt
    (object (["written" .= paths, "rows" .= initRowsJson rows, "repaired" .= repaired] ++ json))
    (T.unlines (heads ++ ["wrote " <> w | w <- paths] ++ [n | Just n <- [note]] ++ map initRowLine rows))
  pure (if any ((== Bad) . checkStatus) rows then ExitFailure 1 else ExitSuccess)
  where
    secs t = T.pack (showFFloat (Just 1) t "") <> " s"

-- | Doctor's rows as its JSON's `checks`.
initRowsJson :: [Check] -> Value
initRowsJson rows = case doctorJson rows of
  Object o | Just v <- KM.lookup "checks" o -> v
  v -> v

-- | One of doctor's rows as doctor prints it, its fix on the next line.
initRowLine :: Check -> Text
initRowLine c = markOf (checkStatus c) <> " " <> checkName c <> " — " <> checkDetail c <> maybe "" ("\n  fix: " <>) (checkFix c)
  where
    markOf = \case Ok -> "✓" :: Text; Warn -> "!"; Bad -> "✗"

-- | The model that fills holes in a `gen` (spec §6.4): `--model`, else
-- the config's `default`; none with `--no-fill`.
modelFor :: Catalog -> GenOptions -> Either Text (Maybe (ModelFiller IO))
modelFor cat g
  | genNoFill g = Right Nothing
  | otherwise = modelFiller cat (genModel g)

-- | The engine's ports, as the CLI runs them.
portsOf :: Runtime -> Ports
portsOf rt = Ports (rtRoot rt) (rtSyntax rt) (rtChecker rt) (rtVerifier rt) (rtTester rt 120) (rtTester rt) (rtNow rt) (rtUserConfigDir rt) (rtHomeDir rt) (rtDoctor rt) (rtTracked rt) (rtBaseText rt) (rtBaseFiles rt) (rtBaseStamp rt)

-- | The project Olifant runs in, reading disk.
projectOf :: Runtime -> Catalog -> Project
projectOf rt cat = Project (rtProjectDir rt) cat (ignoring (catalogIgnores cat) (within (rtProjectDir rt) (rtRoot rt)))

-- | `.olifant/baseline.toml` (spec §5.10): (rule, file, fingerprint) entries.
readBaseline :: Text -> Either Text [((Text, Text, Text), Int)]
readBaseline src = do
  top <- Toml.parseToml src
  entries <- maybe (Right []) (Toml.list Toml.table) (M.lookup "baseline" top)
  traverse entry entries
  where
    entry t = do
      k <- (,,) <$> Toml.required "rule" Toml.text t <*> Toml.required "file" Toml.text t <*> Toml.required "fingerprint" Toml.text t
      n <- maybe 1 fromInteger <$> Toml.optional "count" Toml.int t
      pure (k, n)

-- | Changed lines per file from `git diff -U0 --src-prefix=a/ --dst-prefix=b/`
-- (1-based inclusive ranges; pure deletions have none). Review I6: the
-- prefixes are forced, so `diff.noprefix` and friends cannot hide changes.
parseDiffHunks :: Text -> [(FilePath, [(Int, Int)])]
parseDiffHunks = finish . foldl step (Nothing, [])
  . T.lines
  where
    step (cur, acc) l
      | Just p <- T.stripPrefix "+++ b/" l = (Just (T.unpack p), acc)
      | "+++ " `T.isPrefixOf` l = (Nothing, acc)
      | "@@ " `T.isPrefixOf` l, Just f <- cur, Just r <- range l = (cur, add f r acc)
      | otherwise = (cur, acc)
    add f r acc = case lookup f acc of
      Just rs -> [(g, if g == f then rs ++ [r] else x) | (g, x) <- acc]
      Nothing -> acc ++ [(f, [r])]
    finish (_, acc) = acc
    range l = case T.words l of
      (_ : _ : plus : _) | Just spec <- T.stripPrefix "+" plus ->
        let (start, count) = T.breakOn "," spec
         in case (reads (T.unpack start), if T.null count then Just 1 else readMaybeInt (T.drop 1 count)) of
              ([(c, "")], Just d) | d > 0 -> Just (c, c + d - 1)
              _ -> Nothing
      _ -> Nothing
    readMaybeInt t = case reads (T.unpack t) of
      [(n, "")] -> Just n
      _ -> Nothing

-- | One action as editors read it (spec §4.6).
actionJson :: (Text -> [Pair]) -> (Text -> Text) -> Action -> Value
actionJson suggestOf nameOf a = object (["generator" .= actGenerator a, "name" .= nameOf (actGenerator a), "label" .= actLabel a, "sets" .= M.fromList (actSets a), "missing" .= actMissing a, "choices" .= M.fromList (actChoices a)] ++ suggestOf (actGenerator a))

-- | A progress event as one JSON line (`gen --progress`).
progressJson :: ProgressEvent -> Value
progressJson = \case
  Checking lang -> object ["event" .= ("check" :: Text), "language" .= lang]
  Filling holes attempt model -> object ["event" .= ("fill" :: Text), "holes" .= holes, "attempt" .= attempt, "model" .= model]
  Filled hole attempt -> object ["event" .= ("filled" :: Text), "hole" .= hole, "attempt" .= attempt]
  Unfilled hole why -> object ["event" .= ("unfilled" :: Text), "hole" .= hole, "why" .= why]
  AutoImported hole line -> object ["event" .= ("auto_import" :: Text), "hole" .= hole, "import" .= line]
  Reindented hole fill -> object ["event" .= ("reindent" :: Text), "hole" .= hole, "fill" .= fill]
  CutToHole hole n fill -> object ["event" .= ("cut" :: Text), "hole" .= hole, "dropped" .= n, "fill" .= fill]
  WholeFile hole -> object ["event" .= ("whole_file" :: Text), "hole" .= hole]
  Verifying hole -> object ["event" .= ("verify" :: Text), "hole" .= hole]
  Said words -> object ["event" .= ("said" :: Text), "message" .= words]

-- | `olifant anticipate PATH --line N` (hidden; the prefix corpus calls
-- it): the one anticipation question at the line, as the language server
-- asks it, and one JSON line: the moment, every candidate (generator and
-- what it makes), the answer's choice and confidence, how long the ask
-- took, the unit's lines of code and whether it would be shown (sure
-- enough, and a suggestion comes of it); or {"none": why} when nothing is
-- asked. --overlay FILE is PATH's unsaved text (PATH new to the project);
-- --answers FILE, recorded answers instead of the model. Nothing else is
-- written to stdout; the step log is the language server's.
runAnticipate :: Runtime -> FilePath -> Int -> Maybe FilePath -> Maybe FilePath -> IO ExitCode
runAnticipate rt path line overlayFile answers = do
  buffer <- traverse (readText (rtRoot rt)) overlayFile
  case buffer of
    Just Nothing -> none 2 ("no overlay file " <> T.pack (fromMaybe "" overlayFile))
    _ -> do
      s0 <- newServerState (portsOf rt)
      let s1 = s0 {ssConfig = (ssConfig s0) {scDecideAnswers = answers}}
          s2 = maybe s1 (\t -> openDoc path 1 t s1) (join buffer)
      text <- maybe (fromMaybe "" <$> readText (rtRoot rt) path) pure (join buffer)
      let (unit, _, _) = prefixAt Nothing text line
      projectFor path s2 >>= \case
        (_, Nothing) -> none 1 "not in a project"
        (s3, Just (_, project)) -> case deciderFor s3 project of
          Nothing -> none 1 "no [decide] model"
          Just d ->
            anticipationAsk path line s3 >>= \case
              (s4, Nothing)
                | unitLines unit < minPrefixLines -> none 0 ("the unit has " <> tshow (unitLines unit) <> " lines of code, under minPrefixLines (" <> tshow minPrefixLines <> ")")
                | otherwise ->
                    recognitionFor path line s4 >>= \case
                      (_, Just sg) | Just r <- sgRecognition sg, Rec.rcBand r == Rec.Offer -> none 0 ("recognised: " <> generatorIdText (Rec.rcGenerator r))
                      _ -> none 0 "nothing to ask: one action applies here, or there are no candidates"
              (s4, Just (moment, _, q)) -> do
                t0 <- getMonotonicTime
                answer <- ask d q
                t1 <- getMonotonicTime
                (_, shown) <- anticipationFrom path line moment answer s4
                let candidates = case qShape q of
                      Choice os -> [object ["generator" .= g, "summary" .= what] | (g, what) <- os]
                      _ -> []
                    (chosen, confidence) = case answer of
                      Chosen g _ (Confidence c) -> (Just g, Just c)
                      Chosen g _ Unknown -> (Just g, Nothing)
                      _ -> (Nothing, Nothing)
                rtOut rt . (<> "\n") . encodeJson $
                  object
                    [ "moment" .= (case moment of Tie -> "tie"; Silence -> "silence" :: Text)
                    , "candidates" .= candidates
                    , "generator" .= chosen
                    , "confidence" .= confidence
                    , "ms" .= (round ((t1 - t0) * 1000) :: Int)
                    , "unit_lines" .= unitLines unit
                    , "shown" .= isJust shown
                    , "kind" .= fmap (sgKind . anSuggestion) shown
                    ]
                pure ExitSuccess
  where
    none code why = do
      rtOut rt (encodeJson (object ["none" .= (why :: Text)]) <> "\n")
      pure (if code == 0 then ExitSuccess else ExitFailure code)
    tshow = T.pack . show
