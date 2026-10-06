-- | The `olifant` command line: parses arguments, runs catalog/generation
-- use cases, and renders JSON or human output with the spec's exit codes.
module Olifant.Cli (Runtime (..), DoctorPorts (..), runCli, parseDiffHunks) where

import Control.Applicative ((<|>))
import Olifant.Fill (ModelFiller (..))
import Olifant.Rules (Exception (..), FixDef (..), FixOffer (..), Finding (..), Replacement (..), RuleDef (..), RulesConfig (..), checkFiles, markBaselined, suggestChoice)
import Olifant.Rules qualified as R
import Olifant.Toml qualified as Toml
import Olifant.Doctor (Check (..), DoctorInput (..), DoctorPorts (..), Status (..), doctor)
import Olifant.Actions (Action (..), Completion (..), FileAction (..), Nearby (..), Range (..), actionsAt, actionsInFile, completionsAt, matchPathSlots)
import Olifant.Model (ModelConfig (..), backendModel, curlFiller)
import Control.Monad (forM_, unless, when)
import Olifant.Holes (HoleSite (..))
import Olifant.Snippet (Snippet, snippetOf)
import Data.Aeson (Value, eitherDecodeStrict, object, toJSON)
import Data.Aeson.Types (Pair, (.=))
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Functor ((<&>))
import Data.List (find, sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Olifant.Template (describeTemplateError, render)
import Olifant.Template qualified as Tm
import Olifant.Template.Syntax (Segment (..), Template (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Olifant.Anchor (Located (..), RenderedAnchor (..), astQueries, describeMisses, insertLines, parsePhrase, resolveLayers)
import Olifant.Region (RegionDef, RegionDefWith (..), RegionProblem (..), applyRegion, fenceLines, wholeHeader)
import Olifant.Catalog
import Olifant.Cli.Options
import Olifant.Cli.Output
import Olifant.Generator.Schema (generatorSchema)
import Olifant.Generation
import Olifant.Cursor (afterIndent, afterRule, atCursor, atCursorIndented, cursorIndent, isSnippet)
import Olifant.Uncovered (uncoveredQuery)
import Olifant.Generator (AppliesAt (..), Generator, generatorName, replaceGenerator, ChoiceView (..), Choices (..), EnvError (..), GeneratorId (..), GeneratorWith (..), InputSpec (..), InputType (..), Scalar (..), UseStep (..), choiceViews, generatorIdText, needsChoices, withChoices)
import Olifant.Query (Queries (..), QueryFailure (..), processCommand)
import Olifant.Language (Checker, Language (..), Syntax, languageFor)
import Olifant.Workspace (PathState (..), Workspace (..), isSafeRelative, overlay, within)
import System.FilePath (makeRelative, normalise, splitDirectories, takeBaseName, takeDirectory, takeFileName, (</>))
import System.Exit (ExitCode (..))

data Runtime = Runtime
  { rtRoot :: Workspace IO
  , rtProjectDir :: FilePath
  , rtUserConfigDir :: Maybe FilePath
  , rtHomeDir :: Maybe FilePath
  , rtReadStdin :: IO Text
  , rtNow :: IO UTCTime
  , rtStdoutIsTty :: Bool
  , rtOut :: Text -> IO ()
  , rtErr :: Text -> IO ()
  , rtSyntax :: Syntax IO
  , rtChecker :: Checker IO
  , rtDoctor :: DoctorPorts IO
  , -- | lines changed since a git ref (file, 1-based inclusive ranges) and
    -- untracked files, for `check --changed-since`
    rtGitChanged :: FilePath -> Text -> IO (Either Text ([(FilePath, [(Int, Int)])], [FilePath]))
  , -- | a first commit of everything in a directory, `git init` first if
    -- needed (`olifant init`)
    rtGitInit :: FilePath -> Text -> IO (Either Text ())
  , -- | runs a hole's `verify` command (its tests) in a scratch copy
    rtVerifier :: Verifier IO
  }

runCli :: Runtime -> [String] -> IO ExitCode
runCli rt args = case parseOptions args of
  Left (text, ExitSuccess) -> rtOut rt text >> pure ExitSuccess
  Left (text, _)
    | wantsJson -> rtOut rt (encodeJson (errorJson "usage" (T.strip text) []) <> "\n") >> pure (ExitFailure 2)
    | otherwise -> rtErr rt text >> pure (ExitFailure 2)
  -- `init` makes a project: there is none to open yet
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
                    code <- runSteps rt' dir fmt cat (GenOptions [] Nothing plan Nothing Nothing False False Nothing Nothing) [] [Step gen (M.fromList [("name", name)])]
                    if code /= ExitSuccess || plan
                      then pure code
                      else do
                        -- every region whose files the starter wrote gets its fences
                        let project = within dir ws
                            tools = projectTools rt' cat
                        universe <- listFiles project "."
                        forM_ (catalogRegions cat) $ \def ->
                          forM_ (regionInstances universe def) (initFences project tools)
                        committed <- rtGitInit rt dir ("olifant init: " <> packName)
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
       in if null ls then "" else "\n" <> T.unlines ls

-- | The project is the nearest directory, from where Olifant runs upwards,
-- that has a `.olifant` directory; with none, where it runs. The search
-- stops below the home directory (user config is ~/.config/olifant).
projectRoot :: Monad m => Workspace m -> Maybe FilePath -> FilePath -> m FilePath
projectRoot ws home start = go start
  where
    go dir
      | Just dir == home = pure start
      | otherwise =
          inspectPath ws (dir </> ".olifant") >>= \case
            Directory -> pure dir
            _ | takeDirectory dir == dir -> pure start
              | otherwise -> go (takeDirectory dir)

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
  ListCmd detail -> do
    -- Sources and fixed targets are checked here, so a broken generator shows
    -- as INVALID before anyone runs it.
    checked <- traverse (\e -> (e,) <$> checkEntry (rtRoot rt) (rtSyntax rt) cat e) (catalogEntries cat)
    let broken =
          catalogBroken cat
            ++ [(entryId e, entryDir e </> "generator.toml", why) | (e, Just why) <- checked]
            ++ [(GeneratorId ("language " <> T.pack (takeBaseName path)), path, why) | (path, why) <- catalogBrokenLanguages cat]
        ok = [e | (e, Nothing) <- checked]
    -- --detail prints inputs, so choices_from must be live, as in `show`.
    entries <-
      if detail
        then traverse (\e -> either (const e) (\d -> e {entryDefinition = d}) <$> resolveChoicesFor rt cat M.empty (entryDefinition e)) ok
        else pure ok
    viewList <- if detail then traverse (\e -> (entryId e,) . either (const M.empty) M.fromList <$> choiceViewsFor rt cat M.empty (entryDefinition e)) ok else pure []
    let views e = M.findWithDefault M.empty (entryId e) (M.fromList viewList)
    emit rt fmt (listJson detail (subsOf cat) views entries broken) (listHuman detail (subsOf cat) views entries broken)
    pure ExitSuccess
  ActionsCmd file at fromStdin -> do
    -- Paths are relative to where olifant runs; matching uses the
    -- project-relative path.
    let project = within (rtProjectDir rt) (rtRoot rt)
        rel = makeRelative (rtProjectDir rt) (normalise (cwd </> file))
    content <- if fromStdin then Just <$> rtReadStdin rt else readText (rtRoot rt) (cwd </> file)
    paths <- listFiles project "."
    let gens = [(generatorIdText (entryId e), entryDefinition e) | e <- catalogEntries cat]
        nameOf gid = maybe gid generatorName (lookup gid gens)
        -- how an editor may offer it: unasked (suggest), as a snippet, on a trigger (spec §4.8, §4.9)
        suggestOf gid =
          let g = lookup gid gens
              ap = genAppliesAt =<< g
           in ["suggest" .= maybe False apSuggest ap, "snippet" .= maybe False isSnippet g, "trigger" .= maybe False (isJust . apTrigger) ap]
        human a = actLabel a <> "  (" <> actGenerator a <> ")" <> (if null (actMissing a) then "" else "  asks: " <> T.intercalate ", " (actMissing a))
    case at of
      Just pos -> do
        let here = maybe project (\t -> overlay rel t project) content
        acts <- actionsAt (rtSyntax rt) (catalogLanguages cat) (queriesAt rt cat here rel (fst pos)) paths (readText project) gens rel pos (fromMaybe "" content)
        emit rt fmt (toJSON (map (actionJson suggestOf nameOf) acts)) (T.unlines (map human acts))
      Nothing -> do
        (acts, near) <- actionsInFile (rtSyntax rt) (catalogLanguages cat) (projectQueries rt cat) paths (readText project) gens rel (fromMaybe "" content)
        let rangeJson (Range (l1, c1) (l2, c2)) = object ["start" .= [l1, c1], "end" .= [l2, c2]]
            json = object
              [ "actions" .= [object (["generator" .= actGenerator a, "name" .= nameOf (actGenerator a), "label" .= actLabel a, "range" .= fmap rangeJson r, "sets" .= M.fromList (actSets a), "missing" .= actMissing a] ++ suggestOf (actGenerator a)) | FileAction a r <- acts]
              , "nearby" .= [object ["generator" .= nbGenerator n, "name" .= nameOf (nbGenerator n), "label" .= nbLabel n, "why" .= nbWhy n] | n <- near]
              ]
            where' r = maybe "file  " (\(Range (l, c) _) -> T.pack (show l) <> ":" <> T.pack (show c) <> "  ") r
        emit rt fmt json (T.unlines ([where' r <> human a | FileAction a r <- acts] ++ ["nearby: " <> nbGenerator n <> " — " <> nbWhy n | n <- near]))
    pure ExitSuccess
  CompleteCmd file pos fromStdin -> do
    let project = within (rtProjectDir rt) (rtRoot rt)
        rel = makeRelative (rtProjectDir rt) (normalise (cwd </> file))
    content <- if fromStdin then Just <$> rtReadStdin rt else readText (rtRoot rt) (cwd </> file)
    paths <- listFiles project "."
    let gens = [(generatorIdText (entryId e), entryDefinition e) | e <- catalogEntries cat]
        described gid = maybe ("", gid) (\g -> (genDescription g, generatorName g)) (lookup gid gens)
    let here = maybe project (\t -> overlay rel t project) content
    items <- completionsAt (rtSyntax rt) (catalogLanguages cat) (queriesAt rt cat here rel (fst pos)) paths (readText project) gens rel pos (fromMaybe "" content)
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
        offerJson o = object ["generator" .= foGenerator o, "name" .= nameOf (foGenerator o), "label" .= fromMaybe (nameOf (foGenerator o)) (foLabel o), "sets" .= M.fromList (foSets o)]
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
    let chooseInputs = chooseFixInputs rt cat paths now
    case file of
      Just f -> do
        let rel = makeRelative (rtProjectDir rt) (normalise (cwd </> f))
        content <- if fromStdin then Just <$> rtReadStdin rt else readText (rtRoot rt) (cwd </> f)
        (fs, exs) <- checkFiles (rtSyntax rt) (catalogLanguages cat) (catalogRulesConfig cat) rules paths (\p -> if p == rel then pure content else readText ws p) [rel] >>= chooseInputs
        let olds = baselinedIn fs
        emit rt fmt (object (["findings" .= map (findingJson olds) fs, "exceptions" .= map exceptionJson exs] ++ reportJson fs)) (T.unlines (map (findingHuman olds) fs))
        pure ExitSuccess
      Nothing | baseline -> do
        let checked = [p | p <- paths, any (\r -> isJust (matchPathSlots (ruleFiles r) p)) rules]
        (fs, _) <- checkFiles (rtSyntax rt) (catalogLanguages cat) (catalogRulesConfig cat) rules paths (readText ws) checked
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
          Nothing -> refreshRegions ReportUnset (projectTools rt cat) ws 1 M.empty
        changed <- traverse (rtGitChanged rt (rtProjectDir rt)) changedSince
        let checked = [p | p <- paths, any (\r -> isJust (matchPathSlots (ruleFiles r) p)) rules]
            touched f = case changed of
              Nothing -> True
              Just (Left _) -> True
              Just (Right (ranges, untracked)) ->
                fFile f `elem` untracked
                  || case (lookup (fFile f) ranges, fRange f) of
                    (Nothing, _) -> False
                    (Just _, Nothing) -> True
                    (Just rs, Just ((l1, _), (l2, _))) -> any (\(a, b) -> a <= l2 && l1 <= b) rs
        (fs0, exs) <- checkFiles (rtSyntax rt) (catalogLanguages cat) (catalogRulesConfig cat) rules paths (readText ws) checked >>= chooseInputs
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
                  ++ [ if failed
                         then "check failed"
                         else
                           let n = length fs
                               old = length olds
                            in "conventions hold (" <> T.pack (show n) <> " finding" <> (if n == 1 then "" else "s") <> ", " <> (if old > 0 then T.pack (show old) <> " existing" else "none blocking") <> ")"
                     ]
        emit rt fmt (object (["regions" .= regionsJson, "findings" .= map (findingJson olds) fs, "exceptions" .= map exceptionJson exs, "errors" .= (broken ++ gitProblem)] ++ reportJson fs)) human
        pure (if failed then ExitFailure 1 else ExitSuccess)
  DoctorCmd -> do
    checks <-
      doctor (rtDoctor rt) $
        DoctorInput
          (catalogProject cat)
          (catalogPacks cat)
          (catalogLanguages cat)
          [(T.pack p, why) | (p, why) <- catalogBrokenLanguages cat]
          [(generatorIdText g, why) | (g, _, why) <- catalogBroken cat]
          (catalogModel cat)
    let status = \case Ok -> "ok" :: Text; Warn -> "warn"; Bad -> "error"
        mark = \case Ok -> "✓" :: Text; Warn -> "!"; Bad -> "✗"
        json = object ["checks" .= [object ["name" .= checkName c, "status" .= status (checkStatus c), "detail" .= checkDetail c, "fix" .= checkFix c] | c <- checks]]
    emit rt fmt json (T.unlines (concat [[mark (checkStatus c) <> " " <> checkName c <> " — " <> checkDetail c] ++ ["  fix: " <> f | Just f <- [checkFix c]] | c <- checks]))
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
  ShowCmd ref setList -> case findEntry cat ref of
    Left err -> catalogFailure rt fmt err
    Right e0 ->
      checkEntry (rtRoot rt) (rtSyntax rt) cat e0 >>= \case
       Just why -> failWith rt fmt 1 "generator_invalid" (generatorIdText (entryId e0) <> ": " <> why) Nothing []
       Nothing -> do
        let sets = M.fromList setList
        resolved <- resolveChoicesFor rt cat sets (entryDefinition e0)
        views <- choiceViewsFor rt cat sets (entryDefinition e0)
        case (resolved, views) of
          (Left why, _) -> failWith rt fmt 1 "generator_invalid" why Nothing []
          (_, Left why) -> failWith rt fmt 1 "generator_invalid" why Nothing []
          (Right d, Right vs) -> let e = e0 {entryDefinition = d} in emit rt fmt (showJson (M.fromList vs) (subsOf cat e) e) (showHuman (M.fromList vs) (subsOf cat e) e) >> pure ExitSuccess
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
          resolveLayers (rtSyntax rt) (languageFor (catalogLanguages cat) file) [PhraseAnchor phrase] content >>= \case
            Left misses -> failWith rt fmt 5 "anchor_unresolved" (describeMisses misses) (Just "`olifant anchors <language>` lists the targets") []
            Right loc -> emit rt fmt (whereJson file phrase loc content) (whereHuman file loc content) >> pure ExitSuccess
  SyncCmd mode -> do
    now <- rtNow rt
    let ws = within (rtProjectDir rt) (rtRoot rt)
        tools = projectTools rt cat
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
            Right gens -> runSteps rt cwd fmt cat g [] (zipWith (\gen s -> Step gen (M.fromList (specSets s))) gens ss)
  FixCmd rule file (line, _) n g -> do
    let ws = within (rtProjectDir rt) (rtRoot rt)
        rel = makeRelative (rtProjectDir rt) (normalise (cwd </> file))
    paths <- listFiles ws "."
    now <- rtNow rt
    (fs, _) <- checkFiles (rtSyntax rt) (catalogLanguages cat) (catalogRulesConfig cat) (catalogRules cat) paths (readText ws) [rel] >>= chooseFixInputs rt cat paths now
    let here = [f | f <- fs, fRule f == rule, Just ((l1, _), (l2, _)) <- [fRange f], l1 <= line, line <= l2]
    case here of
      [] -> failWith rt fmt 3 "finding_not_found" ("no `" <> rule <> "` finding at " <> T.pack rel <> ":" <> T.pack (show line)) (Just "run `olifant check --file` to see the findings") []
      f : _ -> case drop (n - 1) (fFixes f) of
        [] -> failWith rt fmt 2 "usage" ("`" <> rule <> "` offers " <> T.pack (show (length (fFixes f))) <> " fix(es); there is no fix " <> T.pack (show n)) Nothing []
        o : _ -> case findEntry cat (foGenerator o) of
          Left err -> catalogFailure rt fmt err
          Right entry ->
            loadGenerator (rtRoot rt) (rtSyntax rt) cat entry >>= \case
              Left err -> catalogFailure rt fmt err
              Right gen -> do
                let sets = M.fromList (foSets o)
                    env = M.map Tm.VText (M.insert "finding" (maybe "" rpFinding (foReplace o)) sets)
                    defs = [d | r <- catalogRules cat, ruleId r == rule, d <- drop (n - 1) (ruleFixes r)]
                    replacement = case foReplace o of
                      Nothing -> Right []
                      Just rp -> do
                        rendered <- first describeTemplateError (render env (rpText rp))
                        let text = T.intercalate "\n" [if T.null l then l else rpIndent rp <> l | l <- T.lines rendered]
                        pure [Step (replaceGenerator rel (rpLines rp) (rpOld rp) (Template text [Lit text])) M.empty]
                context <- fmap catMaybes . traverse (\t -> either (const (pure Nothing)) (\p -> fmap (T.unpack p,) <$> readText ws (T.unpack p)) (render env t)) $ concatMap fdContext (take 1 defs)
                case replacement of
                  Left why -> failWith rt fmt 1 "fix_failed" why Nothing []
                  -- The replacement first, while its lines are where the rule saw them.
                  Right first' -> runSteps rt cwd fmt cat g context (first' ++ [Step gen sets])

-- | Snippets (spec §5.3): each cursor edit becomes a replacement of the
-- `--at` line (the overlay's text when there is one), indented to fit.
placeCursor :: Runtime -> FilePath -> Catalog -> GenOptions -> [Generator] -> IO (Either Text [Generator])
placeCursor rt cwd cat g gens = case (genAt g, filter isSnippet gens) of
  (_, []) -> pure (Right gens)
  (Nothing, s : _) -> pure (Left ("`" <> generatorIdText (genId s) <> "` is a snippet: it needs --at FILE:LINE:COL"))
  (Just (file, line, _), _) -> do
    let rel = makeRelative (rtProjectDir rt) (normalise (cwd </> file))
        overlaid = fmap (\f -> makeRelative (rtProjectDir rt) (normalise (cwd </> f))) (genOverlay g) == Just rel
        lang = languageFor (catalogLanguages cat) rel
        unit = maybe "    " langIndent lang
    text <- if overlaid then Just <$> rtReadStdin rt else readText (within (rtProjectDir rt) (rtRoot rt)) rel
    case text of
      Nothing -> pure (Left (T.pack rel <> " does not exist"))
      Just t
        | let n = length (T.splitOn "\n" t), line < 1 || line > n ->
            pure (Left ("--at " <> T.pack rel <> ":" <> T.pack (show line) <> ": line " <> T.pack (show line) <> " is outside " <> T.pack rel <> " (lines 1–" <> T.pack (show n) <> ")"))
        | otherwise -> do
            let ls = T.splitOn "\n" t
            sequence <$> traverse (place ls lang unit rel line) gens
  where
    -- a snippet anticipated `after` a node takes that node's indentation
    place ls lang unit rel line gen
      | not (isSnippet gen) = pure (Right gen)
      | Just after <- afterRule =<< apAfter =<< genAppliesAt gen, Just l <- lang =
          afterIndent (rtSyntax rt) l after ls line <&> \case
            Left why -> Left ("cannot place `" <> generatorIdText (genId gen) <> "`: " <> why <> " (run `olifant doctor`)")
            Right ind -> Right (atCursorIndented (fromMaybe (cursorIndent unit ls line) ind) rel line ls gen)
      | otherwise = pure (Right (atCursor unit rel line ls gen))

-- | Run plan steps as `gen` does (model, fills, progress, regions,
-- output): `gen` itself, and `fix` (a replacement, then the fix's generator,
-- with the fix's context files shown to the model).
runSteps :: Runtime -> FilePath -> Format -> Catalog -> GenOptions -> [(FilePath, Text)] -> [Step] -> IO ExitCode
runSteps rt cwd fmt cat g context steps = do
  let snippetAt = snippetAtCursor rt cwd g
  now <- maybe (rtNow rt) pure (genNow g)
  let mode = if genPlanOnly g then PlanOnly else Apply
      project = within (rtProjectDir rt) (rtRoot rt)
  -- `--overlay FILE` (spec §4.8): plan against the editor's unsaved text.
  overlaid <- case genOverlay g of
    Nothing -> pure (Right project)
    Just _ | not (genPlanOnly g) -> pure (Left "--overlay needs --plan (an unsaved buffer is planned, never written)")
    Just f -> do
      text <- rtReadStdin rt
      pure (Right (overlay (makeRelative (rtProjectDir rt) (normalise (cwd </> f))) text project))
  case (overlaid, modelFor cat g) of
   (Left why, _) -> failWith rt fmt 2 "usage" why Nothing []
   (Right ws, filler0) -> case filler0 of
             Left why -> failWith rt fmt 2 "usage" why Nothing []
             Right filler ->
              generate ws (resolver (rtRoot rt) (rtSyntax rt) cat) ((projectTools rt cat) {toolQueries = maybe (projectQueries rt cat) (\(f, l, _) -> queriesAt rt cat ws (makeRelative (rtProjectDir rt) (normalise (cwd </> f))) l) (genAt g), toolFiller = filler, toolFillContext = context, toolProgress = if genProgress g then rtErr rt . (<> "\n") . encodeJson . progressJson else const (pure ())}) steps now mode >>= \case
              Left err -> generationFailure rt fmt err
              Right plan0 -> do
                let plan =
                      (if genNoFill g then noFill plan0 else plan0)
                        { planSnippet = snippetAt plan0
                        , planWarnings =
                            planWarnings plan0
                              ++ ["regions are not refreshed from " <> T.pack path <> ": " <> err | (path, err) <- catalogBrokenRegions cat]
                              ++ ["language adapter " <> T.pack path <> " failed to load: " <> err | (path, err) <- catalogBrokenLanguages cat]
                        }
                    applied = mode == Apply && not (hasConflicts plan)
                emit rt fmt (planJson applied plan) (planHuman applied plan)
                pure (if hasConflicts plan then ExitFailure 5 else ExitSuccess)

-- | The snippet a `gen --at` plan placed (spec §4.9): the lines that replaced
-- the cursor's line, holes as tab stops, model fills where there are any.
snippetAtCursor :: Runtime -> FilePath -> GenOptions -> Plan -> Maybe Snippet
snippetAtCursor rt cwd g plan = do
  (file, line, _) <- genAt g
  let rel = makeRelative (rtProjectDir rt) (normalise (cwd </> file))
  text <- maybe (M.lookup rel (planWrites plan)) Just (M.lookup rel (planMarked plan))
  -- as many lines as the replacement of the cursor's line has
  k <- listToMaybe [max 1 (length (T.lines new)) | e <- planEdits plan, editPath e == rel, WillReplace l _ new <- [editAction e], l == line]
  let lines' = take k (drop (line - 1) (T.splitOn "\n" text))
      fills = [(T.takeWhile (/= '[') (holeName (phSite h)), f) | h <- planHoles plan, holeFile (phSite h) == rel, Just f <- [phFill h]]
  Just (snippetOf rel line (T.intercalate "\n" lines') (`lookup` fills))

-- | A usage-level failure inside a command: JSON on stdout in JSON mode.
usageIn :: Runtime -> Format -> Text -> IO ExitCode
usageIn rt fmt msg = do
  case fmt of
    Json -> rtOut rt (encodeJson (errorJson "usage" msg []) <> "\n")
    Human -> rtErr rt (msg <> "\n")
  pure (ExitFailure 2)

-- | A generator's [[uses]] steps with the entries they resolve to, so `show`
-- can describe a composite completely in one call.
subsOf :: Catalog -> CatalogEntry -> [(UseStep, Maybe CatalogEntry)]
subsOf cat e = [(u, either (const Nothing) Just (resolveEntry cat (packOfEntry e) (useGenerator u))) | u <- genUses (entryDefinition e)]

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

-- | Resolve `choices_from` inputs against the project (spec §4.1); the
-- project is listed only when a generator needs it.
resolveChoicesFor :: Runtime -> Catalog -> Map Text Text -> GeneratorWith s -> IO (Either Text (GeneratorWith s))
resolveChoicesFor rt cat sets gen
  | not (needsChoices gen) = pure (Right gen)
  | otherwise = do
      paths <- listFiles (within (rtProjectDir rt) (rtRoot rt)) "."
      now <- rtNow rt
      first failureWhy <$> withChoices (projectQueries rt cat) paths (readText (within (rtProjectDir rt) (rtRoot rt))) sets now gen

-- | `show`'s view of each enum's choices (spec §4.1); the project is listed
-- only when a generator has a `one_of`.
choiceViewsFor :: Runtime -> Catalog -> Map Text Text -> GeneratorWith s -> IO (Either Text [(Text, ChoiceView)])
choiceViewsFor rt cat sets gen
  | not (needsChoices gen) = pure (Right [(inputName i, Flat cs) | i@InputSpec {inputType = ScalarInput (EnumS cs)} <- genInputs gen])
  | otherwise = do
      paths <- listFiles (within (rtProjectDir rt) (rtRoot rt)) "."
      now <- rtNow rt
      first failureWhy <$> choiceViews (projectQueries rt cat) paths (readText (within (rtProjectDir rt) (rtRoot rt))) sets now gen

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
                resolveLayers (toolSyntax tools) lang [PhraseAnchor phrase] content >>= \case
                  Left misses -> pure (Left (5, "anchor_unresolved", T.pack file <> ": " <> describeMisses misses))
                  Right loc -> do
                    let (open, close) = fenceLines def c
                        placed = maybe loc (\i -> loc {locIndent = i}) (regionInitIndent def)
                    writeText ws file (snd (insertLines placed (open <> "\n" <> close) content))
                    pure (Right True)
  where
    file = regionFile def
    lang = languageFor (toolLanguages tools) file

-- | The model that fills holes in a `gen` (spec §6.4): `--model`, else
-- `[model] default`; none with `--no-fill` or without `[model]`.
modelFor :: Catalog -> GenOptions -> Either Text (Maybe (ModelFiller IO))
modelFor cat g
  | genNoFill g = Right Nothing
  | otherwise = case genModel g <|> mcDefault config of
      Nothing -> Right Nothing
      Just name -> case lookup name (mcBackends config) of
        Nothing -> Left ("--model " <> name <> ": no such backend under [model.backends] (backends: " <> (if null (mcBackends config) then "none" else T.intercalate ", " (map fst (mcBackends config))) <> ")")
        Just b -> Right (Just (ModelFiller (backendModel b) (mcRetries config) (curlFiller (mcTimeout config) b)))
  where
    config = catalogModel cat

-- | `--no-fill`: holes were left on purpose.
noFill :: Plan -> Plan
noFill p = p {planHoles = [h {phStatus = HoleSkipped "--no-fill"} | h <- planHoles p]}

-- | The tools a command in this project uses: the runtime's syntax and
-- checker, the catalog's languages and regions, and the project's queries.
projectTools :: Runtime -> Catalog -> Tools IO
projectTools rt cat = (basicTools (rtSyntax rt) (rtChecker rt) (catalogLanguages cat)) {toolRegions = catalogRegions cat, toolQueries = projectQueries rt cat, toolVerifier = rtVerifier rt}

-- | Queries run in the project: commands in its root, AST queries over its
-- files with the catalog's language adapters.
-- | 'projectQueries' with a cursor: `uncovered` queries ask the compiler
-- about the match there (spec §4.9), reading through @ws@ (an overlay).
queriesAt :: Runtime -> Catalog -> Workspace IO -> FilePath -> Int -> Queries IO
queriesAt rt cat ws file line = (projectQueries rt cat) {queryUncovered = uncoveredQuery (rtChecker rt) (catalogLanguages cat) ws file line}

projectQueries :: Runtime -> Catalog -> Queries IO
projectQueries rt cat =
  Queries
    { queryCommand = processCommand (rtProjectDir rt)
    , queryAst = astQueries (rtSyntax rt) (catalogLanguages cat)
    , queryUncovered = \_ -> pure (Right [])
    }

-- | A fix's enum inputs left open are pre-chosen when the finding's code
-- names exactly one choice (`invoices::table` → invoice_repository), and the
-- label says which (beat 4 feedback).
chooseFixInputs :: Runtime -> Catalog -> [FilePath] -> UTCTime -> ([Finding], a) -> IO ([Finding], a)
chooseFixInputs rt cat paths now (fs, exs) = (,exs) <$> traverse (\f -> (\os -> f {fFixes = os}) <$> traverse (choose f) (fFixes f)) fs
  where
    ws = within (rtProjectDir rt) (rtRoot rt)
    nameOf gid = maybe gid (generatorName . entryDefinition) (find ((== gid) . generatorIdText . entryId) (catalogEntries cat))
    choose f o = case find ((== foGenerator o) . generatorIdText . entryId) (catalogEntries cat) of
      Nothing -> pure o
      Just e ->
        withChoices (projectQueries rt cat) paths (readText ws) (M.fromList (foSets o)) now (entryDefinition e) <&> \case
          Left _ -> o
          Right g ->
            let picks = [(inputName i, c) | i <- genInputs g, inputName i `notElem` map fst (foSets o), ScalarInput (EnumOf _ _ (Resolved cs _)) <- [inputType i], Just c <- [suggestChoice (fText f) cs]]
             in case picks of
                  (k, c) : _ -> o {foSets = sortOn fst ((k, c) : foSets o), foLabel = Just (fromMaybe (nameOf (foGenerator o)) (foLabel o) <> ": " <> toPascal c)}
                  [] -> o

-- | `invoice_repository` → `InvoiceRepository`.
toPascal :: Text -> Text
toPascal = T.concat . map (\w -> T.toUpper (T.take 1 w) <> T.drop 1 w) . T.splitOn "_"

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
  Verifying hole -> object ["event" .= ("verify" :: Text), "hole" .= hole]
