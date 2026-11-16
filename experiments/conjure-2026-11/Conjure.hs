{-# LANGUAGE OverloadedStrings, LambdaCase #-}
-- | Conjure 2026-11 (experiment, not product code): one conjure step per cursor situation, three
-- prompt shapes by four writers, judged by Opus. Brief: notes/durandal/conjure-measure-brief-2026-11-15.md.
--
-- Shapes: A today's conjure (the engine's 'conjureSystemPrompt' and 'userPrompt' over the request
-- 'functionConjure' builds, copied below because Olifant.Server.Conjure does not export it), one
-- round; B a strategist (haiku) names the next small goal, then the writer takes it; C the writer
-- names the goal and takes it, one call. B and C answer whole files (any file), so scope is measured.
--
-- Modes (cwd = the olifant checkout; CONJURE_TREES = the pristine case trees, make_cases.py;
-- CONJURE_WORK = a scratch folder for the stepped copies):
--   conjure write WRITER...   each case x writer x shape: ask (or replay), apply, test, measure
--   conjure judge             each case: Opus judges every run blind, from the rubric
-- replies.json holds every model reply keyed "VARIANT|MODEL|CASE" (STRATEGIST|..., JUDGE|...);
-- results.json the cases, the runs and the verdicts. CONJURE_LIVE=1 asks what is not recorded;
-- without it an unrecorded reply fails. Live, it stops before a call once the recorded cost
-- passes CONJURE_CAP (default 8 dollars). A run already in results.json is kept. CONJURE_CASES
-- (comma-separated ids) limits a run to those cases.
module Main (main) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, forM_, forever, unless, when)
import Data.Aeson (FromJSON (..), Value (..), eitherDecodeFileStrict, eitherDecodeStrict, encode, object, withObject, (.:), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Bits (xor)
import Data.ByteString.Lazy qualified as BL
import Data.Char (isDigit, ord)
import Data.Foldable (toList)
import Data.IORef
import Data.List (nub, sortOn, (\\))
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import GHC.Clock (getMonotonicTime)
import Olifant.App (Ports (..), Project (..))
import Olifant.Doctor (DoctorPorts (..))
import Olifant.Fill (FillHole (..), FillRequest (..), Stance (..))
import Olifant.Generation.FillPhase (fillLines, markerOnly, reindented)
import Olifant.Language (ImportsDef (..), Language (..), TestRun (..), Failure (..), failureLineIn, noChecker, processRunner)
import Olifant.Language.AstGrep (astGrepSyntax)
import Olifant.Language.Check (noVerifier)
import Olifant.Language.Grammar (GrammarFailure (NoSource))
import Olifant.Language.TestUnits (testSourceIn)
import Olifant.Model (Backend (..), conjureSystemPrompt, decodeFills, userPrompt)
import Olifant.Process (gitStateStamp)
import Olifant.Server (newServerState)
import Olifant.Server.Site (Site (..), importedContext, runOver, siteAt)
import Olifant.Server.State (ServerState)
import Olifant.Server.Unit (isStubIn)
import Olifant.Workspace (diskWorkspace)
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Text.Printf (printf)

here, repliesFile, resultsFile, casesFile :: FilePath
here = "experiments/conjure-2026-11"
repliesFile = here </> "replies.json"
resultsFile = here </> "results.json"
casesFile = here </> "cases.json"

judgeModel, strategistModel :: Text
judgeModel = "claude-opus-5-5"
strategistModel = "claude-haiku-5-5"

data Case = Case {cId :: Text, cKind :: Text, cTree :: FilePath, cFile :: FilePath, cUnit :: Text, cLine :: Int, cSpan :: (Int, Int), cRef :: Text}

instance FromJSON Case where
  parseJSON = withObject "case" $ \o -> do
    [a, z] <- o .: "span"
    Case <$> o .: "id" <*> o .: "kind" <*> o .: "tree" <*> o .: "file" <*> o .: "unit" <*> o .: "line" <*> pure (a, z) <*> o .: "reference"

-- | A writer: its name in the results, its model id, an effort, and whether it is local (Ollama).
data Writer = Writer {wName :: Text, wModel :: Text, wEffort :: Maybe Text, wLocal :: Bool}

writerOf :: Text -> Writer
writerOf n = case T.breakOn "@" n of
  (m, "") -> Writer n m Nothing (not ("claude-" `T.isPrefixOf` m))
  (m, e) -> Writer n m (Just (T.drop 1 e)) False

data Reply = Reply {rText :: Either Text Text, rCost :: Double, rSecs :: Double, rRss :: Maybe Double}

replyJson :: Reply -> Value
replyJson r = object (["cost" .= rCost r, "secs" .= rSecs r] ++ either (\e -> ["error" .= e]) (\t -> ["text" .= t]) (rText r) ++ maybe [] (\m -> ["rss_mb" .= m]) (rRss r))

replyFrom :: Value -> Reply
replyFrom = \case
  Object o ->
    Reply
      (maybe (Left (str "error" o)) Right (KM.lookup "text" o >>= \case String t -> Just t; _ -> Nothing))
      (num "cost" o) (num "secs" o) (KM.lookup "rss_mb" o >>= \case Number n -> Just (toRealFloat n); _ -> Nothing)
  _ -> Reply (Left "bad record") 0 0 Nothing
  where
    num k o = maybe 0 (\case Number n -> toRealFloat n; _ -> 0) (KM.lookup k o)
    str k o = maybe "" (\case String t -> t; _ -> "") (KM.lookup k o)

readObject :: FilePath -> IO (KM.KeyMap Value)
readObject f = doesFileExist f >>= \case
  False -> pure KM.empty
  True -> eitherDecodeFileStrict f >>= \case
    Right (Object o) -> pure o
    _ -> pure KM.empty

writeObject :: FilePath -> KM.KeyMap Value -> IO ()
writeObject f o = BL.writeFile f (encode (Object o) <> "\n")

-- | The recorded reply under KEY, or (live) the action's, recorded at once; stops past the cap.
recorded :: Text -> IO Reply -> IO Reply
recorded key action = do
  rs <- readObject repliesFile
  case KM.lookup (Key.fromText key) rs of
    Just v -> pure (replyFrom v)
    Nothing -> do
      live <- (== Just "1") <$> lookupEnv "CONJURE_LIVE"
      unless live $ hPutStrLn stderr ("unrecorded: " <> T.unpack key) >> exitFailure
      cap <- maybe 8.0 read <$> lookupEnv "CONJURE_CAP"
      let spent = sum [rCost (replyFrom v) | v <- KM.elems rs]
      when (spent > cap) $ hPutStrLn stderr (printf "recorded spend $%.3f past $%.2f: stopping before %s" spent (cap :: Double) (T.unpack key)) >> exitFailure
      r <- action
      rs' <- readObject repliesFile
      writeObject repliesFile (KM.insert (Key.fromText key) (replyJson r) rs')
      pure r

-- * Model calls

scratchCwd :: IO FilePath
scratchCwd = do
  d <- (</> "conjure-claude-cwd") <$> getTemporaryDirectory
  createDirectoryIfMissing True d
  pure d

-- | Claude through the subscription, as Server/Errand.hs runs it (`claude -p --output-format json
-- --model M --permission-prompts none`, under alarm 420), with the system prompt replaced, no
-- tools, no session, and no CLAUDE.md or settings (hooks, plugins) so only the prompt is sent.
askClaude :: Text -> Maybe Text -> Text -> Text -> IO Reply
askClaude model effort system user = do
  cwd <- scratchCwd
  let args =
        ["-e", "alarm 420; exec @ARGV", "env", "-u", "CLAUDECODE", "CLAUDE_CODE_DISABLE_CLAUDE_MDS=1", "claude", "-p", "--output-format", "json", "--model", T.unpack model]
          ++ maybe [] (\e -> ["--effort", T.unpack e]) effort
          ++ ["--permission-prompts", "none", "--system-prompt", T.unpack system, "--tools", "", "--no-session-persistence", "--setting-sources", ""]
  t0 <- getMonotonicTime
  (code, out, err) <- readCreateProcessWithExitCode (proc "perl" args) {cwd = Just cwd} (T.unpack user)
  t1 <- getMonotonicTime
  let secs = t1 - t0
  pure $ case eitherDecodeStrict (TE.encodeUtf8 (T.pack out)) of
    Right (Object o) ->
      let cost = maybe 0 (\case Number n -> toRealFloat n; _ -> 0) (KM.lookup "total_cost_usd" o)
          isErr = KM.lookup "is_error" o == Just (Bool True)
          res = case KM.lookup "result" o of Just (String t) -> t; _ -> ""
       in Reply (if isErr then Left ("is_error: " <> T.take 200 res) else Right res) cost secs Nothing
    _ -> Reply (Left (T.pack (show code) <> ": " <> T.take 300 (T.pack (out <> err)))) 0 secs Nothing

-- | The local model through Ollama's OpenAI-compatible endpoint, the body the engine's chatBody
-- sends (temperature 0), under alarm 90; the peak resident memory of Ollama's processes sampled
-- every half second while it answers.
askLocal :: Text -> Text -> Text -> IO Reply
askLocal model system user = do
  peak <- newIORef (0 :: Double)
  sampler <- forkIO . forever $ do
    (_, out, _) <- readCreateProcessWithExitCode (proc "ps" ["-axo", "rss=,comm="]) ""
    let kb = sum [read (T.unpack n) :: Double | l <- T.lines (T.pack out), let (n, c) = T.breakOn " " (T.strip l), T.all isDigit n, not (T.null n), "ollama" `T.isInfixOf` c]
    modifyIORef' peak (max (kb / 1024))
    threadDelay 500000
  let body = object ["model" .= model, "messages" .= [object ["role" .= ("system" :: Text), "content" .= system], object ["role" .= ("user" :: Text), "content" .= user]], "temperature" .= (0 :: Int)]
  t0 <- getMonotonicTime
  (code, out, err) <- readCreateProcessWithExitCode (proc "perl" ["-e", "alarm 90; exec @ARGV", "curl", "-sS", "http://localhost:11434/v1/chat/completions", "-H", "content-type: application/json", "--data-binary", "@-"]) (T.unpack (TE.decodeUtf8 (BL.toStrict (encode body))))
  t1 <- getMonotonicTime
  killThread sampler
  mb <- readIORef peak
  let text = case eitherDecodeStrict (TE.encodeUtf8 (T.pack out)) of
        Right (Object o)
          | Just (Array cs) <- KM.lookup "choices" o
          , (Object c : _) <- toList cs
          , Just (Object m) <- KM.lookup "message" c
          , Just (String t) <- KM.lookup "content" m ->
              Right (dropThinking t)
        _ -> Left ((if code == ExitSuccess then "" else "killed or failed (" <> T.pack (show code) <> "): ") <> T.take 300 (T.pack (out <> err)))
  pure (Reply text 0 (t1 - t0) (Just mb))
  where
    dropThinking t = case T.breakOn "</think>" t of
      (_, "") -> t
      (_, rest) -> T.drop (T.length "</think>") rest

askWriter :: Writer -> Text -> Text -> IO Reply
askWriter w system user
  | wLocal w = askLocal (wModel w) system user
  | otherwise = askClaude (wModel w) (wEffort w) system user

-- * The engine's conjure request (variant A)

-- | 'Olifant.Server.Conjure.functionConjure' less the template: the instruction and the context, copied
-- (the module does not export it); 'firstLine', 'oneLine', 'dedent' and 'rejectedBody' as there.
engineConjure :: Language -> Text -> (Int, Int) -> Text -> FilePath -> Text -> TestRun -> (Text, [(FilePath, Text)])
engineConjure lang text (l1, l2) unit testRel testText before = (instruction, context)
  where
    ls = T.splitOn "\n" text
    body = take (l2 - l1 + 1) (drop (l1 - 1) ls)
    short = snd . T.breakOnEnd "::"
    failed = trFailed before
    instruction = oneLine $ case (trBroken before, failed) of
      (Just why, _) -> "the tests could not run (" <> firstLine why <> "); write the body of " <> unit <> " its tests describe"
      (_, []) -> "the tests pass; write the body of " <> unit <> " they describe, keeping them passing"
      _ -> "make the failing tests pass (" <> T.intercalate ", " [short t <> ": " <> failureLineIn lang f | (t, f) <- failed] <> ") and keep the file's other tests passing; the tests and their messages are in the context; write the least code that does it"
    failing = T.intercalate "\n\n" [t <> "\n" <> fMessage f <> "\n" <> T.unlines (sourceOf (short t)) | (t, f) <- failed]
    sourceOf n = maybe [] T.lines (testSourceIn lang (Just testRel) (snd (T.breakOnEnd "::" (fst (T.breakOn "[" n)))) testText)
    context =
      [(testRel, testText)]
        ++ [("failing now (make these pass)", failing) | not (null failed)]
        ++ [("passing now (keep these passing)", T.intercalate "\n" (trPassed before)) | not (null (trPassed before))]
        ++ [("the body on screen, rejected by the person (write another)", T.intercalate "\n" (dedent body)) | not (isStubIn lang body)]
    firstLine = T.strip . T.takeWhile (/= '\n')
    oneLine = T.replace "=}" "= }" . T.intercalate "; " . filter (not . T.null) . map T.strip . T.lines

dedent :: [Text] -> [Text]
dedent ls = map (T.drop n) ls
  where
    n = minimum (maxBound : [T.length (T.takeWhile (== ' ') l) | l <- ls, not (T.null (T.strip l))])

-- | The fill request the fill phase sends for the conjure's one `block` hole `body` (FillPhase's
-- fillHole: the marker-only instruction first, the file around the hole with it marked).
engineRequest :: Language -> Case -> Text -> Text -> [(FilePath, Text)] -> FillRequest
engineRequest lang c text instruction context =
  let (l1, l2) = cSpan c
      ls = T.splitOn "\n" text
      indent = T.takeWhile (`elem` [' ', '\t']) (fromMaybe "" (listToMaybe (drop (l1 - 1) ls)))
      marked = T.intercalate "\n" (take (l1 - 1) ls ++ [indent <> "\10214body\10215"] ++ drop l2 ls)
      hole = FillHole "body" "block" (markerOnly <> "; " <> instruction) (cFile c) marked [] (isJust (langImports lang)) (langImportsDef lang >>= imExample)
   in FillRequest [hole] [] context ConjureStance

-- * B and C prompts

stepSystem :: Bool -> Text
stepSystem ownGoal =
  T.unlines $
    [ "You take one small step on a programmer's code where their cursor is. Read what is there and do the next small thing"
    , "that leaves the code more right than you found it: fix a syntax error, fill or continue a body, make a failing test pass,"
    , "write a missing test; or nothing, when the code is complete and its tests pass."
    , "The code on screen is the person's work in progress, not a rejected attempt: continue their intent, keep what already"
    , "works, change only what the step needs. A red test is the clearest goal, but a step need not turn every test green,"
    , "and a step needs no test to be worth taking."
    , ""
    , "Reply in this format and nothing else (no prose, no markdown fences):"
    ]
      ++ ( if ownGoal
             then ["<goal>the step you take, one sentence</goal>"]
             else ["(the step to take is given at the end of the message)"]
         )
      ++ [ "then one block per file you change or create, with the file's complete new text:"
         , "<file path=\"path/relative/to/the/project\">"
         , "the complete file"
         , "</file>"
         , if ownGoal then "When nothing needs doing: <goal>nothing</goal> and no file blocks." else "When the step is nothing: <nothing/> and no file blocks."
         ]

strategistSystem :: Text
strategistSystem =
  T.unlines
    [ "You read a programmer's code where their cursor is and name the next small goal: one step that leaves the code more"
    , "right than it is (fix a syntax error, fill or continue a body, make a failing test pass, write a missing test), or"
    , "nothing when the code is complete and its tests pass. The code on screen is the person's work in progress: the goal"
    , "continues their intent. Reply with two lines and nothing else:"
    , "GOAL: <one sentence>"
    , "UNITS: <the units or files the step touches, comma-separated, or none>"
    ]

-- | The situation at the cursor, as B, C, the strategist and the judge read it.
situation :: Case -> Site -> Text -> [(FilePath, Text)] -> Text
situation c site testsBefore imported =
  T.unlines $
    [ "Project files are Python (a bistro table-booking app; tests are pytest files named <module>__test.py beside their module)."
    , ""
    , "The cursor is in `" <> cUnit c <> "` in " <> T.pack (cFile c) <> ", line " <> tshow (cLine c) <> " (the unit's body is lines " <> tshow (fst (cSpan c)) <> "-" <> tshow (snd (cSpan c)) <> ")."
    , ""
    , "File " <> T.pack (cFile c) <> ":"
    , "```"
    , siteModuleText site
    , "```"
    , ""
    ]
      ++ maybe
        ["There is no test file for this module yet (its tests would go in " <> T.pack (siteTestRel site) <> ")."]
        (\t -> ["Its test file " <> T.pack (siteTestRel site) <> ":", "```", t, "```"])
        (siteTestText site)
      ++ ["", "Tests before the step: " <> testsBefore]
      ++ concat [["", "Related file " <> T.pack p <> ":", "```", t, "```"] | (p, t) <- imported]

testsLine :: Language -> Maybe TestRun -> Text
testsLine lang = \case
  Nothing -> "none (no test file)."
  Just r -> case trBroken r of
    Just why -> "they could not run: " <> T.intercalate " / " (take 6 (filter (not . T.null) (map T.strip (T.lines why))))
    Nothing ->
      tshow (length (trPassed r)) <> " passing (" <> T.intercalate ", " (map short (trPassed r)) <> ")"
        <> (if null (trFailed r) then "; none failing." else "; failing: " <> T.intercalate "; " [short t <> ": " <> failureLineIn lang f | (t, f) <- trFailed r] <> ".")
  where
    short = snd . T.breakOnEnd "::"

-- * Applying a step

-- | A's fill as the engine reads it ('decodeFills'); it is placed with the engine's 'fillLines'.
fillOf :: Text -> Either Text Text
fillOf reply =
  case decodeFills (OpenAICompatible "" "") (BL.toStrict (encode (object ["choices" .= [object ["message" .= object ["content" .= reply]]]]))) of
    Left why -> Left why
    Right fs -> maybe (Left "the model returned no fill for `body`") Right (M.lookup "body" fs)

-- | B's and C's file blocks (path, text), and C's goal.
fileBlocks :: Text -> [(FilePath, Text)]
fileBlocks t =
  [ (T.unpack (T.strip path), ensureNl (unfence (trimNl body)))
  | chunk <- drop 1 (T.splitOn "<file path=\"" t)
  , let (path, rest) = T.breakOn "\">" chunk
  , not (T.null rest)
  , let (body, close) = T.breakOn "</file>" (T.drop 2 rest)
  , not (T.null close)
  ]
  where
    trimNl b = let b' = fromMaybe b (T.stripPrefix "\n" b) in fromMaybe b' (T.stripSuffix "\n" b')
    unfence b = case T.lines b of
      (l : rest) | "```" `T.isPrefixOf` T.strip l, not (null rest), "```" `T.isPrefixOf` T.strip (last rest) -> T.intercalate "\n" (init rest)
      _ -> b
    ensureNl b = if "\n" `T.isSuffixOf` b then b else b <> "\n"

goalOf :: Text -> Maybe Text
goalOf t = case T.breakOn "<goal>" t of
  (_, "") -> Nothing
  (_, rest) -> Just (T.strip (fst (T.breakOn "</goal>" (T.drop 6 rest))))

safePath :: FilePath -> Bool
safePath p = not (null p) && take 1 p /= "/" && ".." `notElem` T.splitOn "/" (T.pack p)

-- * Measuring

run :: FilePath -> [String] -> IO (ExitCode, Text)
run dir (cmd : args) = do
  (code, out, err) <- readCreateProcessWithExitCode (proc cmd args) {cwd = Just dir} ""
  pure (code, T.pack (out <> err))
run _ [] = pure (ExitSuccess, "")

testFilesIn :: FilePath -> IO [FilePath]
testFilesIn dir = do
  (_, out) <- run dir ["find", "src", "-name", "*__test.py"]
  pure (sortOn id [T.unpack l | l <- T.lines out, not (T.null l)])

-- | Every test file run apart (a collection error in one does not hide the others).
runAll :: ServerState -> Site -> FilePath -> IO (M.Map FilePath TestRun)
runAll s site dir = do
  tfs <- testFilesIn dir
  rs <- forM tfs $ \tf -> do
    r <- runOver s (prWorkspace (siteProject site)) (siteLang site) [T.pack tf]
    pure (tf, either (TestRun [] [] . Just) id r)
  pure (M.fromList rs)

passedIn :: M.Map FilePath TestRun -> [Text]
passedIn = concatMap trPassed . M.elems

parsesAll :: FilePath -> [FilePath] -> IO (Bool, Text)
parsesAll dir fs = do
  (code, out) <- run dir (["python3", "-c", "import ast,sys\nbad=[]\nfor p in sys.argv[1:]:\n  try: ast.parse(open(p).read(), p)\n  except Exception as e: bad.append(p+': '+str(e))\nprint('\\n'.join(bad)); sys.exit(1 if bad else 0)"] ++ fs)
  pure (code == ExitSuccess, T.strip out)

-- | Lines changed, files touched, whether any change falls outside the unit (its def line to its
-- body's end), and the diff.
scopeOf :: FilePath -> Case -> IO (Int, [Text], Bool, Text)
scopeOf dir c = do
  _ <- run dir ["git", "add", "-A"]
  (_, numstat) <- run dir ["git", "diff", "--cached", "--numstat"]
  (_, hunks) <- run dir ["git", "diff", "--cached", "-U0", "--", cFile c]
  (_, diff) <- run dir ["git", "diff", "--cached"]
  let rows = [(T.words l) | l <- T.lines numstat, not (T.null l)]
      files = [f | (_ : _ : f : _) <- rows]
      lines' = sum [readInt a + readInt d | (a : d : _) <- rows]
      (l1, l2) = cSpan c
      inside (a, n) = if n == 0 then l1 - 1 <= a && a <= l2 else l1 - 1 <= a && a + n - 1 <= l2
      olds = [oldRange h | h <- T.lines hunks, "@@ " `T.isPrefixOf` h]
      outside = any (/= T.pack (cFile c)) files || not (all inside olds)
  pure (lines', files, outside, diff)
  where
    readInt t = if T.all isDigit t && not (T.null t) then read (T.unpack t) else 0 :: Int
    oldRange h =
      let w = T.drop 1 (T.words h !! 1)
          (a, n) = T.breakOn "," w
       in (readInt a, if T.null n then 1 else readInt (T.drop 1 n))

tshow :: Show a => a -> Text
tshow = T.pack . show

-- * Modes

plainPorts :: Ports
plainPorts = Ports (diskWorkspace "/") (error "syntax set below") (const noChecker) (const noVerifier) (\cancel -> processRunner cancel 120) (\n cancel -> processRunner cancel n) getCurrentTime Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False) (\_ -> pure Nothing) (\_ -> pure []) (\_ -> pure (Left NoSource))) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base")) (\_ _ -> pure (Left "no base")) gitStateStamp

main :: IO ()
main = do
  args <- getArgs
  allCases <- either fail pure =<< eitherDecodeFileStrict casesFile
  only <- lookupEnv "CONJURE_CASES"
  let cases = maybe allCases (\o -> [c | c <- allCases, cId c `elem` T.splitOn "," (T.pack o)]) only
  trees <- fromMaybe (error "CONJURE_TREES unset") <$> lookupEnv "CONJURE_TREES"
  tmp <- getTemporaryDirectory
  s0 <- newServerState plainPorts {pSyntax = astGrepSyntax (tmp </> "olifant-test-cache")}
  res0 <- readObject resultsFile
  writeObject resultsFile (KM.insert "cases" (toJSONCases allCases) res0)
  case args of
    "write" : ws -> do
      work <- fromMaybe (error "CONJURE_WORK unset") <$> lookupEnv "CONJURE_WORK"
      forM_ cases $ \c -> writeCase s0 trees work (map (writerOf . T.pack) ws) c
    ["judge"] -> forM_ cases (judgeCase s0 trees)
    _ -> hPutStrLn stderr "usage: conjure write WRITER... | conjure judge" >> exitFailure
  where
    toJSONCases cs = toJSONList [object ["id" .= cId c, "kind" .= cKind c, "tree" .= cTree c, "file" .= cFile c, "unit" .= cUnit c, "line" .= cLine c, "span" .= [fst (cSpan c), snd (cSpan c)], "reference" .= cRef c] | c <- cs]
    toJSONList = Array . foldr (\x acc -> pure x <> acc) mempty

siteIn :: ServerState -> FilePath -> Case -> IO (ServerState, Site)
siteIn s dir c =
  siteAt (dir </> cFile c) (cLine c) s >>= \case
    (s', Right site) -> pure (s', site)
    (_, Left why) -> fail (T.unpack why)

writeCase :: ServerState -> FilePath -> FilePath -> [Writer] -> Case -> IO ()
writeCase s0 trees work writers c = do
  let tree = trees </> cTree c
  (s, site) <- siteIn s0 tree c
  let lang = siteLang site
      text = siteModuleText site
      testRel = siteTestRel site
  before <- runAll s site tree
  let unitBefore = M.lookup testRel before
  imported <- importedContext (prWorkspace (siteProject site)) lang ((cFile c, text) : [(testRel, t) | Just t <- [siteTestText site]])
  let sit = situation c site (testsLine lang unitBefore) imported
  forM_ writers $ \w -> forM_ ["A", "B", "C"] $ \shape -> do
    let key = shape <> "|" <> wName w <> "|" <> cId c
    done <- readObject resultsFile
    let runs = case KM.lookup "runs" done of Just (Object o) -> o; _ -> KM.empty
    unless (KM.member (Key.fromText key) runs) $ do
      -- the step's reply (and B's goal)
      (reply, goal, extraCost, extraSecs) <- case shape of
        "A" -> case siteTestText site of
          -- today's conjure refuses a module with no test file (Server/Conjure.hs, "no test file for this module yet")
          Nothing -> pure (Reply (Left "refused: no test file for this module yet (today's conjure stops here)") 0 0 Nothing, Nothing, 0, 0)
          Just testText -> do
            let (instruction, context) = engineConjure lang text (cSpan c) (cUnit c) testRel testText (fromMaybe (TestRun [] [] (Just "no run")) unitBefore)
            ctx <- importedContext (prWorkspace (siteProject site)) lang ((cFile c, text) : context)
            let req = engineRequest lang c text instruction (context ++ ctx)
            r <- recorded key (askWriter w conjureSystemPrompt (userPrompt req))
            pure (r, Nothing, 0, 0)
        "B" -> do
          g <- recorded ("STRATEGIST|" <> strategistModel <> "|" <> cId c) (askClaude strategistModel Nothing strategistSystem sit)
          case rText g of
            Left why -> pure (Reply (Left ("strategist: " <> why)) 0 0 Nothing, Nothing, rCost g, rSecs g)
            Right gt -> do
              let goal = T.strip (T.intercalate " " [T.strip (T.drop 5 (T.strip l)) | l <- T.lines gt, "GOAL:" `T.isPrefixOf` T.strip l])
                  units = T.strip (T.intercalate " " [T.strip (T.drop 6 (T.strip l)) | l <- T.lines gt, "UNITS:" `T.isPrefixOf` T.strip l])
              r <- recorded key (askWriter w (stepSystem False) (sit <> "\nThe step to take: " <> goal <> " (touches: " <> units <> ")\n"))
              pure (r, Just (gt), rCost g, rSecs g)
        _ -> do
          r <- recorded key (askWriter w (stepSystem True) sit)
          pure (r, either (const Nothing) goalOf (rText r), 0, 0)
      -- apply it in a fresh copy of the tree
      let dir = work </> T.unpack (cId c) </> T.unpack (T.replace ":" "_" (T.replace "@" "_" (shape <> "-" <> wName w)))
      removePathForcibly dir
      createDirectoryIfMissing True (takeDirectory dir)
      _ <- run "." ["cp", "-R", tree, dir]
      applied <- case rText reply of
        Left why -> pure (Left why)
        Right t
          | shape == "A" -> case fillOf t of
              Left why -> pure (Left ("unreadable fill: " <> why))
              Right f -> do
                -- placed as a conjure round lands ('fillLines'); a block written as if its first
                -- line sat at the marker's column is read brought up to it when that parses
                -- (the fill phase's 'reindented' rescue, judged there by the checker)
                let place g = TIO.writeFile (dir </> cFile c) (fillLines (Just lang) text (cSpan c) g)
                place f
                (ok, _) <- parsesAll dir [cFile c]
                case reindented (Just (langComment lang)) f of
                  Just f' | not ok -> do
                    place f'
                    (ok', _) <- parsesAll dir [cFile c]
                    unless ok' (place f)
                  _ -> pure ()
                pure (Right ())
          | otherwise -> do
              let blocks = fileBlocks t
              if any (not . safePath . fst) blocks
                then pure (Left "a file block outside the project")
                else do
                  forM_ blocks $ \(p, body) -> createDirectoryIfMissing True (takeDirectory (dir </> p)) >> TIO.writeFile (dir </> p) body
                  pure (Right ())
      (lines', files, outside, diff) <- scopeOf dir c
      (parses, parseWhy) <- parsesAll dir (nub (cFile c : [T.unpack f | f <- files, ".py" `T.isSuffixOf` f]))
      (s', site') <- siteIn s dir c
      after <- runAll s' site' dir
      let newlyPassing = passedIn after \\ passedIn before
          newlyFailing = passedIn before \\ passedIn after
          unitAfter = M.lookup testRel after
          cost = rCost reply + extraCost
          secs = rSecs reply + extraSecs
          v =
            object $
              [ "variant" .= shape, "writer" .= wName w, "case" .= cId c, "kind" .= cKind c
              , "usable" .= either (const False) (const True) applied
              , "error" .= either id (const "") applied
              , "goal" .= goal
              , "parses" .= parses, "parse_errors" .= parseWhy
              , "newly_passing" .= newlyPassing, "newly_failing" .= newlyFailing
              , "unit_tests_before" .= testsLine lang unitBefore, "unit_tests_after" .= testsLine lang unitAfter
              , "lines_changed" .= lines', "files_touched" .= files, "outside_unit" .= outside
              , "secs" .= secs, "cost" .= cost
              , "reply" .= either id id (rText reply)
              , "diff" .= diff
              ]
                ++ maybe [] (\m -> ["rss_mb" .= m, "within_60s" .= (secs <= 60)]) (rRss reply)
      cur <- readObject resultsFile
      let runs' = case KM.lookup "runs" cur of Just (Object o) -> o; _ -> KM.empty
      writeObject resultsFile (KM.insert "runs" (Object (KM.insert (Key.fromText key) v runs')) cur)
      printf "%-40s %s parses=%s +%d -%d lines=%d $%.4f %.1fs\n" (T.unpack key) (either (const "unusable" :: Text -> String) (const "ok") applied) (show parses) (length newlyPassing) (length newlyFailing) lines' cost secs
      hFlush stdout

-- | Opus judges every run of a case at once, blind (labels shuffled by a fixed hash, no model or
-- shape named); a run with no usable reply is "no" without asking.
judgeCase :: ServerState -> FilePath -> Case -> IO ()
judgeCase s0 trees c = do
  res <- readObject resultsFile
  let runs = case KM.lookup "runs" res of Just (Object o) -> [(Key.toText k, o') | (k, Object o') <- KM.toList o, KM.lookup "case" o' == Just (String (cId c))]; _ -> []
      usable o = KM.lookup "usable" o == Just (Bool True)
      ordered = sortOn (hashText . fst) [r | r@(_, o) <- runs, usable o]
      labelled = zip ["S" <> T.justifyRight 2 '0' (tshow i) | i <- [1 :: Int ..]] ordered
  unless (null runs) $ do
    (_, site) <- siteIn s0 (trees </> cTree c) c
    let lang = siteLang site
    before <- runAll s0 site (trees </> cTree c)
    let sit = situation c site (testsLine lang (M.lookup (siteTestRel site) before)) []
        candidate (l, (_, o)) =
          T.unlines
            [ "=== " <> l <> " ==="
            , "Goal it stated: " <> fromMaybe "(none stated)" (str "goal" o)
            , "After the step: " <> (if KM.lookup "parses" o == Just (Bool True) then "every changed Python file parses" else "a changed file does not parse: " <> fromMaybe "" (str "parse_errors" o))
                <> "; tests newly passing: " <> listOf "newly_passing" o <> "; newly failing (regressions): " <> listOf "newly_failing" o
            , "The unit's tests after: " <> fromMaybe "" (str "unit_tests_after" o)
            , "Diff (empty: nothing changed):"
            , "```diff"
            , T.unlines (take 220 (T.lines (fromMaybe "" (str "diff" o))))
            , "```"
            ]
        user =
          T.unlines
            [ "The situation before the step:"
            , ""
            , sit
            , ""
            , "Reference next step (written by the harness author; what forward looks like here, a guide rather than the only right answer):"
            , cRef c
            , ""
            , "The candidate steps, each taken alone on the situation above:"
            , ""
            , T.unlines (map candidate labelled)
            , "Answer with one JSON object and nothing else: {\"S01\": {\"forward\": \"yes\" or \"no\", \"why\": \"one line\"}, ...} with every candidate."
            ]
    verdicts <-
      if null labelled
        then pure KM.empty
        else do
          r <- recorded ("JUDGE|" <> judgeModel <> "|" <> cId c) (askClaude judgeModel Nothing judgeSystem user)
          pure $ case rText r of
            Left _ -> KM.empty
            Right t -> case eitherDecodeStrict (TE.encodeUtf8 (outerBraces t)) of
              Right (Object o) -> o
              _ -> KM.empty
    let verdictOf (l, (k, _)) = case KM.lookup (Key.fromText l) verdicts of
          Just (Object v) -> (k, object ["forward" .= fromMaybe "?" (strV "forward" v), "why" .= fromMaybe "" (strV "why" v), "label" .= l])
          _ -> (k, object ["forward" .= ("?" :: Text), "why" .= ("the judge gave no verdict" :: Text), "label" .= l])
        refused = [(k, object ["forward" .= ("no" :: Text), "why" .= ("no usable step: " <> fromMaybe "" (str "error" o)), "label" .= ("-" :: Text)]) | (k, o) <- runs, not (usable o)]
    cur <- readObject resultsFile
    let judged = case KM.lookup "judged" cur of Just (Object o) -> o; _ -> KM.empty
        judged' = foldr (\(k, v) acc -> KM.insert (Key.fromText k) v acc) judged (map verdictOf labelled ++ refused)
    writeObject resultsFile (KM.insert "judged" (Object judged') cur)
    printf "%-18s judged %d (%d unusable)\n" (T.unpack (cId c)) (length labelled) (length refused)
    hFlush stdout
  where
    str k o = case KM.lookup k o of Just (String t) -> Just t; _ -> Nothing
    strV = str
    listOf k o = case KM.lookup k o of
      Just (Array xs) | not (null xs) -> T.intercalate ", " [snd (T.breakOnEnd "::" t) | String t <- toList xs]
      _ -> "none"
    outerBraces t = case (T.breakOn "{" t, T.breakOnEnd "}" t) of
      ((_, fromOpen), (toClose, _)) | not (T.null fromOpen), not (T.null toClose) -> T.take (T.length fromOpen - (T.length t - T.length toClose)) fromOpen
      _ -> t
    hashText = T.foldl' (\h ch -> (h `xor` ord ch) * 16777619 `mod` 4294967296) (2166136261 :: Int)

judgeSystem :: Text
judgeSystem =
  T.unlines
    [ "You judge automated coding steps on a programmer's code: did each step move the code forward?"
    , "Forward means all of these:"
    , "- it kept what already worked (no regression: no test that passed now fails, nothing working was deleted);"
    , "- it continued the person's intent rather than discarding it (the code on screen is their work in progress);"
    , "- it left the code closer to the reference next step (other sensible next steps count too);"
    , "- it left no nonsense behind (no stray lines, no invented names, it parses);"
    , "- its scope is reasonable for one step (not a rewrite of things the step did not need)."
    , "When the code was already complete and green, changing nothing (or a tiny tidy) is forward; churn is not."
    , "A step need not turn every test green to be forward. Judge each candidate on its own against the situation;"
    , "you are not told which model or prompt produced it. Be strict and brief."
    ]
