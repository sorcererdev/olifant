{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | `olifant/conjure` as one step (design 2026-11-16): the facts at the
-- cursor, one writer call ('stepSystem' over 'situationText'), the check
-- (each touched file parsed, the unit's tests run over the step) and the
-- step shown under a token. The person accepts it (it lands), rejects it, or
-- asks the harder writer to take it again over the same situation. Nothing is
-- refused for red tests, a missing test file or a body that is not a stub.
module Olifant.Server.ConjureStep
  ( ShownStep (..)
  , LandedStep (..)
  , StepOutcome (..)
  , StepChoice (..)
  , conjureAt
  , answerStep
  , sessionDiff
  , stepGone
  , stepJson
  , stepResumeOf
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.Functor ((<&>))
import Data.IORef (atomicModifyIORef')
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import GHC.Clock (getMonotonicTime)
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (catalogConjureHarder, catalogConjureWriter, catalogLanguages, catalogModel)
import Olifant.Generation.Tools (ProgressEvent (..))
import Olifant.Language (Language (..), TestRun (..), failureLineIn, failureOf, languageFor)
import Olifant.Language.TreeSitter (errorLines)
import Olifant.Model (ChatReply (..), ModelConfig (..), askBackendReply, backendModel)
import Olifant.Process (runBytes)
import Olifant.Server.Edits (FileEdit (..), smallestChange, unifiedDiff)
import Olifant.Server.Site (Site (..), appendStepLine, importedContext, runOver, siteAt, siteTestFile)
import Olifant.Server.State (ServerConfig (..), ServerState (..))
import Olifant.Server.Step
import Olifant.Server.Tests (TestsAnswer (..), TestsScope (..), testsJson)
import Olifant.Server.Unit (Unit (..), testOrUnitAt)
import Olifant.Workspace (Workspace (..), normalisePath, overlayMany)
import System.Directory (getModificationTime)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, (</>))

-- | A step as the review shows it.
data ShownStep = ShownStep
  { shToken :: Text
  , shFile :: FilePath
  , shUnit :: Text
  , shWriter :: Text
  , shGoal :: Maybe Text
  , shEdits :: [FileEdit]
  , shDropped :: [FilePath]
  , shParses :: [(FilePath, Bool)]
  , shDelta :: TestDelta
  , shTests :: Maybe TestsAnswer
  , shCost :: Maybe Double
  , shSeconds :: Double
  , shHarder :: Bool
  }
  deriving stock (Eq, Show)

-- | An accepted step: its edits, to land as they are.
data LandedStep = LandedStep {lsFile :: FilePath, lsEdits :: [FileEdit]}
  deriving stock (Eq, Show)

data StepOutcome = StepShown ShownStep | StepNothing Text | StepLanded LandedStep | StepRejected
  deriving stock (Eq, Show)

data StepChoice = StepAccept | StepReject | StepHarder
  deriving stock (Eq, Show)

stepGone :: Text
stepGone = "this step is gone (it was answered, or a new conjure replaced it): conjure again"

noModel :: Text
noModel = "no model is configured: set [model] in .olifant/config.toml to conjure"

-- | One step at LINE (1-based) of PATH, or over the SELECTED lines. Left is
-- one sentence on why there is none.
conjureAt :: (ProgressEvent -> IO ()) -> FilePath -> Int -> Maybe (Int, Int) -> ServerState -> IO (ServerState, Either Text StepOutcome)
conjureAt progress path line selected s0 =
  siteAt path line s0 >>= \case
    (s, Left why) -> pure (s, Left why)
    (s, Right site) -> fmap (s,) . runExceptT $ do
      let lang = siteLang site
          project = siteProject site
          ws = prWorkspace project
          root = siteRoot site
          onTest = siteOnTest site
          rel = if onTest then siteTestRel site else siteModuleRel site
          file = normalisePath (root </> rel)
      text <- if onTest then maybe (throwE ("cannot read " <> T.pack rel)) pure (siteTestText site) else pure (siteModuleText site)
      found <- lift (testOrUnitAt (pSyntax (ssPorts s)) lang (Just rel) text line)
      (unit, span') <- case (selected, found) of
        (Just sp, _) -> pure (either (const "the selection") unitName found, sp)
        (Nothing, Left why) -> throwE (why <> ": put the cursor in one to conjure it")
        (Nothing, Right u) -> pure (unitName u, unitLines u)
      (writer, ask) <- ExceptT (writerFor s project False)
      -- a new conjure in this file replaces the step shown there
      lift (dropShown s file)
      before <- lift (if isJust (siteTestText site) then either (const Nothing) Just <$> runOver s ws lang [T.pack (siteTestRel site)] else pure Nothing)
      imported <- lift (importedContext ws lang [(rel, text)])
      session <- lift (sessionDiff root [T.dropWhile (/= '.') g | g <- langFiles lang])
      let situation =
            situationText
              Situation
                { siLanguage = langName lang
                , siTestNaming = "the tests for " <> T.pack (siteModuleRel site) <> " go in " <> T.pack (siteTestRel site)
                , siFile = rel
                , siUnit = unit
                , siLine = line
                , siSpan = span'
                , siModule = text
                , siTestFile = siteTestRel site
                , siTestText = siteTestText site
                , siTestsBefore = testsWords lang before
                , siRelated = imported
                , siSession = session
                }
      lift (progress (Said ("conjuring with " <> writer)))
      ExceptT (takeStep s site file unit writer ask situation before)

-- | The writer asked over SITUATION; its answer checked and shown (kept under
-- a new token), or nothing to do.
takeStep :: ServerState -> Site -> FilePath -> Text -> Text -> (Text -> Text -> IO (Either Text ChatReply)) -> Text -> Maybe TestRun -> IO (Either Text StepOutcome)
takeStep s site file unit writer ask situation before =
  ask stepSystem situation >>= \case
    Left why -> pure (Left why)
    Right reply -> do
      let r = readStep (crText reply)
      if srNothing r
        then pure (Right (StepNothing (fromMaybe "nothing" (srGoal r))))
        else if null (srFiles r) then pure (Left "the writer took no step: conjure again, or ask the harder writer") else Right . StepShown <$> checked reply r
  where
    root = siteRoot site
    lang = siteLang site
    ws = prWorkspace (siteProject site)
    cat = prCatalog (siteProject site)
    testRel = siteTestRel site
    checked reply r = do
      let kept = [(k, t) | (p, t) <- srFiles r, Just k <- [keptPath root p]]
          dropped = [p | (p, _) <- srFiles r, keptPath root p == Nothing]
      read' <- forM kept $ \(k, t) -> do
        let a = normalisePath (root </> k)
        (v, base) <- case M.lookup a (ssDocs s) of
          Just (v, d) -> pure (Just v, Just d)
          Nothing -> (Nothing,) <$> readText ws k
        pure (k, a, t, v, base)
      let relWrites = M.fromList [(k, t) | (k, _, t, _, _) <- read']
          parses = [(k, null (errorLines (langGrammar l) t)) | (k, _, t, _, _) <- read', Just l <- [languageFor (catalogLanguages cat) k]]
          hasTests = isJust (siteTestText site) || M.member testRel relWrites
      started <- getMonotonicTime
      after <- if hasTests then either (const Nothing) Just <$> runOver s (overlayMany relWrites ws) lang [T.pack testRel] else pure Nothing
      ended <- getMonotonicTime
      let heldStep =
            StepHeld
              { hdRoot = root
              , hdFile = file
              , hdUnit = unit
              , hdWriter = writer
              , hdGoal = fromMaybe "(no goal stated)" (srGoal r)
              , hdSituation = situation
              , hdWrites = M.fromList [(a, t) | (_, a, t, _, _) <- read']
              , hdVersions = M.fromList [(a, v) | (_, a, _, v, _) <- read']
              , hdBases = M.fromList [(a, b) | (_, a, _, _, b) <- read']
              , hdDelta = testDelta before after
              , hdParses = parses
              , hdLines = sum [diffLines (unifiedDiff k (fromMaybe "" b) t) | (k, _, t, _, b) <- read']
              , hdCost = crCost reply
              , hdSeconds = crSeconds reply
              }
      token <- atomicModifyIORef' (ssShown s) (\(n, m) -> let t = "step-" <> T.pack (show (n + 1)) in ((n + 1, M.insert t heldStep m), t))
      pure
        ShownStep
          { shToken = token
          , shFile = file
          , shUnit = unit
          , shWriter = writer
          , shGoal = srGoal r
          , shEdits = editsOf heldStep
          , shDropped = dropped
          , shParses = parses
          , shDelta = hdDelta heldStep
          , shTests = (\run -> TestsAnswer (siteTestFile site) ScopeFile Nothing root run (ended - started) (failureOf lang)) <$> after
          , shCost = crCost reply
          , shSeconds = crSeconds reply
          , shHarder = isJust (catalogConjureHarder cat)
          }

-- | The person's answer to the step under TOKEN: accept lands it (unless a
-- file it writes moved since), reject drops it, harder asks the harder
-- writer over the same situation and shows that step instead. A token
-- answers once.
answerStep :: (ProgressEvent -> IO ()) -> FilePath -> Text -> StepChoice -> ServerState -> IO (ServerState, Either Text StepOutcome)
answerStep progress path token choice s0 =
  siteAt path 1 s0 >>= \case
    (s, Left why) -> pure (s, Left why)
    (s, Right site) -> fmap (s,) $ do
      held <- M.lookup token . snd <$> readShown s
      case held of
        Nothing -> pure (Left stepGone)
        Just hd -> case choice of
          StepHarder
            | Nothing <- catalogConjureHarder (prCatalog (siteProject site)) -> pure (Left "no harder writer: name one with [conjure] harder")
            | otherwise ->
                writerFor s (siteProject site) True >>= \case
                  Left why -> pure (Left why)
                  Right (writer, ask) -> do
                    progress (Said ("conjuring again with " <> writer))
                    before <- if isJust (siteTestText site) then either (const Nothing) Just <$> runOver s (prWorkspace (siteProject site)) (siteLang site) [T.pack (siteTestRel site)] else pure Nothing
                    taken <- takeShown s token
                    if not taken
                      then pure (Left stepGone)
                      else do
                        record s hd "replaced"
                        takeStep s site (hdFile hd) (hdUnit hd) writer ask (hdSituation hd) before
          StepReject -> do
            taken <- takeShown s token
            if taken then record s hd "rejected" >> pure (Right StepRejected) else pure (Left stepGone)
          StepAccept -> do
            taken <- takeShown s token
            if not taken
              then pure (Left stepGone)
              else
                movedSince s hd >>= \case
                  Just moved -> do
                    record s hd "dropped"
                    pure (Left ("the file changed since the step was shown; your text stands (" <> T.pack (makeRelative (hdRoot hd) moved) <> "): conjure again"))
                  Nothing -> do
                    record s hd "accepted"
                    pure (Right (StepLanded (LandedStep (hdFile hd) (editsOf hd))))
  where
    readShown s = atomicModifyIORef' (ssShown s) (\x -> (x, x))

-- | The writer for a step (the harder one when HARDER): its name as the
-- review says it, and how to ask it. The test writer, when set, answers for
-- any name.
writerFor :: ServerState -> Project -> Bool -> IO (Either Text (Text, Text -> Text -> IO (Either Text ChatReply)))
writerFor s project harder = pure $ case (named, ssTestWriter s, scStepReplies (ssConfig s)) of
  (n, Just w, _) -> let n' = fromMaybe "test-writer" n in Right (n', w n')
  -- recorded: the file's text is every writer's reply (the editor's specs)
  (n, Nothing, Just file) -> Right (fromMaybe "recorded" n, \_ _ -> readText (pFiles (ssPorts s)) file <&> maybe (Left ("recorded replies: cannot read " <> T.pack file)) (\t -> Right (ChatReply t Nothing 0)))
  (Nothing, Nothing, Nothing) -> Left noModel
  (Just n, Nothing, Nothing) -> case lookup n (mcBackends config) of
    Nothing -> Left ("no backend " <> n <> " under [model.backends]")
    Just b -> Right (backendModel b, askBackendReply (ssCancel s) (mcTimeout config) b)
  where
    cat = prCatalog project
    config = catalogModel cat
    named = if harder then catalogConjureHarder cat else maybe (mcDefault config) Just (catalogConjureWriter cat)

-- | The step's writes as edits on what was read: a new file created, each
-- other changed at the editor's version it was read at.
editsOf :: StepHeld -> [FileEdit]
editsOf hd =
  [ case M.findWithDefault Nothing a (hdBases hd) of
      Nothing -> FileEdit a v True ((0, 0), (0, 0)) t
      Just old -> let (range, new) = smallestChange old t in FileEdit a v False range new
  | (a, t) <- M.toList (hdWrites hd)
  , M.lookup a (hdBases hd) /= Just (Just t)
  , let v = M.findWithDefault Nothing a (hdVersions hd)
  ]

-- | The first file the step writes whose text moved since it was read: an
-- open document at another version (the editor's live one when it can
-- say), or one read when not open whose text is not what it was.
movedSince :: ServerState -> StepHeld -> IO (Maybe FilePath)
movedSince s hd = firstJust <$> mapM moved (M.keys (hdWrites hd))
  where
    firstJust = foldr (\x acc -> maybe acc Just x) Nothing
    moved a = do
      now <- maybe (pure (M.lookup a (ssDocs s))) ($ a) (ssLiveDoc s)
      let base = M.findWithDefault Nothing a (hdBases hd)
      same <- case (M.findWithDefault Nothing a (hdVersions hd), now) of
        (Just v, Just (v', _)) -> pure (v == v')
        (_, Just (_, t)) -> pure (Just t == base)
        (_, Nothing) -> (== base) <$> readText (pFiles (ssPorts s)) a
      pure (if same then Nothing else Just a)

-- | The step under TOKEN removed: whether it was there.
takeShown :: ServerState -> Text -> IO Bool
takeShown s token = atomicModifyIORef' (ssShown s) (\(n, m) -> ((n, M.delete token m), M.member token m))

-- | Every step shown in FILE dropped, each recorded as such.
dropShown :: ServerState -> FilePath -> IO ()
dropShown s file = do
  gone <- atomicModifyIORef' (ssShown s) (\(n, m) -> let (out, keep) = M.partition ((== file) . hdFile) m in ((n, keep), M.elems out))
  forM_ gone (\hd -> record s hd "dropped")

-- | The step's line in `.olifant/state/conjure-steps.jsonl` and its words in
-- the step log, with the person's CHOICE.
record :: ServerState -> StepHeld -> Text -> IO ()
record s hd choice = do
  now <- pNow (ssPorts s)
  let path = hdRoot hd </> ".olifant/state/conjure-steps.jsonl"
      d = hdDelta hd
      line =
        object
          [ "time" .= stamp now
          , "file" .= makeRelative (hdRoot hd) (hdFile hd)
          , "unit" .= hdUnit hd
          , "writer" .= hdWriter hd
          , "goal" .= hdGoal hd
          , "files" .= map (makeRelative (hdRoot hd)) (M.keys (hdWrites hd))
          , "lines" .= hdLines hd
          , "passing" .= tdPassing d
          , "failing" .= tdFailing d
          , "stillRed" .= tdStillRed d
          , "noTests" .= tdNoTests d
          , "parses" .= and (map snd (hdParses hd))
          , "seconds" .= hdSeconds hd
          , "cost" .= hdCost hd
          , "choice" .= choice
          ]
      entry = TE.decodeUtf8 (BL.toStrict (encode line))
  void (try (readText (pFiles (ssPorts s)) path >>= \old -> writeText (pFiles (ssPorts s)) path (fromMaybe "" old <> entry <> "\n")) :: IO (Either SomeException ()))
  appendStepLine (ssPorts s) (hdRoot hd) [stepLogLine (hdWriter hd) (hdGoal hd) (M.size (hdWrites hd)) d choice]
  where
    stamp :: UTCTime -> String
    stamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- | The before-run in words, as the writer reads it.
testsWords :: Language -> Maybe TestRun -> Text
testsWords lang = \case
  Nothing -> "none (no test file, or the tests could not run)."
  Just r -> case trBroken r of
    Just why -> "they could not run: " <> T.intercalate " / " (take 6 (filter (not . T.null) (map T.strip (T.lines why))))
    Nothing ->
      T.pack (show (length (trPassed r))) <> " passing (" <> T.intercalate ", " (map short (trPassed r)) <> ")"
        <> (if null (trFailed r) then "; none failing." else "; failing: " <> T.intercalate "; " [short t <> ": " <> failureLineIn lang f | (t, f) <- trFailed r] <> ".")
  where
    short = snd . T.breakOnEnd "::"

-- | The changed lines in a unified diff.
diffLines :: Text -> Int
diffLines d = length [l | l <- T.lines d, Just c <- [fst <$> T.uncons l], c `elem` ['+', '-'], not ("+++" `T.isPrefixOf` l), not ("---" `T.isPrefixOf` l)]

-- | What the person has been doing: the project's uncommitted changes
-- (`git diff HEAD`) per file, most recently changed first, and the untracked
-- files by name, through 'sessionFilter' at 300 lines. Not a git repository,
-- or git failing: nothing.
sessionDiff :: FilePath -> [Text] -> IO Text
sessionDiff root exts =
  runBytes "git" ["-C", root, "diff", "HEAD", "--no-color", "--no-ext-diff"] Nothing "" >>= \case
    Right (ExitSuccess, out, _) -> do
      let files = perFile (TE.decodeUtf8Lenient out)
      timed <- mapM (\(p, d) -> (\t -> (t, (p, d))) . either (const Nothing) Just <$> (try (getModificationTime (root </> p)) :: IO (Either SomeException UTCTime))) files
      untracked <-
        runBytes "git" ["-C", root, "ls-files", "--others", "--exclude-standard"] Nothing "" <&> \case
          Right (ExitSuccess, o, _) -> [l | l <- T.lines (TE.decodeUtf8Lenient o), any (`T.isSuffixOf` l) exts]
          _ -> []
      let diffs = sessionFilter exts 300 (map snd (sortOn (Down . fst) timed))
      pure (diffs <> (if null untracked then "" else "New files, not yet in git: " <> T.intercalate ", " untracked <> "\n"))
    _ -> pure ""
  where
    perFile t = case T.splitOn "diff --git a/" t of
      (_ : chunks) -> [(T.unpack (fst (T.breakOn " b/" c)), "diff --git a/" <> c) | c <- chunks]
      _ -> []

-- | What `olifant/conjure` answers: a shown step (the edits as EDITS words
-- them, the editor's workspace edit), nothing to do, a landed step, or a
-- rejected one.
stepJson :: ([FileEdit] -> Value) -> StepOutcome -> Value
stepJson edits = \case
  StepShown sh ->
    object
      [ "shown"
          .= object
            [ "token" .= shToken sh
            , "file" .= shFile sh
            , "unit" .= shUnit sh
            , "writer" .= shWriter sh
            , "goal" .= shGoal sh
            , "edits" .= (if null (shEdits sh) then Null else edits (shEdits sh))
            , "dropped" .= shDropped sh
            , "parses" .= [object ["file" .= f, "ok" .= ok] | (f, ok) <- shParses sh]
            , "tests" .= object ["passing" .= tdPassing d, "failing" .= tdFailing d, "stillRed" .= tdStillRed d, "noTests" .= tdNoTests d]
            , "after" .= maybe Null testsJson (shTests sh)
            , "cost" .= shCost sh
            , "seconds" .= shSeconds sh
            , "harder" .= shHarder sh
            ]
      ]
    where
      d = shDelta sh
  StepNothing goal -> object ["nothing" .= goal]
  StepLanded l -> object ["landed" .= object ["file" .= lsFile l, "edits" .= (if null (lsEdits l) then Null else edits (lsEdits l))]]
  StepRejected -> object ["rejected" .= True]

-- | A request's `resume`: the step's token and the person's choice (accept,
-- reject or harder); Nothing when it has none, Left when it is not whole.
stepResumeOf :: Value -> Either Text (Maybe (Text, StepChoice))
stepResumeOf = \case
  Object o | Just r <- KM.lookup "resume" o -> case r of
    Object ro
      | Just (String tok) <- KM.lookup "token" ro
      , not (T.null tok)
      , Just (String c) <- KM.lookup "choice" ro
      , Just choice <- lookup c [("accept", StepAccept), ("reject", StepReject), ("harder", StepHarder)] ->
          Right (Just (tok, choice))
    _ -> Left "olifant/conjure resume needs a token and a choice (accept, reject or harder)"
  _ -> Right Nothing
