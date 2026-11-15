{-# LANGUAGE OverloadedStrings, LambdaCase #-}
-- | Vet 1a (experiment, not product code): the claim check on code review findings. For each item
-- of IN.json ({id, tree, file, line, statement}) it loads the tree as a project (a stub plan and a
-- minimal .olifant/config.toml are written into the tree), builds a prose 'Sentence' from the
-- statement and runs the engine's own 'checkSentence' under the project's [plans] defaults (the
-- cascade: haiku, Jev guards, sonnet on doubt) with 'CheckClaim'. OUT.json maps id to
-- {verdict, cost, secs, escalated, citations}.
--
-- Recordings (cwd = the olifant checkout): claims.json holds every model reply ('replayChecker'
-- format, keyed "MODEL|ID|sentence-key"); claims-results.json holds each item's finished result.
-- VET_LIVE=1 asks what is not recorded; without it everything replays and an unrecorded item
-- fails. JEV_API_KEY must be set when live. Stops before an item once the recorded reply cost
-- passes VET_CLAIMS_CAP (default 4.0 dollars).
module Main (main) where

import Control.Monad (foldM, forM_, when)
import Data.Aeson (FromJSON (..), Value (..), eitherDecodeFileStrict, encode, object, withObject, (.:), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import GHC.Clock (getMonotonicTime)
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..), PlansConfig (..))
import Olifant.Doctor (DoctorPorts (..))
import Olifant.Language (noChecker, noRunner, noSyntax)
import Olifant.Language.AstGrep (astGrepSyntax)
import Olifant.Language.Check (noVerifier)
import Olifant.Language.Grammar (GrammarFailure (NoSource))
import Olifant.Plans (Kind (Prose))
import Olifant.Plans.Sentences (Sentence (..), sentenceKey)
import Olifant.Process (gitStateStamp, neverCancel)
import Olifant.Server (newServerState, projectFor)
import Olifant.Server.Checker
import Olifant.Server.Claims (CheckRecord (..), Failure (..))
import Olifant.Server.Loop (checkSentence)
import Olifant.Server.State (ServerState (..))
import Olifant.Workspace (diskWorkspace)
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Text.Printf (printf)

here, claimsFile, resultsFile :: FilePath
here = "experiments/vet-1a-2026-11"
claimsFile = here </> "claims.json"
resultsFile = here </> "claims-results.json"

data Item = Item {itId :: Text, itTree :: FilePath, itFile :: Text, itLine :: Int, itStatement :: Text}

instance FromJSON Item where
  parseJSON = withObject "item" $ \o -> Item <$> o .: "id" <*> o .: "tree" <*> o .: "file" <*> o .: "line" <*> o .: "statement"

plainPorts :: Ports
plainPorts = Ports (diskWorkspace "/") noSyntax (const noChecker) (const noVerifier) (const noRunner) (\_ _ -> noRunner) getCurrentTime Nothing Nothing (DoctorPorts (\_ -> pure Nothing) (pure "") (\_ -> pure (Right ())) (\_ -> pure False) (\_ -> pure Nothing) (\_ -> pure []) (\_ -> pure (Left NoSource))) (\_ -> pure Nothing) (\_ _ _ -> pure (Left "no base")) (\_ _ -> pure (Left "no base")) gitStateStamp

configToml :: String
configToml = unlines
  [ "languages = [\"python\", \"haskell\"]"
  , "[model.backends.jev]"
  , "kind = \"jev\""
  , "[decide]"
  , "model = \"jev\"" ]

readObject :: FilePath -> IO (KM.KeyMap Value)
readObject f = doesFileExist f >>= \case
  False -> pure KM.empty
  True -> eitherDecodeFileStrict f >>= \case
    Right (Object o) -> pure o
    _ -> pure KM.empty

-- | Dollars in the recorded replies.
recordedSpend :: IO Double
recordedSpend = do
  o <- readObject claimsFile
  pure $ case KM.lookup "replies" o of
    Just (Object rs) -> sum [toRealFloat n | Object r <- KM.elems rs, Just (Number n) <- [KM.lookup "cost" r]]
    _ -> 0

main :: IO ()
main = do
  [inFile, outFile] <- getArgs
  live <- (== Just "1") <$> lookupEnv "VET_LIVE"
  cap <- maybe 4.0 read <$> lookupEnv "VET_CLAIMS_CAP"
  key <- lookupEnv "JEV_API_KEY"
  when (live && key == Nothing) $ hPutStrLn stderr "JEV_API_KEY unset" >> exitFailure
  items <- (either fail pure =<< eitherDecodeFileStrict inFile) :: IO [Item]
  tmp <- getTemporaryDirectory
  s0 <- newServerState plainPorts {pSyntax = astGrepSyntax (tmp </> "olifant-test-cache")}
  done0 <- readObject resultsFile
  (_, out) <- foldM (\(s, acc) it -> do
      let k = Key.fromText (itId it)
      case KM.lookup k done0 of
        Just v -> pure (s, KM.insert k v acc)
        Nothing | not live -> hPutStrLn stderr ("unrecorded: " <> T.unpack (itId it)) >> exitFailure
        Nothing -> do
          spent <- recordedSpend
          when (spent > cap) $ hPutStrLn stderr (printf "recorded spend $%.3f past $%.2f: stopping" spent cap) >> exitFailure
          (s', v) <- runItem live s it
          -- keep each finished item at once, so an interrupted run resumes
          cur <- readObject resultsFile
          BL.writeFile resultsFile (encode (Object (KM.insert k v cur)) <> "\n")
          pure (s', KM.insert k v acc)) (s0, KM.empty) items
  BL.writeFile outFile (encode (Object out) <> "\n")
  printf "%d items\n" (KM.size out)

runItem :: Bool -> ServerState -> Item -> IO (ServerState, Value)
runItem live s0 it = do
  let tree = itTree it
  createDirectoryIfMissing True (tree </> "plans")
  createDirectoryIfMissing True (tree </> ".olifant")
  writeFile (tree </> "plans/vet-1a.md") "# Vet 1a\n\n- stub\n"
  writeFile (tree </> ".olifant/config.toml") configToml
  (s, p) <-
    projectFor (tree </> "plans/vet-1a.md") s0 >>= \case
      (s, Just (_, p)) -> pure (s, p)
      _ -> fail ("the tree does not load as a project: " <> tree)
  let cfg = catalogPlans (prCatalog p)
      text = itStatement it
      x = Sentence 0 0 0 (T.length text) Prose text "" Nothing Nothing
      ask = CheckAsk CheckClaim False []
      -- every reply is recorded under "MODEL|ID|key" (the cascade asks two models the same sentence)
      checkerFor c a sn =
        let keyed = sn {snText = pcCheckModel' c <> "|" <> itId it <> "|" <> snText sn}
            liveRun = if live then Just (\a' _ -> liveChecker neverCancel c tree a' sn) else Nothing
         in replayChecker claimsFile liveRun a keyed
  now <- getCurrentTime
  t0 <- getMonotonicTime
  (r, cost) <- checkSentence s p cfg checkerFor False now (tree </> "plans/vet-1a.md") ask x
  t1 <- getMonotonicTime
  let v = case r of
        Left f -> object ["verdict" .= ("failed" :: Text), "why" .= fuWhy' f, "cost" .= cost, "secs" .= (t1 - t0), "escalated" .= False, "citations" .= ([] :: [Text])]
        Right c ->
          object [ "verdict" .= judgementName (cfJudgement (cjConfirmed c)), "cost" .= cost, "secs" .= (t1 - t0)
                 , "escalated" .= cjEscalated c, "model" .= cjModel c
                 , "citations" .= [T.pack (ciPath ci) <> maybe "" (":" <>) (ciUnit ci) <> maybe "" (\l -> "@" <> T.pack (show l)) (ciLine ci) | ci <- cfEvidence (cjConfirmed c)]
                 , "why" .= cfWhy (cjConfirmed c) ]
  printf "%-22s %s $%.4f %.1fs\n" (T.unpack (itId it)) (T.unpack (case r of Left _ -> "failed"; Right c -> judgementName (cfJudgement (cjConfirmed c)))) cost (t1 - t0)
  hFlush stdout
  pure (s, v)
  where
    pcCheckModel' = pcCheckModel
    fuWhy' = fuWhy
