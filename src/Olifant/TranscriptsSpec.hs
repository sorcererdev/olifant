module Olifant.TranscriptsSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.Catalog (defaultPrices)
import Olifant.Server.Errand (Tokens (..))
import Olifant.Transcripts
import Olifant.Workspace (diskWorkspace, listFiles)
import Test.Hspec

fixtureDir :: FilePath
fixtureDir = "src/Olifant/testdata/transcripts/-proj"

fixtureLines :: IO [Text]
fixtureLines = T.lines <$> TIO.readFile (fixtureDir <> "/a.jsonl")

sonnetMsg :: Message
sonnetMsg =
  Message
    { msSession = "S1", msBranch = Just "feat", msAt = UTCTime (fromGregorian 2026 10 25) 36000
    , msModel = "claude-sonnet-5-5", msTokens = Tokens 100 50 1000 4000, msWrite1h = 1000, msKey = ("m1", "r1") }

spec :: Spec
spec = describe "Olifant.Transcripts" $ do
  it "slugOf replaces every non-alphanumeric character" $
    slugOf "/Users/x/projects/olifant" `shouldBe` "-Users-x-projects-olifant"

  it "parses an assistant line and skips lines without usage" $ do
    ls <- fixtureLines
    let (userLine, assistantLine) = (ls !! 0, ls !! 1)
    parseLine userLine `shouldBe` Nothing
    parseLine "not json, though it says usage" `shouldBe` Nothing
    fmap (\m -> (msSession m, msBranch m, msModel m, msTokens m, msWrite1h m)) (parseLine assistantLine)
      `shouldBe` Just ("S1", Just "feat", "claude-sonnet-5-5", Tokens 100 50 1000 4000, 1000)
    parseLine assistantLine `shouldBe` Just sonnetMsg

  it "a line without gitBranch has no branch" $ do
    ls <- fixtureLines
    fmap msBranch (parseLine (ls !! 3)) `shouldBe` Just Nothing

  it "a line without cache_creation prices all writes at the 1h rate" $ do
    ls <- fixtureLines
    let noSplitLine = ls !! 3
    fmap msWrite1h (parseLine noSplitLine) `shouldBe` Just 0
    let withWrites = T.replace "\"cache_creation_input_tokens\":0" "\"cache_creation_input_tokens\":300" noSplitLine
    fmap msWrite1h (parseLine withWrites) `shouldBe` Just 300

  it "dedupe keeps one message per (id, requestId)" $ do
    let m = sonnetMsg
    length (dedupe [m, m, m {msKey = ("m2", "r1")}]) `shouldBe` 2
    dedupe [m, m {msModel = "later"}] `shouldBe` [m]

  it "messageCost follows the table and is Nothing for unknown models" $ do
    messageCost defaultPrices sonnetMsg `shouldBe` Just ((100 * 2 + 50 * 10 + 4000 * 0.2 + 1000 * 4) / 1e6)
    messageCost defaultPrices sonnetMsg {msWrite1h = 400}
      `shouldBe` Just ((100 * 2 + 50 * 10 + 4000 * 0.2 + 400 * 4 + 600 * 2.5) / 1e6)
    messageCost defaultPrices sonnetMsg {msModel = "claude-unknown-9"} `shouldBe` Nothing

  it "readTranscripts walks subdirectories and dedupes" $ do
    ms <- readTranscripts (listFiles (diskWorkspace ".")) (fmap Just . TIO.readFile) fixtureDir
    map (fst . msKey) ms `shouldMatchList` ["m1", "m2", "m3", "m4"]
    [msSession m | m <- ms, fst (msKey m) == "m4"] `shouldBe` ["S2"]
