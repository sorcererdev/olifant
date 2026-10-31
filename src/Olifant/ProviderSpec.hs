module Olifant.ProviderSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Either (isLeft)
import Data.Text qualified as T
import Olifant.Provider
import System.Directory (copyFile, getCurrentDirectory, getPermissions, setOwnerExecutable, setPermissions)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

-- | A provider run as `sh <script>` from this folder, as a project root.
script :: FilePath -> IO Provider
script name = do
  here <- getCurrentDirectory
  pure (Provider ["sh", T.pack ("testdata/providers" </> name)] (Just (here </> "src/Olifant")))

spec :: Spec
spec = describe "Olifant.Provider" $ do
  it "a provider's units decode" $ do
    p <- script "units.sh"
    answer <- runProvider p (object ["text" .= ("x" :: T.Text)])
    (answer >>= decodeUnits 9) `shouldBe` Right [UnitAnswer "add" 3 5 Nothing]

  it "the request carries v 1 beside the caller's keys" $ do
    p <- script "echo-request.sh"
    runProvider p (object ["text" .= ("abc" :: T.Text)]) >>= \case
      Right (Object o) -> do
        KM.lookup "v" o `shouldBe` Just (Number 1)
        KM.lookup "text" o `shouldBe` Just (String "abc")
      other -> expectationFailure (show other)

  describe "a provider that cannot answer is a Left with one line of reason" $ do
    it "a missing program" $
      runProvider (Provider ["olifant-no-such-provider"] Nothing) (object [])
        `shouldReturn` Left "`olifant-no-such-provider` is not installed"
    it "a missing program named by a path under the root" $
      withSystemTempDirectory "olifant-provider" $ \dir ->
        runProvider (Provider ["./tools/none.sh"] (Just dir)) (object [])
          `shouldReturn` Left "`./tools/none.sh` is not installed"
    it "a non-zero exit: stderr's first line" $ do
      p <- script "fails.sh"
      runProvider p (object []) `shouldReturn` Left "no grammar"
    it "an answer that is not JSON" $ do
      p <- script "bad-json.sh"
      r <- runProvider p (object [])
      r `shouldSatisfy` either ("its answer is not JSON: " `T.isPrefixOf`) (const False)
    it "a provider that hangs is stopped after 10 s" $ do
      p <- script "sleeps.sh"
      timeout (15 * 1000000) (runProvider p (object [])) `shouldReturn` Just (Left "it timed out (10 s)")

  describe "decodeUnits rejects an answer as a whole, naming the first bad entry" $ do
    let unit l e = object ["name" .= ("f" :: T.Text), "line" .= (l :: Int), "end" .= (e :: Int)]
    it "an end past the text" $
      decodeUnits 4 (toArray [unit 1 9]) `shouldSatisfy` leftNaming "entry 0"
    it "an end before its line" $
      decodeUnits 4 (toArray [unit 3 2]) `shouldSatisfy` leftNaming "entry 0"
    it "a line before the first" $
      decodeUnits 4 (toArray [unit 0 2]) `shouldSatisfy` leftNaming "entry 0"
    it "a duplicate span, after good entries" $
      decodeUnits 9 (toArray [unit 1 2, unit 3 4, unit 3 4]) `shouldSatisfy` leftNaming "entry 2"
    it "a kind is kept" $
      decodeUnits 4 (toArray [object ["name" .= ("C" :: T.Text), "line" .= (1 :: Int), "end" .= (2 :: Int), "kind" .= ("class" :: T.Text)]])
        `shouldBe` Right [UnitAnswer "C" 1 2 (Just "class")]

  describe "decodeAnchor and decodeArgs" $ do
    it "null is no anchor; a line may be one past the end; columns start at 1" $ do
      decodeAnchor 4 Null `shouldBe` Right Nothing
      decodeAnchor 4 (object ["line" .= (5 :: Int), "column" .= (1 :: Int)]) `shouldBe` Right (Just (AnchorAnswer 5 1))
      decodeAnchor 4 (object ["line" .= (6 :: Int), "column" .= (1 :: Int)]) `shouldSatisfy` isLeft
      decodeAnchor 4 (object ["line" .= (2 :: Int), "column" .= (0 :: Int)]) `shouldSatisfy` isLeft
    it "an argv is a list of strings" $ do
      decodeArgs (toArray [String "a", String "b"]) `shouldBe` Right ["a", "b"]
      decodeArgs (toArray [Number 1]) `shouldSatisfy` isLeft

  it "providerPure runs a program once per request" $
    withSystemTempDirectory "olifant-provider" $ \dir -> do
      here <- getCurrentDirectory
      let logFile = dir </> "runs"
          p = Provider ["sh", "testdata/providers/echo-request.sh", T.pack logFile] (Just (here </> "src/Olifant"))
          req = object ["file" .= ("a.py" :: T.Text), "text" .= ("x" :: T.Text)]
      fmap isLeft (pure $! providerPure p req) `shouldReturn` False
      fmap isLeft (pure $! providerPure p req) `shouldReturn` False
      lines <$> readFile logFile `shouldReturn` ["ran"]
      -- an edit is a new request: asked again
      fmap isLeft (pure $! providerPure p (object ["file" .= ("a.py" :: T.Text), "text" .= ("y" :: T.Text)])) `shouldReturn` False
      length . lines <$> readFile logFile `shouldReturn` 2

  it "a relative argv[0] runs from the root, whatever the cwd" $
    withSystemTempDirectory "olifant-provider" $ \dir -> do
      copyFile "src/Olifant/testdata/providers/units.sh" (dir </> "units.sh")
      perms <- getPermissions (dir </> "units.sh")
      setPermissions (dir </> "units.sh") (setOwnerExecutable True perms)
      (runProvider (Provider ["./units.sh"] (Just dir)) (object []) >>= pure . (>>= decodeUnits 9))
        `shouldReturn` Right [UnitAnswer "add" 3 5 Nothing]
  where
    toArray = Array . foldMap pure
    leftNaming needle = either (needle `T.isInfixOf`) (const False)
