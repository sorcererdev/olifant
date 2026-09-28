{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.GeneratorTestingSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.GeneratorTesting
import Olifant.Workspace (diskWorkspace, memoryWorkspace)
import System.Environment (lookupEnv)
import Test.Hspec

tinyPack :: Text -> [(FilePath, Text)]
tinyPack snapshotContent =
  [ ("/packs/p/generators/g/generator.toml", "description = \"g\"\nlanguage = \"x\"\n[inputs.name]\ntype = \"ident\"\n[[edits]]\ncreate = \"out/{{ name|snake }}.txt\"\ntext = \"hello {{ name|pascal }}\\n\"\n")
  , ("/packs/p/fixtures/app/README", "fixture\n")
  , ("/packs/p/generators/g/tests/basic/inputs.toml", "now = \"2026-09-27T15:30:00Z\"\n[inputs]\nname = \"blog_post\"\n")
  , ("/packs/p/generators/g/tests/basic/snapshot/out/blog_post.txt", snapshotContent)
  ]

runTiny :: [(FilePath, Text)] -> IO CaseResult
runTiny files = do
  (root, _) <- memoryWorkspace files
  [c] <- discoverCases root "/packs"
  runCase root False c

spec :: Spec
spec = do
  describe "runCase" $ do
    it "passes when output matches the snapshot and re-running changes nothing" $
      runTiny (tinyPack "hello BlogPost\n") `shouldReturn` CasePassed
    it "fails with the differing file when output differs" $
      runTiny (tinyPack "hello Wrong\n") >>= \case
        CaseFailed msgs -> T.unlines msgs `shouldSatisfy` T.isInfixOf "out/blog_post.txt"
        CasePassed -> expectationFailure "expected a failure"
    it "fails when a generated file is missing from the snapshot" $
      runTiny (take 3 (tinyPack "")) >>= \case
        CaseFailed msgs -> T.unlines msgs `shouldSatisfy` T.isInfixOf "not in snapshot"
        CasePassed -> expectationFailure "expected a failure"

  describe "packs" $ do
    cases <- runIO (discoverCases (diskWorkspace ".") "packs")
    bless <- runIO ((== Just "1") <$> lookupEnv "OLIFANT_BLESS")
    it "has pack cases to run" $ cases `shouldNotBe` []
    forM_ cases $ \c ->
      it (T.unpack (caseName c)) $ runCase (diskWorkspace ".") bless c `shouldReturn` CasePassed
