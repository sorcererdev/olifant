{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.GeneratorTestingSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.GeneratorTesting
import Data.Map.Strict qualified as M
import Data.Text.IO qualified as TIO
import Olifant.Language (compileClean, noChecker, noSyntax, parseLanguage, processChecker)
import Olifant.Workspace (Workspace (..))
import System.Directory (findExecutable)
import Olifant.Language.AstGrep (astGrepSyntax)
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import Olifant.Workspace (diskWorkspace, memoryWorkspace)
import System.Environment (lookupEnv)
import Test.Hspec

tinyPack :: Text -> [(FilePath, Text)]
tinyPack snapshotContent =
  [ ("/packs/p/generators/g/generator.toml", "description = \"g\"\nlanguage = \"x\"\n[inputs.name]\ntype = \"ident\"\n[[edits]]\ncreate = \"out/{= name|snake =}.txt\"\ntext = \"hello {= name|pascal =}\\n\"\n")
  , ("/packs/p/fixtures/app/README", "fixture\n")
  , ("/packs/p/generators/g/tests/basic/inputs.toml", "now = \"2026-09-27T15:30:00Z\"\n[inputs]\nname = \"blog_post\"\n")
  , ("/packs/p/generators/g/tests/basic/snapshot/out/blog_post.txt", snapshotContent)
  ]

runTiny :: [(FilePath, Text)] -> IO CaseResult
runTiny files = do
  (root, _) <- memoryWorkspace files
  [c] <- discoverCases root "/packs"
  runCase root noSyntax noChecker False c

spec :: Spec
spec = do
  describe "runCase" $ do
    it "passes when output matches the snapshot and re-running changes nothing" $
      runTiny (tinyPack "hello BlogPost\n") `shouldReturn` CasePassed
    it "fails with the differing file when output differs" $
      runTiny (tinyPack "hello Wrong\n") >>= \case
        CaseFailed msgs -> T.unlines msgs `shouldSatisfy` T.isInfixOf "out/blog_post.txt"
        other -> expectationFailure ("expected a failure, got " <> show other)
    it "fails when a generated file is missing from the snapshot" $
      runTiny (take 3 (tinyPack "")) >>= \case
        CaseFailed msgs -> T.unlines msgs `shouldSatisfy` T.isInfixOf "not in snapshot"
        other -> expectationFailure ("expected a failure, got " <> show other)

    it "reports a generation error in words, not as a Haskell value" $
      runTiny (map (\(p, c) -> (p, T.replace "hello {= name|pascal =}" "hello {= nope =}" c)) (tinyPack "")) >>= \case
        CaseFailed msgs -> do
          T.unlines msgs `shouldSatisfy` T.isInfixOf "undefined variable `nope`"
          T.unlines msgs `shouldNotSatisfy` T.isInfixOf "TemplateFailed"
        other -> expectationFailure ("expected a failure, got " <> show other)
    it "blessing removes snapshot files the case no longer generates" $ do
      (root, snap) <- memoryWorkspace (tinyPack "old\n" ++ [("/packs/p/generators/g/tests/basic/snapshot/out/stale.txt", "x\n")])
      [c] <- discoverCases root "/packs"
      runCase root noSyntax noChecker True c `shouldReturn` CasePassed
      files <- snap
      M.member "/packs/p/generators/g/tests/basic/snapshot/out/stale.txt" files `shouldBe` False
      M.lookup "/packs/p/generators/g/tests/basic/snapshot/out/blog_post.txt" files `shouldBe` Just "hello BlogPost\n"
    it "resolves choices_from in a [[uses]] generator, from the fixture" $ do
      let files =
            [ ("/packs/p/generators/outer/generator.toml", "description = \"o\"\nlanguage = \"x\"\n[inputs.svc]\ntype = \"text\"\n[[uses]]\ngenerator = \"inner\"\nwith = { svc = \"{= svc =}\" }\n")
            , ("/packs/p/generators/inner/generator.toml", "description = \"i\"\nlanguage = \"x\"\n[inputs.svc]\ntype = \"enum\"\nchoices_from = { glob = \"svc/*\", value = \"{= stem =}\" }\n[[edits]]\ncreate = \"out.txt\"\ntext = \"{= svc =}\\n\"\n")
            , ("/packs/p/fixtures/app/svc/billing", "")
            , ("/packs/p/generators/outer/tests/basic/inputs.toml", "now = \"2026-09-27T15:30:00Z\"\n[inputs]\nsvc = \"billing\"\n")
            , ("/packs/p/generators/outer/tests/basic/snapshot/out.txt", "billing\n")
            ]
      (root, _) <- memoryWorkspace files
      [c] <- discoverCases root "/packs"
      runCase root noSyntax noChecker False c `shouldReturn` CasePassed
    it "runs command queries in the fixture directory" $ do
      tmp <- getTemporaryDirectory
      -- Rooted at / with absolute paths, like the real pack runs (rooted at .).
      let dir = tmp </> "olifant-command-case"
          ws = diskWorkspace "/"
      forM_
        [ ("p/generators/g/generator.toml", "description = \"g\"\nlanguage = \"x\"\n[inputs.svc]\ntype = \"enum\"\nchoices_from = { command = [\"cat\", \"svcs.json\"], value = \"{= name =}\" }\n[[edits]]\ncreate = \"out.txt\"\ntext = \"{= svc =}\\n\"\n")
        , ("p/fixtures/app/svcs.json", "[{\"name\": \"billing\"}]")
        , ("p/generators/g/tests/basic/inputs.toml", "now = \"2026-09-27T15:30:00Z\"\n[inputs]\nsvc = \"billing\"\n")
        , ("p/generators/g/tests/basic/snapshot/out.txt", "billing\n")
        ]
        (\(p, t) -> writeText ws (dir </> p) t)
      [c] <- discoverCases ws dir
      runCase ws noSyntax noChecker False c `shouldReturn` CasePassed

  describe "packs" $ do
    cases <- runIO (discoverCases (diskWorkspace ".") "packs")
    bless <- runIO ((== Just "1") <$> lookupEnv "OLIFANT_BLESS")
    strict <- runIO ((== Just "1") <$> lookupEnv "OLIFANT_REQUIRE_TOOLS")
    it "has pack cases to run" $ cases `shouldNotBe` []
    tmp <- runIO getTemporaryDirectory
    forM_ cases $ \c ->
      it (T.unpack (caseName c)) $
        runCase (diskWorkspace ".") (astGrepSyntax (tmp </> "olifant-test-cache")) processChecker bless c >>= \case
          CaseSkipped why -> missingTool strict (T.unpack why)
          other -> other `shouldBe` CasePassed

  -- Structure-placed code must still build: fixture + each Elm case's output.
  describe "elm pack output compiles" $ do
    cases <- runIO (discoverCases (diskWorkspace ".") "packs")
    strict <- runIO ((== Just "1") <$> lookupEnv "OLIFANT_REQUIRE_TOOLS")
    elmSrc <- runIO (TIO.readFile "packs/elm/languages/elm.toml")
    let Right elm = parseLanguage "elm" "packs/elm" elmSrc
    forM_ [c | c <- cases, casePack c == "elm"] $ \c ->
      it (T.unpack (caseName c)) $
        findExecutable "elm" >>= \case
          Nothing -> missingTool strict "elm is not installed"
          Just _ -> do
            fixture <- readTree "packs/elm/fixtures/app"
            snapshot <- readTree (caseDir c </> "snapshot")
            compileClean elm (M.union snapshot fixture) `shouldReturn` Right ()
  where
    -- Pending locally; a failure where tools are required (the pre-merge hook
    -- sets OLIFANT_REQUIRE_TOOLS=1), so a missing tool cannot look green.
    missingTool strict why = if strict then expectationFailure ("required tool missing: " <> why) else pendingWith why
    readTree dir = do
      let ws = diskWorkspace "."
      paths <- filter (\p -> takeWhile (/= '/') p /= "elm-stuff") <$> listFiles ws dir
      M.fromList <$> traverse (\p -> (p,) . maybe "" id <$> readText ws (dir </> p)) paths
