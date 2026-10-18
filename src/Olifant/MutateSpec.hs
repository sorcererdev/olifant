{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.MutateSpec (spec) where

import Control.Monad (forM)
import Data.List (find, sort)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Olifant.Toml (int, list, optional, parseToml, required, table, text)
import Olifant.Process (neverCancel)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Language (Failure (..), Runner (..), Syntax, TestRun (..), parseLanguage, processRunner, runTestsIn)
import Olifant.Language.AstGrep (astGrepSyntax)
import Olifant.Mutate
import Olifant.Mutate.Python (pythonOperators)
import System.Directory (findExecutable, getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Mutate" $ do
  pythonToml <- runIO (TIO.readFile "packs/python-htmx/languages/python.toml")
  strict <- runIO ((== Just "1") <$> lookupEnv "OLIFANT_REQUIRE_TOOLS")
  tmp <- runIO getTemporaryDirectory
  let Right py = parseLanguage "python" "packs/python-htmx" pythonToml
      body =
        T.unlines
          [ "def handle(self, input):",
            "    table = self.bookings.table(input.table_id)",
            "    if table is None:",
            "        return TableNotFound(f\"there is no table {input.table_id}\")",
            "    if input.party > table.seats:",
            "        return PartyTooLarge(\"too many\")",
            "    return Ok(table)"
          ]
      allLines = S.fromList [1 .. 7]
      -- the real ast-grep; a missing one is pending, or a failure when
      -- OLIFANT_REQUIRE_TOOLS=1 so a missing tool cannot look green
      withSg :: (Syntax IO -> Expectation) -> Expectation
      withSg act =
        findExecutable "ast-grep" >>= \case
          Nothing -> if strict then expectationFailure "required tool missing: ast-grep is not installed" else pendingWith "ast-grep is not installed (brew install ast-grep)"
          Just _ -> act (astGrepSyntax (tmp </> "olifant-test-cache"))
  it "(a) boundary: `>` becomes `>=` on its line" $ withSg $ \sg -> do
    Right ms <- mutantsOf sg py pythonOperators body allLines
    [(muLine m, muTo m) | m <- ms, muOperator m == "boundary"] `shouldBe` [(5, ">=")]
    let Just m = find ((== "boundary") . muOperator) ms
    T.lines (muText m) !! 4 `shouldBe` "    if input.party >= table.seats:"
  it "(b) strings become empty; a docstring is not a site" $ withSg $ \sg -> do
    Right ms <- mutantsOf sg py pythonOperators (T.unlines ["def f():", "    \"\"\"doc\"\"\"", "    return \"x\""]) (S.fromList [1, 2, 3])
    [(muLine m, muTo m) | m <- ms, muOperator m == "string"] `shouldBe` [(3, "\"\"")]
  it "(c) return value becomes None; `return None` is not a site (same text dropped)" $ withSg $ \sg -> do
    Right ms <- mutantsOf sg py pythonOperators (T.unlines ["def f():", "    return None", "def g():", "    return 1"]) (S.fromList [1 .. 4])
    [muLine m | m <- ms, muOperator m == "return-none"] `shouldBe` [4]
  it "(d) only lines in scope are sites" $ withSg $ \sg -> do
    Right ms <- mutantsOf sg py pythonOperators body (S.fromList [7])
    map muLine ms `shouldSatisfy` all (== 7)
  it "(e) a call statement is deleted (becomes pass); `is None` is negated; and/or swap" $ withSg $ \sg -> do
    Right ms <- mutantsOf sg py pythonOperators (T.unlines ["def f(a, b):", "    log(a)", "    if a is None and b:", "        pass"]) (S.fromList [1 .. 4])
    sort [(muOperator m, muLine m) | m <- ms] `shouldBe` sort [("delete-call", 2), ("negate", 3), ("and-or", 3)]
  it "(f) mutants are in file order and capped" $ withSg $ \sg -> do
    Right ms <- mutantsOf sg py pythonOperators body allLines
    map muLine ms `shouldBe` sort (map muLine ms)
    length (capMutants 2 ms) `shouldBe` 2
  it "(i) the number operator is a transform: 4 becomes 5" $ withSg $ \sg -> do
    Right ms <- mutantsOf sg py pythonOperators (T.unlines ["def f():", "    return x + 4"]) (S.fromList [1, 2])
    [(muOperator m, muTo m) | m <- ms, muLine m == 2] `shouldSatisfy` elem ("number", "5")

  describe "judge" $ do
    let fake outcome = Runner (\_ _ fs -> pure (outcome fs))
        m1 = Mutant 2 "boundary" "<" "<=" "MUTANT-1" Nothing False
        m2 = Mutant 3 "string" "\"x\"" "\"\"" "MUTANT-2" Nothing False
        files = M.fromList [("src/a.py", "ORIGINAL"), ("src/a__test.py", "t")]
    it "(a) a mutant whose tests fail is killed; one whose tests pass survives" $ do
      Right vs <- judge (fake (\fs -> Right (if M.lookup "src/a.py" fs == Just "MUTANT-1" then TestRun [] [("t", Failure Nothing "x" "")] Nothing else TestRun ["t"] [] Nothing))) 2 py "src/a.py" files "src/a__test.py" ["t"] [m1, m2]
      map snd vs `shouldBe` [Killed, Survived]
    it "(b) results keep the mutants' order whatever finishes first" $ do
      Right vs <- judge (fake (const (Right (TestRun ["t"] [] Nothing)))) 4 py "src/a.py" files "src/a__test.py" ["t"] [m1, m2]
      map fst vs `shouldBe` [m1, m2]
    it "(c) a timed-out or broken run is killed" $ do
      Right vs <- judge (fake (const (Right (TestRun [] [] (Just "the tests timed out (30 s)"))))) 2 py "src/a.py" files "src/a__test.py" ["t"] [m1]
      map snd vs `shouldBe` [Killed]
    it "(d) the runner failing to start is reported, not a verdict" $
      judge (fake (const (Left "uv is not installed"))) 2 py "src/a.py" files "src/a__test.py" ["t"] [m1] `shouldReturn` Left "uv is not installed"
    it "(g) each mutant's run gets five times the baseline's wall time: at least 5 s, at most 120 s" $
      map judgeTimeout [0.2, 1.01, 3, 30] `shouldBe` [5, 6, 15, 120]
    it "(e) judged by the tests that passed before only: a sibling test red on every run kills nothing" $ do
      let sibling fs = Right (TestRun ["t" | M.lookup "src/a.py" fs /= Just "MUTANT-1"] ([("u", Failure Nothing "red before" "")] ++ [("t", Failure Nothing "x" "") | M.lookup "src/a.py" fs == Just "MUTANT-1"]) Nothing)
      Right vs <- judge (fake sibling) 2 py "src/a.py" files "src/a__test.py" ["t"] [m1, m2]
      map snd vs `shouldBe` [Killed, Survived]
    it "(f) with no test that passed before, nothing is judged" $
      judge (fake (const (Right (TestRun [] [] Nothing)))) 2 py "src/a.py" files "src/a__test.py" [] [m1] `shouldReturn` Left "no passing test to judge by"

  describe "pack cases" $ sequential $ do
    bless <- runIO ((== Just "1") <$> lookupEnv "OLIFANT_BLESS")
    let caseFile = "packs/python-htmx/tests/mutate/create-booking/case.toml"
    it "python-htmx create-booking: the survivors are the ones the case names" $ do
      uvPath <- findExecutable "uv"
      sgPath <- findExecutable "ast-grep"
      case (uvPath, sgPath) of
        (Nothing, _) -> missing "uv is not installed (brew install uv)"
        (_, Nothing) -> missing "ast-grep is not installed (brew install ast-grep)"
        _ -> do
          Right tbl <- parseToml <$> TIO.readFile caseFile
          let Right (file, linesSpec, expected) = (,,) <$> required "file" text tbl <*> required "lines" text tbl <*> required "survivors" (list (\t -> table t >>= \r -> (,,) <$> (fromInteger <$> required "line" int r) <*> required "operator" text r <*> optional "to" text r)) tbl
              [lo, hi] = map (read . T.unpack) (T.splitOn "-" linesSpec) :: [Int]
          project <- readTree "packs/python-htmx/fixtures/app"
          let Just source = M.lookup (T.unpack file) project
          started <- getCurrentTime
          Right ms <- mutantsOf (astGrepSyntax (tmp </> "olifant-test-cache")) py pythonOperators source (S.fromList [lo .. hi])
          let testFile = "src/bistro/booking/commands/create_booking__test.py"
          Right baseline <- runTestsIn (processRunner neverCancel 60) py [testFile] project
          Right verdicts <- judge (processRunner neverCancel 60) 4 py (T.unpack file) project testFile (trPassed baseline) (capMutants 20 ms)
          finished <- getCurrentTime
          putStrLn ("  mutants judged: " <> show (length verdicts) <> ", wall time " <> show (diffUTCTime finished started))
          let actual = [(muLine m, muOperator m, muTo m) | (m, Survived) <- verdicts]
          if bless
            then TIO.writeFile caseFile (blessed file linesSpec actual)
            else do
              sort [(l, o) | (l, o, _) <- actual] `shouldBe` sort [(l, o) | (l, o, _) <- expected]
              [(l, o, t) | (l, o, Just t) <- expected] `shouldSatisfy` all (`elem` actual)
  where
    missing why = do
      strict <- (== Just "1") <$> lookupEnv "OLIFANT_REQUIRE_TOOLS"
      if strict then expectationFailure ("required tool missing: " <> why) else pendingWith why
    blessed :: Text -> Text -> [(Int, Text, Text)] -> Text
    blessed file linesSpec actual =
      T.unlines $
        [ "# create_booking's handle, as the fixture has it, judged by its own tests.",
          "file  = \"" <> file <> "\"",
          "lines = \"" <> linesSpec <> "\"",
          "# What the fixture's four tests do not notice. Blessed with OLIFANT_BLESS=1.",
          "survivors = ["
        ]
          ++ ["  { line = " <> T.pack (show l) <> ", operator = \"" <> o <> "\"" <> (if o == "boundary" then ", to = \"" <> t <> "\"" else "") <> " }," | (l, o, t) <- actual]
          ++ ["]"]

readTree :: FilePath -> IO (M.Map FilePath Text)
readTree root = M.fromList <$> go root
  where
    go d = do
      names <- listDirectory d
      fmap concat . forM names $ \n -> do
        let p = d </> n
        isDir <- doesDirectoryExist p
        if isDir then go p else (\t -> [(makeRelative root p, t)]) <$> TIO.readFile p
