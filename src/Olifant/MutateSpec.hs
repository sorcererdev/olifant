{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.MutateSpec (spec) where

import Data.List (find, sort)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Language (Syntax, parseLanguage)
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
  it "(g) a waiver is per line and per operator (Review Focus 4)" $ do
    let waived = waiverLine py body 5 "boundary" "inclusive by design"
    T.lines waived !! 4 `shouldBe` "    if input.party > table.seats:  # olifant: boundary ok: inclusive by design"
    waiverOn py waived 5 "boundary" `shouldBe` Just "inclusive by design"
    waiverOn py waived 5 "equality" `shouldBe` Nothing
    waiverOn py waived 4 "boundary" `shouldBe` Nothing
  it "(h) a waived site's mutant carries its reason" $ withSg $ \sg -> do
    Right ms <- mutantsOf sg py pythonOperators (waiverLine py body 5 "boundary" "r") allLines
    [muWaived m | m <- ms, muOperator m == "boundary"] `shouldBe` [Just "r"]
  it "(i) the number operator is a transform: 4 becomes 5" $ withSg $ \sg -> do
    Right ms <- mutantsOf sg py pythonOperators (T.unlines ["def f():", "    return x + 4"]) (S.fromList [1, 2])
    [(muOperator m, muTo m) | m <- ms, muLine m == 2] `shouldSatisfy` elem ("number", "5")
