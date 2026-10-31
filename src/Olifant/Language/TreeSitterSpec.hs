module Olifant.Language.TreeSitterSpec (spec) where

import Control.Concurrent.Async (forConcurrently)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_)
import Data.Either (isLeft)
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Time (diffUTCTime, getCurrentTime)
import GHC.Conc (getNumCapabilities)
import Olifant.Anchor (RenderedAnchor (..), locLine, resolveLayers)
import Olifant.Language (Language, parseLanguage)
import Olifant.Language.Adapter (parseOverShipped)
import Olifant.Language.AstGrep (astGrepSyntax)
import System.Directory (getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Language.Captures (Cap (..), PNode (..))
import Olifant.Language.TreeSitter (cacheSize, capturesOf, linkedGrammars, queriesFor)
import Test.Hspec

rust :: Text -> [Cap]
rust src = either (error . T.unpack) snd (capturesOf "rust" (queriesFor "rust") src)

-- | The definitions of a text: (capture, @name of the same match).
defs :: [Cap] -> [(Text, Text)]
defs caps =
  [ (capName c, pText (capNode n))
  | c <- caps
  , "definition." `T.isPrefixOf` capName c
  , Just n <- [M.lookup (capMatch c) names]
  ]
  where
    names = M.fromList [(capMatch n, n) | n <- caps, capName n == "name"]

spec :: Spec
spec = captureSpec >> cacheSpec >> benchSpec

-- | The line of the struct named @n@'s definition.
structLine :: Text -> Text -> [Int]
structLine n src = [fst (pStart (capNode c)) | c <- rust src, capName c == "name", pText (capNode c) == n]

cacheSpec :: Spec
cacheSpec = describe "tree-sitter: the parse cache" $ do
  it "holds at most 16 texts" $ do
    forM_ [1 .. 40 :: Int] $ \i -> evaluate (length (rust ("struct S" <> T.pack (show i) <> " { a: u8 }\n")))
    n <- cacheSize
    n `shouldSatisfy` (<= 16)
  it "an edited text is reparsed" $ do
    let original = "struct Moved { a: u8 }\n"
        edited = "\n" <> original
    structLine "Moved" original `shouldBe` [0]
    structLine "Moved" edited `shouldBe` [1]
    structLine "Moved" original `shouldBe` [0]
  it "concurrent reads agree with serial ones" $ do
    let texts = ["struct T" <> T.pack (show i) <> " {\n    a: u8,\n}\n" <> T.replicate i "\nfn f() {}\n" | i <- [1 .. 4 :: Int]]
        answer t = [(capName c, pStart (capNode c), pText (capNode c)) | c <- rust t]
        serial = map answer texts
    _ <- evaluate (length (show serial))
    caps <- getNumCapabilities
    caps `shouldSatisfy` (>= 1)
    results <- forConcurrently [1 .. 8 :: Int] $ \_ -> forM [1 .. 50 :: Int] $ \k -> do
      let i = k `mod` 4
          r = answer (texts !! i)
      _ <- evaluate (length (show r))
      pure (r == serial !! i)
    and (concat results) `shouldBe` True

-- | Opt-in timing (OLIFANT_TS_BENCH=<dir> holding big.rs, lock_api's
-- rwlock.rs, and rust-main.toml, main's rust.toml with its declared targets).
benchSpec :: Spec
benchSpec = describe "tree-sitter: timing" $
  it "tree-sitter: timing (OLIFANT_TS_BENCH=dir)" $
    lookupEnv "OLIFANT_TS_BENCH" >>= \case
      Nothing -> pendingWith "set OLIFANT_TS_BENCH"
      Just dir -> do
        tmp <- getTemporaryDirectory
        new <- either (error . T.unpack) id . parseOverShipped "rust" "packs/rust-htmx" <$> TIO.readFile "packs/rust-htmx/languages/rust.toml"
        old <- either (error . T.unpack) id . parseLanguage "rust" "." <$> TIO.readFile (dir </> "rust-main.toml")
        service <- TIO.readFile "packs/rust-htmx/fixtures/app/billing/billing-service/src/service.rs"
        big <- TIO.readFile (dir </> "big.rs")
        let syn = astGrepSyntax (tmp </> "olifant-test-cache")
            ms a b = realToFrac (diffUTCTime b a) * 1000 :: Double
            timeIt act = do
              t0 <- getCurrentTime
              r <- act
              _ <- evaluate (length (show r))
              t1 <- getCurrentTime
              pure (ms t0 t1, r)
            med xs = sort xs !! (length xs `div` 2)
            -- cold: a text the cache has not seen (a trailing comment line
            -- numbered by the run); warm: the same text again
            run :: Language -> Text -> Text -> Bool -> IO (Double, Either () Int)
            run lang file p cold = do
              rs <- forM [1 .. 20 :: Int] $ \i ->
                let f = if cold then file <> "// run " <> T.pack (show i) <> "\n" else file
                 in timeIt (resolveLayers syn (Just lang) Nothing [PhraseAnchor p] f)
              pure (med (map fst rs), maybe (Left ()) (either (const (Left ())) (Right . locLine) . snd) (listToMaybe rs))
        rows <- forM [("service.rs", service, "end of struct BillingService"), ("service.rs", service, "end of impl BillingService"), ("service.rs", service, "end of parameters in new"), ("rwlock.rs (3130 lines)", big, "end of struct RwLock")] $ \(f, file, p) -> do
          (cold, rNew) <- run new file p True
          (warm, _) <- run new file p False
          (tOld, rOld) <- run old file p False
          pure (f <> " | " <> T.unpack p <> " | cold " <> show cold <> " | warm " <> show warm <> " | ast-grep " <> show tOld <> " | " <> show rNew <> " | " <> show rOld)
        mapM_ (putStrLn . ("BENCH " <>)) rows
        capRuns <- forM [1 .. 20 :: Int] $ \i -> timeIt (pure (either (const 0) (length . snd) (capturesOf "rust" (queriesFor "rust") (big <> "// c " <> T.pack (show i) <> "\n"))))
        putStrLn ("BENCH capturesOf rwlock.rs alone, cold: " <> show (med (map fst capRuns)) <> " ms, " <> show (maybe 0 snd (listToMaybe capRuns)) <> " captures")

captureSpec :: Spec
captureSpec = describe "tree-sitter captures" $ do
  it "links the python and rust grammars" $
    linkedGrammars `shouldBe` ["python", "rust"]
  it "every shipped query compiles and uses only known predicates" $
    forM_ linkedGrammars $ \g ->
      either (Just . ((g <> ": ") <>)) (const Nothing) (capturesOf g (queriesFor g) "") `shouldBe` Nothing
  it "a Python file's classes and functions are captured" $
    defs (either (error . T.unpack) snd (capturesOf "python" (queriesFor "python") "class A:\n    def m(self):\n        pass\n\ndef f():\n    pass\n"))
      `shouldBe` [("definition.class", "A"), ("definition.function", "m"), ("definition.function", "f")]
  describe "predicates (evaluated in Haskell)" $ do
    -- @import is a capture the rule reads (others are pruned)
    let idents q = [pText (capNode c) | c <- either (error . T.unpack) snd (capturesOf "rust" [("p.scm", q)] "fn f() { a; b; a; abc; }\n"), capName c == "import"]
    it "#eq? filters" $ idents "((identifier) @import (#eq? @import \"a\"))" `shouldBe` ["a", "a"]
    it "#not-eq? filters" $ idents "((identifier) @import (#not-eq? @import \"a\"))" `shouldBe` ["f", "b", "abc"]
    it "#match? filters (a search, POSIX extended)" $ idents "((identifier) @import (#match? @import \"^a\"))" `shouldBe` ["a", "a", "abc"]
    it "#not-match? filters" $ idents "((identifier) @import (#not-match? @import \"^a\"))" `shouldBe` ["f", "b"]
    it "#any-of? filters" $ idents "((identifier) @import (#any-of? @import \"a\" \"b\"))" `shouldBe` ["a", "b", "a"]
    it "#not-any-of? filters" $ idents "((identifier) @import (#not-any-of? @import \"a\" \"b\"))" `shouldBe` ["f", "abc"]
    it "#eq? compares two captures, even one the rule does not read" $
      idents "((expression_statement (identifier) @other) @import (#eq? @other \"b\"))" `shouldBe` ["b;"]
    it "an unknown predicate is a Left naming the file and the predicate" $
      either (\e -> "p.scm" `T.isInfixOf` e && "lua-match?" `T.isInfixOf` e) (const False)
        (capturesOf "rust" [("p.scm", "((identifier) @import (#lua-match? @import \"^a\"))")] "fn f() {}\n")
        `shouldBe` True
  it "a Rust file's definitions are captured" $ do
    src <- TIO.readFile "packs/rust-htmx/fixtures/app/billing/billing-service/src/service.rs"
    let ds = defs (rust src)
    ds `shouldContain` [("definition.class", "BillingService")]
    ds `shouldContain` [("definition.impl", "BillingService")]
  it "a trait impl is not an impl definition" $ do
    let ds = defs (rust "struct InMemoryRepo;\ntrait Repo {}\nimpl Repo for InMemoryRepo {\n    fn f(&self) {}\n}\n")
    [n | ("definition.impl", n) <- ds] `shouldBe` []
  it "a generic impl is named by its type" $
    [n | ("definition.impl", n) <- defs (rust "struct Store<T>(T);\nimpl<T> Store<T> {\n    fn get(&self) {}\n}\n")] `shouldBe` ["Store"]
  it "columns count characters, after non-ASCII text (line 1, character column 16)" $ do
    let fieldA caps = [pStart (capNode c) | c <- caps, pText (capNode c) == "a: u8"]
    fieldA (rust "// café\nstruct größe { a: u8 }\n") `shouldSatisfy` (\ps -> not (null ps) && all (== (1, 15)) ps)
  it "CRLF endings give the same lines and columns" $ do
    let lf = "// café\nstruct größe { a: u8 }\nfn f() {}\n"
        crlf = T.replace "\n" "\r\n" lf
        at caps = [(capName c, pStart (capNode c)) | c <- caps]
    at (rust crlf) `shouldBe` at (rust lf)
  it "a broken file still captures" $
    [n | (_, n) <- defs (rust "struct A { a: u8 }\nfn f( {\n")] `shouldContain` ["A"]
  it "an unlinked grammar is a Left naming it" $
    either (T.isInfixOf "go") (const False) (capturesOf "go" [] "package main\n") `shouldBe` True
  it "a query that does not compile is a Left naming the file and byte" $ do
    let r = capturesOf "rust" [("bad.scm", "(no_such_node) @x")] "fn f() {}\n"
    r `shouldSatisfy` isLeft
    either (\e -> "bad.scm" `T.isInfixOf` e && "byte" `T.isInfixOf` e) (const False) r `shouldBe` True
