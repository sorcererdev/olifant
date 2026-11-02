module Olifant.Language.TreeSitterSpec (spec) where

import Control.Concurrent.Async (forConcurrently)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_)
import Data.Either (isLeft)
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Time (diffUTCTime, getCurrentTime)
import GHC.Conc (getNumCapabilities)
import Olifant.Anchor (Miss (..), RenderedAnchor (..), locLine, resolveLayers)
import Olifant.Language (Language (..), parseLanguage)
import Olifant.Language.Adapter (parseOverShipped)
import Olifant.Language.AstGrep (astGrepSyntax)
import System.Directory (getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Language.Captures (Cap (..), PNode (..), anchorFromCaptures)
import Olifant.Language.Syntax (Match (..))
import Olifant.Language.TreeSitter (cacheHolds, cacheSize, compiledSize, capturesOf, definitionKinds, loadableGrammar, queriesFor)
import Olifant.Language.Adapter (Grammar (..))
import Olifant.Language (shippedLanguageNow)
import Test.Hspec

-- | A shipped language's grammar (its git source, read from the vendored copy).
shipped :: Text -> Grammar
shipped g = maybe (error ("no loadable grammar: " <> T.unpack g)) id (loadableGrammar (shippedLanguageNow g))

rust :: Text -> [Cap]
rust src = either (error . T.unpack) snd (capturesOf (shipped "rust") (queriesFor "rust") src)

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
  it "holds at most the 16 newest texts: the oldest go out first" $ do
    let text i = "struct S" <> T.pack (show i) <> " { a: u8 }\n"
    forM_ [1 .. 40 :: Int] $ \i -> evaluate (length (rust (text i)))
    n <- cacheSize
    n `shouldSatisfy` (<= 16)
    old <- mapM (cacheHolds . text) [1 .. 24]
    newest <- cacheHolds (text 40)
    (or old, newest) `shouldBe` (False, True)
    -- an evicted text is asked again: reparsed, right, and cached
    evaluate (length (rust (text 1)))
    structLine "S1" (text 1) `shouldBe` [0]
    cacheHolds (text 1) `shouldReturn` True
  it "holds at most 16 compiled query sets: each saved project .scm is a new one (m2b M2)" $ do
    forM_ [1 .. 40 :: Int] $ \i ->
      evaluate (length (either (const []) snd (capturesOf (shipped "rust") [("x.scm", "; " <> T.pack (show i) <> "\n(struct_item name: (type_identifier) @name) @definition.class")] "struct A;\n")))
    n <- compiledSize
    n `shouldBe` 16
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
        capRuns <- forM [1 .. 20 :: Int] $ \i -> timeIt (pure (either (const 0) (length . snd) (capturesOf (shipped "rust") (queriesFor "rust") (big <> "// c " <> T.pack (show i) <> "\n"))))
        putStrLn ("BENCH capturesOf rwlock.rs alone, cold: " <> show (med (map fst capRuns)) <> " ms, " <> show (maybe 0 snd (listToMaybe capRuns)) <> " captures")

captureSpec :: Spec
captureSpec = describe "tree-sitter captures" $ do
  it "loads the haskell, python and rust grammars from the vendored copies" $
    forM_ ["haskell", "python", "rust"] $ \g ->
      either Just (const Nothing) (capturesOf (shipped g) (queriesFor g) "") `shouldBe` Nothing
  it "a project imports.scm replaces the shipped one" $ do
    rs <- either (error . T.unpack) id . parseOverShipped "rust" "packs/rust-htmx" <$> TIO.readFile "packs/rust-htmx/languages/rust.toml"
    let own = rs {langQueries = [("imports.scm", "(extern_crate_declaration) @import\n", "project")]}
        src = "extern crate a;\nuse b::c;\n\nfn f() {}\n"
        syn = astGrepSyntax "/nonexistent"
        line l = fmap (fmap locLine) (resolveLayers syn (Just l) Nothing [PhraseAnchor "end of imports"] src)
    line rs `shouldReturn` Right 2
    line own `shouldReturn` Right 1
  describe "Haskell (olifant's own queries)" $ do
    let hs src = either (error . T.unpack) id (capturesOf (shipped "haskell") (queriesFor "haskell") src)
        containers src ws = case hs src of
          (root, caps) -> fmap (fmap (map (\(c, items) -> ((fst (matchStart c), fst (matchEnd c)), length items)))) (anchorFromCaptures True root caps ws)
    it "a Haskell function's definition spans its signature and equations" $
      containers "f :: Int -> Int\nf 0 = 1\nf n = n\n\ng = 2\n" ["function", "f"] `shouldBe` Just (Right [((0, 2), 1)])
    it "a function without a signature is its equations" $
      containers "f :: Int -> Int\nf 0 = 1\n\ng 0 = 1\ng n = n\n" ["function", "g"] `shouldBe` Just (Right [((3, 4), 1)])
    it "`data` is a class-like definition: its constructors are its items" $ do
      let src = "data E\n  = A\n  | B Int\n  deriving (Show)\n"
      [n | ("definition.type", n) <- defs (snd (hs src))] `shouldBe` ["E"]
      containers src ["class", "E"] `shouldBe` Just (Right [((1, 2), 2)])
      containers src ["data", "E"] `shouldBe` Just (Right [((1, 2), 2)])
    it "an operator's name is exact beside another operator (review I2)" $
      containers "(<+>) :: A\na <+> b = b\n\ninstance S X where\n  a <> b = a\n" ["function", "(<+>)"] `shouldBe` Just (Right [((0, 1), 1)])
    it "a class's and an instance's declarations are their items" $ do
      let src = "class C a where\n  m :: a -> Int\n  m _ = 1\n\ninstance C Int where\n  m x = x\n"
      containers src ["instance", "C"] `shouldBe` Just (Right [((5, 5), 1)])
      -- review I3: `class C` is the class, not its instances
      -- (its signature and default equation are one item: one `m`)
      containers src ["class", "C"] `shouldBe` Just (Right [((1, 2), 1)])
  describe "Elm (the elm pack's grammar; tags and textobjects vendored, imports and types olifant's)" $ do
    elm <- runIO (either (error . T.unpack) id . parseOverShipped "elm" "packs/elm" <$> TIO.readFile "packs/elm/languages/elm.toml")
    let sample = T.unlines ["module A exposing (..)", "", "import Dict", "", "type alias Model =", "    { count : Int", "    , name : String", "    }", "", "type alias Id = String", "", "type Msg", "    = Increment", "    | Named { who : String }", "", "update : Msg -> Model -> Model", "update msg model =", "    let", "        helper x = x + 1", "    in", "    model"]
    it "elm's queries compile against the pack's grammar" $
      fmap (const ()) (loadableGrammar elm >>= \g -> either (const Nothing) Just (capturesOf g (queriesFor "elm") "")) `shouldBe` Just ()
    it "elm definitions: functions, types, aliases, a record (its unit the whole alias); not the module" $
      fmap (map (\(n, k, own, unit) -> (n, k, own, unit))) (definitionKinds elm sample)
        `shouldBe` Just
          [ ("Model", "definition.type", (6, 8), (5, 8))
          , ("Id", "definition.type", (10, 10), (10, 10))
          , ("Msg", "definition.type", (12, 14), (12, 14))
          , ("update", "definition.function", (17, 21), (17, 21))
          , ("helper", "definition.function", (19, 19), (19, 19))
          ]
    it "elm's type, record and imports phrases answer from the queries where its targets did" $ do
      tmp <- getTemporaryDirectory
      let syn = astGrepSyntax (tmp </> "olifant-test-cache")
          at f phrase = TIO.readFile ("packs/elm/fixtures/app/src" </> f) >>= fmap (fmap locLine) . resolveLayers syn (Just elm) Nothing [PhraseAnchor phrase]
      got <- mapM (uncurry at) [("Main.elm", "end of type Msg"), ("Main.elm", "start of type Msg"), ("Main.elm", "end of type Page"), ("Main.elm", "end of record Model"), ("Main.elm", "start of record Model"), ("Main.elm", "end of imports"), ("Route.elm", "end of type Route"), ("Route.elm", "end of imports")]
      let sharesLine = "the first item shares its line with other code; Olifant inserts whole lines (use a multi-line layout, or another anchor)"
      map (either (Left . map missReason) Right) got `shouldBe` [Right 41, Left [sharesLine], Right 25, Right 18, Left [sharesLine], Right 9, Right 9, Right 4]
  it "every shipped query compiles and uses only known predicates" $
    forM_ ["haskell", "python", "rust"] $ \g ->
      either (Just . ((g <> ": ") <>)) (const Nothing) (capturesOf (shipped g) (queriesFor g) "") `shouldBe` Nothing
  it "a Python file's classes and functions are captured" $
    defs (either (error . T.unpack) snd (capturesOf (shipped "python") (queriesFor "python") "class A:\n    def m(self):\n        pass\n\ndef f():\n    pass\n"))
      `shouldBe` [("definition.class", "A"), ("definition.function", "m"), ("definition.function", "f")]
  describe "predicates (evaluated in Haskell)" $ do
    -- @import is a capture the rule reads (others are pruned)
    let idents q = [pText (capNode c) | c <- either (error . T.unpack) snd (capturesOf (shipped "rust") [("p.scm", q)] "fn f() { a; b; a; abc; }\n"), capName c == "import"]
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
        (capturesOf (shipped "rust") [("p.scm", "((identifier) @import (#lua-match? @import \"^a\"))")] "fn f() {}\n")
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
  it "a grammar with no source is a Left naming it" $
    either (\e -> "go" `T.isInfixOf` e && "no source" `T.isInfixOf` e) (const False) (capturesOf (Grammar "go" Nothing) [] "package main\n") `shouldBe` True
  it "a query that does not compile is a Left naming the file and byte" $ do
    let r = capturesOf (shipped "rust") [("bad.scm", "(no_such_node) @x")] "fn f() {}\n"
    r `shouldSatisfy` isLeft
    either (\e -> "bad.scm" `T.isInfixOf` e && "byte" `T.isInfixOf` e) (const False) r `shouldBe` True
