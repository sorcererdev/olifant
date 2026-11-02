module Olifant.RecognizeSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Language (Language (..), parseLanguage, parseOverShipped)
import Olifant.Recognize
import Control.Monad (forM, forM_, (>=>))
import Olifant.Generator (EditWith (..), GeneratorId (..), GeneratorWith (..), generatorIdText, parseGenerator)
import Olifant.Naming (parseName)
import Olifant.Template (Value (..), parseTemplate, render)
import Olifant.Template.Syntax (Template (..))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (makeRelative, takeExtension, (</>))
import Olifant.Catalog (Catalog (..), loadGenerator, openPackCatalog)
import Olifant.Generator (Generator)
import Olifant.Language (noSyntax)
import Olifant.Workspace (diskWorkspace)
import Olifant.Toml qualified as Toml
import Data.List (isInfixOf, sort)
import Text.Printf (printf)
import Olifant.Decide (Question (..), Shape (..), curlDecider, decisionPrompt, recordedDecider)
import System.IO.Temp (withSystemTempDirectory)
import Olifant.Model (Backend (..))
import Olifant.Process (neverCancel)
import Test.Hspec
import Control.Exception (evaluate)
import Data.Time (diffUTCTime, getCurrentTime)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))
import Data.Text (Text)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

spec :: Spec
spec = describe "Olifant.Recognize" $ do
  weightsSpec
  pathsSpec
  indexSpec
  inputsSpec
  recognizeSpec
  askSpec
  sequential . it "decides in under 50 ms with 5,000 more files in the project (median of 20 changes)" $ do
    (gens, fixture) <- pythonProject
    let bulk = [("src/bistro/bulk/m" <> show (i `div` 100) <> "/mod_" <> show i <> ".py", "x_" <> T.pack (show i) <> " = " <> T.pack (show i) <> "\n") | i <- [1 .. 5000 :: Int]]
        ix = buildIndex (pythonKeywords gens) gens (fixture ++ bulk)
        ownWay v = "class CloseTable:\n    def __init__(self, repo):\n        self.repo = repo\n\n    def run(self, table_id):\n        self.repo.save(table_id)\n# " <> T.pack (show v) <> "\n"
        decide v = recognizeIn ix gens (Change "src/bistro/booking/commands/close_table.py" (ownWay v))
    t0 <- getCurrentTime
    _ <- evaluate (length (show (decide (0 :: Int))))
    t1 <- getCurrentTime
    times <- forM [1 .. 20 :: Int] $ \v -> do
      a <- getCurrentTime
      r <- evaluate (decide v)
      _ <- evaluate (length (show r))
      b <- getCurrentTime
      pure (realToFrac (diffUTCTime b a) * 1000 :: Double)
    let median = sort times !! 10
    printf "      deciding at 5,000 more files: the index %d ms once, then median %.1f ms, max %.1f ms\n" (round (realToFrac (diffUTCTime t1 t0) * 1000 :: Double) :: Int) median (maximum times)
    fmap rcName (decide (0 :: Int)) `shouldBe` Just "Command"
    median `shouldSatisfy` (< 50)
  describe "shapes" $ do
    let kw = Keywords (S.fromList ["class", "def", "self", "return"]) "#"
    it "(a) keeps keywords and punctuation, and names become their case shape" $
      shapes kw "class CloseTable:\n    def run(self, table_id):"
        `shouldBe` ["class", "Pascal2", ":", "NL", "def", "lower1", "(", "self", ",", "snake2", ")", ":"]
    it "(b) names, case and string contents do not change shapes" $
      shapes kw "def a(x): return \"hi\"" `shouldBe` shapes kw "def b(y): return 'yo'"
    it "(c) a triple-quoted docstring is one str; a comment vanishes" $
      shapes kw "\"\"\"A docstring\nover lines.\"\"\"\nx = 1  # note\n" `shouldBe` ["str", "NL", "lower1", "=", "num", "NL"]
    it "(c2) only the language's comments: a Rust attribute stays" $
      shapes (Keywords (S.fromList ["derive"]) "//") "#[derive(Debug)] // why\nstruct X;"
        `shouldBe` ["#", "[", "derive", "(", "Pascal1", ")", "]", "NL", "lower1", "Pascal1", ";"]
    it "(d) the keywords of a pack's templates are their literal words, not their variables" $ do
      src <- TIO.readFile "packs/python-htmx/generators/command/templates/command.py.dt"
      t <- either (fail . show) pure (parseTemplate src)
      let Keywords ws _ = keywordsFrom "#" [t] []
      mapM_ (\w -> (w, w `S.member` ws) `shouldBe` (w, True)) ["dataclass", "frozen", "handle", "NotImplementedError"]
      S.member "Command" ws `shouldBe` False
    it "(e) a language's keywords come from its adapter (`keywords`), none when absent" $ do
      fmap langKeywords (parseLanguage "x" "/p" "files = [\"*.x\"]\ncomment = \"#\"\ngrammar = { git = \"https://github.com/tree-sitter/tree-sitter-python\", rev = \"293fdc02038ee2bf0e2e206711b69c90ac0d413f\" }\nkeywords = [\"def\", \"class\"]\n")
        `shouldBe` Right ["def", "class"]
      fmap langKeywords (parseLanguage "x" "/p" "files = [\"*.x\"]\ncomment = \"#\"\ngrammar = { git = \"https://github.com/tree-sitter/tree-sitter-python\", rev = \"293fdc02038ee2bf0e2e206711b69c90ac0d413f\" }\n") `shouldBe` Right []
    it "makes overlapping windows of three, and knows which carry a name" $ do
      shingles ["a", "b", "c", "d"] `shouldBe` S.fromList [("a", "b", "c"), ("b", "c", "d")]
      nameShingle ("def", "snake2", "(") `shouldBe` True
      nameShingle ("def", "(", ")") `shouldBe` False

-- | Rarity weights and weighted containment (Task 2).
weightsSpec :: Spec
weightsSpec = describe "weights" $ do
  let a = ("a", "b", "c")
      b = ("b", "c", "d")
      c = ("x", "y", "z")
      everywhere = ("self", ".", "lower1")
      files = [S.fromList [everywhere, a], S.fromList [everywhere], S.fromList [everywhere], S.fromList [everywhere], S.fromList [everywhere, c]]
      w = weightsOf files
  it "(a) weighs a pattern in every file less than one in a single file" $
    weightOf w everywhere < weightOf w a `shouldBe` True
  it "(b) removes boilerplate (in more than 30% of the files) before containment" $ do
    S.member everywhere (wBoilerplate w) `shouldBe` True
    containment w (S.fromList [everywhere, a]) (S.fromList [a]) `shouldBe` 1
  it "(c) is 1 for itself, 0 for nothing shared, and always between" $ do
    containment w (S.fromList [a, b]) (S.fromList [a, b]) `shouldBe` 1
    containment w (S.fromList [a, b]) (S.fromList [c]) `shouldBe` 0
  it "(c) property: containment stays between 0 and 1" $
    hedgehog $ do
      xs <- forAll (Gen.set (Range.linear 0 8) (Gen.element [a, b, c, everywhere, ("p", "q", "r")]))
      ys <- forAll (Gen.set (Range.linear 0 8) (Gen.element [a, b, c, everywhere, ("p", "q", "r")]))
      let v = containment w xs ys
      H.assert (v >= 0 && v <= 1)
  it "(d) a change of generic patterns scores lower weighted than unweighted against a large reference (the spike's finding)" $ do
    let generic = [("g" <> T.pack (show i), "h", "k") | i <- [1 .. 6 :: Int]]
        files' = [S.fromList (generic <> [("u" <> T.pack (show i), "v", "w")]) | i <- [1 .. 10 :: Int]] <> [S.fromList [a, b]]
        w' = (weightsOf files') {wBoilerplate = S.empty}
        change = S.fromList (take 3 generic <> [a, c])
        big = S.fromList (generic <> [("q", "q", "q")])
        flat = fromIntegral (S.size (S.intersection change big)) / fromIntegral (S.size change) :: Double
    containment w' change big < flat `shouldBe` True
  it "has no boilerplate in a project of fewer than 4 files" $
    wBoilerplate (weightsOf (take 3 files)) `shouldBe` S.empty
  where
    weightOf w s = M.findWithDefault (wDefault w) s (wIdf w)

-- | Output paths as globs, and inverted to inputs (Task 3).
pathsSpec :: Spec
pathsSpec = describe "paths" $ do
  let tmpl t = either (error . show) id (parseTemplate t)
  it "(a) the command's output path: its glob, and the inputs a path gives back" $ do
    g <- packGenerator "python-htmx" "command"
    pat <- maybe (fail "no pattern") pure (pathPattern (genDerived g) (tmpl "{= dir =}/{= command_ =}.py"))
    ppGlob pat `shouldBe` "src/*/*/commands/*.py"
    invertPath pat "src/bistro/booking/commands/close_table.py"
      `shouldBe` Just [("app", "bistro"), ("service", "booking"), ("command", "close_table")]
  it "(b) a name rendered in Pascal case reads back in snake case" $ do
    pat <- maybe (fail "no pattern") pure (pathPattern [("Model", tmpl "{= model|pascal =}")] (tmpl "models/{= Model =}.py"))
    invertPath pat "models/WaiterShift.py" `shouldBe` Just [("model", "waiter_shift")]
  gens <- runIO (concat <$> forM ["python-htmx", "rust-htmx"] packGenerators)
  let cases = [(g, t, pat) | g <- gens, Create {editTarget = t} <- genEdits g, Just pat <- [pathPattern (genDerived g) t]]
  it "(c) inverts most of the packs' output paths (they are plain)" $
    length cases `shouldSatisfy` (> 10)
  it "(c) property: every output path of python-htmx and rust-htmx inverts to the inputs that rendered it" $
    hedgehog $ do
      (g, t, pat) <- forAll (Gen.element cases)
      let vars = [v | PVar v _ <- ppParts pat]
      values <- forM vars $ \v -> (v,) <$> forAll (snakeName)
      let inputs = M.fromList values
          env0 = M.map (\v -> VName (either (error . show) id (parseName v))) inputs
          env = foldl (\e (d, dt) -> either (const e) (\r -> M.insert d (VText r) e) (render e dt)) env0 (genDerived g)
      case render env t of
        Left _ -> H.discard
        Right path -> fmap M.fromList (invertPath pat (T.unpack path)) === Just inputs
  it "(d) a path that does not match, or a repeated input that disagrees, gives nothing" $ do
    g <- packGenerator "python-htmx" "command"
    pat <- maybe (fail "no pattern") pure (pathPattern (genDerived g) (tmpl "{= dir =}/{= command_ =}.py"))
    invertPath pat "src/bistro/booking/queries/x.py" `shouldBe` Nothing
    twice <- maybe (fail "no pattern") pure (pathPattern [] (tmpl "{= a =}/{= a =}.py"))
    invertPath twice "x/y.py" `shouldBe` Nothing
    invertPath twice "x/x.py" `shouldBe` Just [("a", "x")]
  it "refuses what it cannot invert: a condition, a loop, a filter that is not a case" $ do
    pathPattern [] (tmpl "{= if a =}x{= end =}.py") `shouldBe` Nothing
    pathPattern [] (tmpl "{= a|plural =}.py") `shouldBe` Nothing
  where
    snakeName = do
      ws <- Gen.list (Range.linear 1 3) (Gen.text (Range.linear 2 6) Gen.lower)
      pure (T.intercalate "_" ws)

-- | A pack's generator definitions (their paths and derived values).
packGenerators :: Text -> IO [GeneratorWith ()]
packGenerators pack = do
  names <- listDirectory ("packs/" <> T.unpack pack <> "/generators")
  fmap concat . forM names $ \n -> do
    src <- TIO.readFile ("packs/" <> T.unpack pack <> "/generators/" <> n <> "/generator.toml")
    pure (either (const []) (\g -> [() <$ g]) (parseGenerator (GeneratorId (pack <> "/" <> T.pack n)) src))

packGenerator :: Text -> Text -> IO (GeneratorWith ())
packGenerator pack name = do
  src <- TIO.readFile ("packs/" <> T.unpack pack <> "/generators/" <> T.unpack name <> "/generator.toml")
  either (fail . T.unpack) (pure . (() <$)) (parseGenerator (GeneratorId (pack <> "/" <> name)) src)

-- | The project's recognition index (Task 4).
indexSpec :: Spec
indexSpec = describe "the index" $ do
  it "(a) reads `recognize = false`; true by default; a starter generator is never recognisable" $ do
    let gen extra = parseGenerator (GeneratorId "p/g") (T.unlines (["description = \"d\"", "language = \"x\""] ++ extra))
    fmap genRecognize (gen ["recognize = false"]) `shouldBe` Right False
    fmap genRecognize (gen []) `shouldBe` Right True
    (gens, files) <- pythonProject
    let ix = buildIndex (pythonKeywords gens) gens files
    [cvGenerator c | c <- ixConventions ix, cvGenerator c == GeneratorId "python-htmx/project"] `shouldBe` []
  it "(b) renders a template for its shape: sample names, holes, the first branch" $ do
    (gens, _) <- pythonProject
    command <- case [c | g <- gens, genId g == GeneratorId "python-htmx/command", Create {editSource = c} <- genEdits g, "def handle" `T.isInfixOf` templateSource c] of
      c : _ -> pure c
      [] -> fail "no command template"
    let sample = sampleText command
    sample `shouldSatisfy` T.isInfixOf "class SampleThing:"
    sample `shouldSatisfy` T.isInfixOf "def handle(self, input: SampleThingInput)"
  it "(c) finds the project's own instances of each convention (its siblings)" $ do
    (gens, files) <- pythonProject
    let ix = buildIndex (pythonKeywords gens) gens files
        siblings gid = [p | c <- ixConventions ix, cvGenerator c == GeneratorId gid, (p, _) <- cvReferences c, p /= "template"]
    siblings "python-htmx/command" `shouldSatisfy` elem "src/bistro/booking/commands/create_booking.py"
    siblings "python-htmx/command" `shouldSatisfy` elem "src/bistro/booking/commands/create_booking__test.py"
    M.lookup (GeneratorId "python-htmx/command") (ixSiblingDirs ix) `shouldBe` Just (S.fromList ["src/bistro/booking/commands"])
  it "(d) weighs patterns over the files it is given (one language's)" $ do
    (gens, files) <- pythonProject
    let ix = buildIndex (pythonKeywords gens) gens files
    wDefault (ixWeights ix) `shouldBe` log (2 + fromIntegral (length files))

-- | A pack's generators of a language (loaded), its fixture app's files of
-- that language, and the language's keywords (its adapter's and its
-- templates').
packProject :: Text -> String -> Text -> IO ([Generator], [(FilePath, Text)], Keywords)
packProject pack ext lang = do
  cat <- either (fail . show) pure =<< openPackCatalog (diskWorkspace ".") [(pack, "packs/" <> T.unpack pack)]
  gens <- fmap concat . forM (catalogEntries cat) $ \e -> either (const []) pure <$> loadGenerator (diskWorkspace ".") noSyntax cat e
  let root = "packs/" <> T.unpack pack <> "/fixtures/app"
  paths <- filter (\p -> takeExtension p == ext && not ("/target/" `isInfixOf` p)) <$> walk root
  files <- forM paths $ \p -> (makeRelative root p,) <$> TIO.readFile p
  adapter <- TIO.readFile ("packs/" <> T.unpack pack <> "/languages/" <> T.unpack lang <> ".toml")
  language <- either (fail . T.unpack) pure (parseOverShipped lang ("packs/" <> T.unpack pack) adapter)
  let mine = [g | g <- gens, genLanguage g == lang]
  pure (mine, files, keywordsFrom (langComment language) [t | g <- mine, Create {editSource = t} <- genEdits g] (langKeywords language))
  where
    walk d = do
      names <- listDirectory d
      fmap concat . forM names $ \n -> do
        isDir <- doesDirectoryExist (d </> n)
        if isDir then walk (d </> n) else pure [d </> n]

-- | python-htmx's generators (loaded) and its fixture app's .py files.
pythonProject :: IO ([Generator], [(FilePath, Text)])
pythonProject = do
  cat <- either (fail . show) pure =<< openPackCatalog (diskWorkspace ".") [("python-htmx", "packs/python-htmx")]
  gens <- fmap concat . forM (catalogEntries cat) $ \e -> either (const []) pure <$> loadGenerator (diskWorkspace ".") noSyntax cat e
  let root = "packs/python-htmx/fixtures/app"
  paths <- filter ((== ".py") . takeExtension) <$> walk root
  files <- forM paths $ \p -> (makeRelative root p,) <$> TIO.readFile p
  pure ([g | g <- gens, genLanguage g == "python"], files)
  where
    walk d = do
      names <- listDirectory d
      fmap concat . forM names $ \n -> do
        isDir <- doesDirectoryExist (d </> n)
        if isDir then walk (d </> n) else pure [d </> n]

pythonKeywords :: [Generator] -> Keywords
pythonKeywords gens = keywordsFrom "#" [t | g <- gens, Create {editSource = t} <- genEdits g] ["class", "def", "self", "return", "import", "from"]

-- | Reading inputs: the path, aligned class names, siblings; validated (Task 5).
inputsSpec :: Spec
inputsSpec = describe "inputs" $ do
  let setup = do
        (gens, files) <- pythonProject
        let ix = buildIndex (pythonKeywords gens) gens files
        case ([g | g <- gens, genId g == GeneratorId "python-htmx/command"], [c | c <- ixConventions ix, cvGenerator c == GeneratorId "python-htmx/command", ppGlob (cvPattern c) == "src/*/*/commands/*.py"]) of
          (g : _, c : _) -> pure (g, c)
          _ -> fail "no python-htmx/command convention"
  it "(a) off its place: the class name, its template's suffix stripped (CloseTableInput is not the command)" $ do
    (g, c) <- setup
    lookup "command" (inputsFor g c "src/bistro/booking/close_table.py" "class CloseTableInput:\n    table_id: str\n\nclass CloseTable:\n    def run(self):\n        pass\n")
      `shouldBe` Just "close_table"
  it "(b) at its place, the path wins over the class name" $ do
    (g, c) <- setup
    lookup "command" (inputsFor g c "src/bistro/booking/commands/seat_party.py" "class Something:\n    pass\n") `shouldBe` Just "seat_party"
  it "(c) the folder's siblings give the inputs they share (app, service)" $ do
    (g, c) <- setup
    let got = inputsFor g c "src/bistro/booking/close_table.py" "class CloseTable:\n    def run(self):\n        pass\n"
    (lookup "app" got, lookup "service" got) `shouldBe` (Just "bistro", Just "booking")
  it "(d) keeps only declared inputs whose values fit their type" $ do
    (g, _) <- setup
    validInputs g [("command", "close_table"), ("nonsense", "x"), ("command2", "y")] `shouldBe` [("command", "close_table")]
    validInputs g [("command", "Not A Name!")] `shouldBe` []

-- | Scores, place-aware bands, the choice; the packs' corpora (Task 6).
recognizeSpec :: Spec
recognizeSpec = describe "recognition" $ do
  (gens, files) <- runIO pythonProject
  let ix = buildIndex (pythonKeywords gens) gens files
      recognise path text = recognizeIn ix gens (Change path text)
  it "(c) needs three lines, and more than boilerplate" $ do
    recognise "src/bistro/booking/commands/x.py" "class X:\n    pass\n" `shouldBe` Nothing
    recognise "src/bistro/booking/commands/x.py" "from dataclasses import dataclass\n\nfrom bistro.shared.result import Ok\n" `shouldBe` Nothing
  it "(d) says why: the place, and what it is shaped like" $ do
    r <- maybe (fail "not recognised") pure $ recognise "src/bistro/booking/commands/close_table.py" "class CloseTable:\n    def __init__(self, repo):\n        self.repo = repo\n\n    def run(self, table_id):\n        self.repo.close(table_id)\n"
    rcReasons r `shouldSatisfy` any ("beside" `T.isInfixOf`)
    rcReasons r `shouldSatisfy` any ("shaped like" `T.isInfixOf`)
  it "(e) works in a project with no siblings yet: the template and the place alone (Review Focus 2)" $ do
    let bare = buildIndex (pythonKeywords gens) gens []
        r = recognizeIn bare gens (Change "src/bistro/booking/commands/close_table.py" "from dataclasses import dataclass\n\n@dataclass(frozen=True)\nclass CloseTableInput:\n    table_id: str\n\nclass CloseTable:\n    def handle(self, input: CloseTableInput):\n        raise NotImplementedError\n")
    fmap (\x -> (rcGenerator x, rcBand x)) r `shouldBe` Just (GeneratorId "python-htmx/command", Offer)
  forM_ [("python-htmx", ".py", "python"), ("rust-htmx", ".rs", "rust")] $ \(pack, ext, lang) -> do
    (pgens, pfiles, kw) <- runIO (packProject pack ext lang)
    cases <- runIO (corpus pack)
    let pix = buildIndex kw pgens pfiles
        precognise path text = recognizeIn pix pgens (Change path text)
        short = T.drop (T.length pack + 1) . generatorIdText
    describe ("the " <> T.unpack pack <> " corpus") $ do
      forM_ cases $ \c -> it (caseName c) $ do
        let r = precognise (casePath c) (caseText c)
            band = maybe "nothing" (bandText . rcBand) r
            generator = fmap (short . rcGenerator) r
        (caseName c, band) `shouldBe` (caseName c, caseBand c)
        case r of
          Just x -> do
            generator `shouldSatisfy` maybe False (\gen -> maybe (gen `elem` T.splitOn "|" (caseExpect c)) (== gen) (caseAs c))
            fmap short (rcOr x) `shouldBe` caseOr c
            [(k, v) | (k, v) <- caseInputs c, lookup k (rcInputs x) /= Just v] `shouldBe` []
          Nothing -> pure ()
      it "(b) never offers less precisely, nor finds less, than it does today" $ do
        let judged = [(c, precognise (casePath c) (caseText c)) | c <- cases]
            right c x = any (`elem` T.splitOn "|" (caseExpect c)) (map short (rcGenerator x : maybe [] pure (rcOr x)))
            offers = [(c, x) | (c, Just x) <- judged, rcBand x == Offer]
            precision = ratio (length [() | (c, x) <- offers, right c x]) (length offers)
            positives = [(c, r) | (c, r) <- judged, caseExpect c /= "nothing"]
            recall = ratio (length [() | (c, Just x) <- positives, right c x]) (length positives)
            (pFloor, rFloor) = floors pack
        printf "      %s corpus: %d cases, offer precision %.2f, recall %.2f\n" (T.unpack pack) (length cases) precision recall
        (precision >= pFloor, recall >= rFloor) `shouldBe` (True, True)
  where
    -- each corpus's floors: what it scores today (a change that lowers either fails)
    floors :: Text -> (Double, Double)
    floors = \case
      "python-htmx" -> (0.9, 0.88)
      "rust-htmx" -> (1, 1)
      _ -> (0, 0)
    ratio :: Int -> Int -> Double
    ratio a b = if b == 0 then 1 else fromIntegral a / fromIntegral b
    bandText = \case
      Offer -> "offer"
      Hint -> "hint"

data Case = Case {caseName :: String, casePath :: FilePath, caseExpect :: Text, caseBand :: Text, caseAs :: Maybe Text, caseOr :: Maybe Text, caseInputs :: [(Text, Text)], caseText :: Text}

-- | A pack's recognition corpus (tests/recognize/*.toml).
corpus :: Text -> IO [Case]
corpus pack = do
  let dir = "packs/" <> T.unpack pack <> "/tests/recognize"
  names <- sort . filter ((== ".toml") . takeExtension) <$> listDirectory dir
  forM names $ \n -> do
    src <- TIO.readFile (dir </> n)
    either (\e -> fail (n <> ": " <> T.unpack e)) pure $ do
      t <- Toml.parseToml src
      Case (takeWhile (/= '.') n)
        <$> (T.unpack <$> Toml.required "path" Toml.text t)
        <*> Toml.required "expect" Toml.text t
        <*> Toml.required "band" Toml.text t
        <*> Toml.optional "as" Toml.text t
        <*> Toml.optional "or" Toml.text t
        <*> (maybe [] M.toList <$> Toml.optional "inputs" (Toml.table >=> traverse Toml.text) t)
        <*> Toml.required "text" Toml.text t

-- | The unsure band asks the decider's `recognize` Choice (a recorded
-- answer stands in for the model).
askSpec :: Spec
askSpec = describe "asking a model (optional)" $ do
  (gens, files) <- runIO pythonProject
  let ix = buildIndex (pythonKeywords gens) gens files
      recognise path text = recognizeIn ix gens (Change path text)
      ownWay = "from dataclasses import dataclass\n\n@dataclass\nclass CloseTableInput:\n    table_id: str\n\nclass CloseTable:\n    def __init__(self, bookings):\n        self.bookings = bookings\n\n    def handle(self, input):\n        return self.bookings.close(input.table_id)\n"
      offPlace = "src/bistro/booking/close_table.py"
      -- consult under a recorded answer; the questions it asked, by kind
      answering answer act = withSystemTempDirectory "olifant-ask" $ \dir -> do
        let file = dir </> "answers.json"
        TIO.writeFile file answer
        result <- act (recordedDecider file neverCancel)
        asked <- doesFileExist (file <> ".asked") >>= \case
          True -> lines <$> readFile (file <> ".asked")
          False -> pure []
        pure (result, asked)
      recognised g fields = "{\"answers\": {\"recognize\": {\"choice\": \"" <> g <> "\", \"fields\": {" <> T.intercalate ", " ["\"" <> k <> "\": \"" <> v <> "\"" | (k, v) <- fields] <> "}}}}"
  it "(a) is asked only when unsure: a hint, a choice, a rival — never a clear offer" $ do
    let Just clear = recognise "src/bistro/booking/commands/close_table.py" ownWay
        Just hint = recognise offPlace ownWay
    (rcBand clear, rcOr clear, rcBand hint) `shouldBe` (Offer, Nothing, Hint)
    (clear', asked) <- answering "{}" $ \d -> consult d gens (Change "src/bistro/booking/commands/close_table.py" ownWay) clear {rcRival = Nothing}
    asked `shouldBe` []
    clear' `shouldBe` Just clear {rcRival = Nothing}
    (_, asked') <- answering "{}" $ \d -> consult d gens (Change offPlace ownWay) hint
    asked' `shouldBe` ["recognize"]
  it "(b) a yes (the convention chosen) makes a hint an offer, with the inputs it read; unsure leaves it as it was" $ do
    let Just hint = recognise offPlace ownWay
    (Just upgraded, _) <- answering (recognised (generatorIdText (rcGenerator hint)) []) $ \d -> consult d gens (Change offPlace ownWay) hint
    (rcBand upgraded, lookup "service" (rcInputs upgraded)) `shouldBe` (Offer, Just "booking")
    (kept, _) <- answering "{\"answers\": {\"recognize\": \"unsure\"}}" $ \d -> consult d gens (Change offPlace ownWay) hint
    kept `shouldBe` Just hint
  it "(c) an answer with inputs outside their types, or not the JSON asked for, is unsure (Review Focus 4)" $ do
    let Just hint = recognise offPlace ownWay
    (Just r, _) <- answering (recognised "python-htmx/command" [("command", "Not A Name!"), ("nonsense", "x")]) $ \d -> consult d gens (Change offPlace ownWay) hint
    (lookup "nonsense" (rcInputs r), lookup "command" (rcInputs r)) `shouldBe` (Nothing, Just "close_table")
    (malformed, _) <- answering "no json here" $ \d -> consult d gens (Change offPlace ownWay) hint
    malformed `shouldBe` Just hint
  it "(c) a backend that does not answer is unsure, not an error (Review Focus 4)" $ do
    let Just hint = recognise offPlace ownWay
    consult (curlDecider neverCancel 1 (OpenAICompatible "http://127.0.0.1:9" "nothing")) gens (Change offPlace ownWay) hint `shouldReturn` Just hint
  it "(d) asks `recognize` over the candidates' ids and summaries, the inputs it lacks as fields, from the path and the code, each named in the instructions" $ do
    let Just hint = recognise offPlace ownWay
        Just q = recognizeQuestion gens (Change offPlace ownWay) hint {rcInputs = filter ((/= "command") . fst) (rcInputs hint)}
    qKind q `shouldBe` "recognize"
    (case qShape q of Choice os -> os; _ -> []) `shouldBe` [("python-htmx/command", "A command of a service, with its test")]
    qFields q `shouldBe` [("command", "ident")]
    map fst (qContext q) `shouldBe` ["path", "code"]
    lookup "code" (qContext q) `shouldSatisfy` maybe False ("class CloseTable" `T.isInfixOf`)
    qInstructions q `shouldBe` "Which of these generators is the code in code, at path, a hand-written instance of? Give the inputs asked for as identifiers from code."
    let prompt = decisionPrompt q
    mapM_ (\needle -> (needle, needle `T.isInfixOf` prompt) `shouldBe` (needle, True)) ["python-htmx/command", "command (ident)", T.pack offPlace, "class CloseTable"]
