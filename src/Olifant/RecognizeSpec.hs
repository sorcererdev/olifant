module Olifant.RecognizeSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Language (Language (..), parseLanguage)
import Olifant.Recognize
import Control.Monad (forM)
import Olifant.Generator (EditWith (..), GeneratorId (..), GeneratorWith (..), parseGenerator)
import Olifant.Naming (parseName)
import Olifant.Template (Value (..), parseTemplate, render)
import Olifant.Template.Syntax (Template (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, takeExtension, (</>))
import Olifant.Catalog (Catalog (..), CatalogEntry (..), loadGenerator, openPackCatalog)
import Olifant.Generator (Generator, generatorName)
import Olifant.Language (noSyntax)
import Olifant.Workspace (diskWorkspace)
import Test.Hspec
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
      fmap langKeywords (parseLanguage "x" "/p" "files = [\"*.x\"]\ncomment = \"#\"\ngrammar = \"python\"\nkeywords = [\"def\", \"class\"]\n")
        `shouldBe` Right ["def", "class"]
      fmap langKeywords (parseLanguage "x" "/p" "files = [\"*.x\"]\ncomment = \"#\"\ngrammar = \"python\"\n") `shouldBe` Right []
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
        corpus = [S.fromList (generic <> [("u" <> T.pack (show i), "v", "w")]) | i <- [1 .. 10 :: Int]] <> [S.fromList [a, b]]
        w' = (weightsOf corpus) {wBoilerplate = S.empty}
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
    let [command] = [c | g <- gens, genId g == GeneratorId "python-htmx/command", Create {editSource = c} <- genEdits g, "def handle" `T.isInfixOf` templateSource c]
        sample = sampleText command
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
    wDefault (ixWeights ix) `shouldBe` log (1 + fromIntegral (length files))

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
