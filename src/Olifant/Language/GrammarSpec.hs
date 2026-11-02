module Olifant.Language.GrammarSpec (spec) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_)
import Data.Either (isRight)
import Data.List (isInfixOf, isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Olifant.Language.Adapter (Grammar (..), GrammarSource (..))
import Olifant.Language.Grammar
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getModificationTime, getTemporaryDirectory, listDirectory, removePathForcibly, setModificationTime)
import System.FilePath ((</>))
import Test.Hspec

-- | The vendored python sources (the specs' local source).
pythonDir :: FilePath
pythonDir = "cbits/tree-sitter-python"

-- | A fresh cache of this spec's own, and settings over it with no local
-- sources and the real PATH.
fresh :: String -> IO GrammarSettings
fresh n = do
  tmp <- getTemporaryDirectory
  let cache = tmp </> "olifant-test-cache" </> ("grammar-spec-" <> n)
  removePathForcibly cache
  createDirectoryIfMissing True cache
  pure (defaultSettings cache [])

python :: Grammar
python = Grammar "python" (Just (SourceDir pythonDir))

libraries :: GrammarSettings -> IO [FilePath]
libraries s = do
  let d = gsCache s </> "grammars"
  there <- doesDirectoryExist d
  if there then filter (not . (== "src")) <$> listDirectory d else pure []

-- | A copy of the python sources in a temp folder.
copyPython :: FilePath -> IO ()
copyPython to = do
  createDirectoryIfMissing True (to </> "tree_sitter")
  forM_ ["parser.c", "scanner.c"] $ \f -> copyFile (pythonDir </> f) (to </> f)
  forM_ ["alloc.h", "array.h", "parser.h"] $ \f -> do
    present <- doesFileExist (pythonDir </> "tree_sitter" </> f)
    if present then copyFile (pythonDir </> "tree_sitter" </> f) (to </> "tree_sitter" </> f) else pure ()

later :: UTCTime -> UTCTime
later = addUTCTime 120

spec :: Spec
spec = describe "loadGrammar" $ do
  it "builds a pack's sources once and loads them" $ do
    s <- fresh "once"
    Right l <- loadGrammarWith s python
    ldAbi l `shouldBe` 15
    ldNodeTypes l `shouldSatisfy` elem "function_definition"
    doesFileExist (ldLibrary l) `shouldReturn` True
    stamp <- getModificationTime (ldLibrary l)
    Right l2 <- loadGrammarWith s python
    ldLibrary l2 `shouldBe` ldLibrary l
    getModificationTime (ldLibrary l) `shouldReturn` stamp
  it "two loads at once build one library" $ do
    s <- fresh "twice"
    rs <- mapConcurrently (const (loadGrammarWith s python)) [1 :: Int, 2]
    all isRight rs `shouldBe` True
    files <- libraries s
    length files `shouldBe` 1
    filter (".tmp-" `isInfixOf`) files `shouldBe` []
  it "a changed parser.c is a new key; the old library stays" $ do
    s <- fresh "changed"
    let src = gsCache s </> "copy"
        g = Grammar "python" (Just (SourceDir src))
    copyPython src
    Right l1 <- loadGrammarWith s g
    t <- getModificationTime (src </> "parser.c")
    setModificationTime (src </> "parser.c") (later t)
    Right l2 <- loadGrammarWith s g
    ldLibrary l2 `shouldNotBe` ldLibrary l1
    doesFileExist (ldLibrary l1) `shouldReturn` True
  it "the key is the name and 8 hex, holding no path" $ do
    s <- fresh "key"
    k <- grammarKey s python pythonDir
    T.unpack k `shouldSatisfy` ("python-" `isPrefixOf`)
    T.length k `shouldBe` T.length "python-" + 8
    T.all (`elem` ("0123456789abcdef" :: String)) (T.drop 7 k) `shouldBe` True
  it "without cc it is NeedsCompiler, and nothing is written" $ do
    s <- fresh "nocc"
    r <- loadGrammarWith s {gsFind = \_ -> pure Nothing} python
    case r of
      Left (NeedsCompiler _) -> pure ()
      other -> expectationFailure (show (fmap ldLibrary other))
    libraries s `shouldReturn` []
  it "a source without parser.c is BuildFailed naming it" $ do
    s <- fresh "noparser"
    let src = gsCache s </> "empty"
    createDirectoryIfMissing True src
    r <- loadGrammarWith s (Grammar "python" (Just (SourceDir src)))
    case r of
      Left (BuildFailed t) -> T.unpack t `shouldSatisfy` ("parser.c" `isInfixOf`)
      other -> expectationFailure (show (fmap ldLibrary other))
  it "an ABI outside the runtime's range is refused before any parse" $ do
    s <- fresh "abi"
    let src = gsCache s </> "x"
    createDirectoryIfMissing True src
    -- a TSLanguage whose first field (abi_version) is 99
    writeFile (src </> "parser.c") "#include <stdint.h>\ntypedef struct { uint32_t abi_version; uint32_t rest[64]; } L;\nstatic const L language = {99, {0}};\nconst void *tree_sitter_x(void) { return &language; }\n"
    loadGrammarWith s (Grammar "x" (Just (SourceDir src))) >>= \case
      Left f -> f `shouldBe` AbiMismatch 99 abiRange
      Right _ -> expectationFailure "loaded"
    abiRange `shouldBe` (13, 15)
  it "a library without the symbol is LoadFailed naming it" $ do
    s <- fresh "nosymbol"
    loadGrammarWith s (Grammar "nope" (Just (SourceDir pythonDir))) >>= \case
      Left (LoadFailed t) -> T.unpack t `shouldSatisfy` ("tree_sitter_nope" `isInfixOf`)
      other -> expectationFailure (show (fmap ldLibrary other))
  it "a bare name is NoSource" $ do
    s <- fresh "bare"
    fmap (either Just (const Nothing)) (loadGrammarWith s (Grammar "go" Nothing)) `shouldReturn` Just NoSource
  it "grammarLabel" $ do
    grammarLabel (Grammar "python" (Just (SourceGit "https://github.com/tree-sitter/tree-sitter-python" "293fdc02038ee2bf0e2e206711b69c90ac0d413f" Nothing))) `shouldBe` "tree-sitter-python @293fdc0"
    grammarLabel (Grammar "elm" (Just (SourceDir "/p/languages/grammar/elm"))) `shouldBe` "elm"
    grammarLabel (Grammar "go" Nothing) `shouldBe` ("go" :: Text)
  it "a failure's text names the grammar" $
    grammarFailureText "go" NoSource `shouldSatisfy` (\t -> "go" `T.isInfixOf` t && "no source" `T.isInfixOf` t)
