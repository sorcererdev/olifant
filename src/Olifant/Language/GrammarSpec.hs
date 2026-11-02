module Olifant.Language.GrammarSpec (spec) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_)
import Data.Either (isRight)
import Data.List (isInfixOf, isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.Aeson (object, (.=))
import Data.Text.IO qualified as TIO
import Olifant.Language.Adapter (Grammar (..), GrammarSource (..), Language (..), parseOverShipped)
import Olifant.Language.AstGrep (astGrepSyntax)
import Olifant.Language.Syntax (Syntax (..))
import Olifant.Language.Grammar
import System.Directory (findExecutable, copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getModificationTime, getTemporaryDirectory, listDirectory, removePathForcibly, setModificationTime)
import System.FilePath (takeBaseName, (</>))
import System.Process (readProcess)
import GHC.Clock (getMonotonicTime)
import Test.Hspec

-- | The vendored python sources (the specs' local source).
pythonDir :: FilePath
pythonDir = "library/grammars/python"

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

-- | A repository in a temp folder: the pack's Elm sources under @sub/src/@,
-- committed, then a second commit touching README. Its first commit's SHA.
fixtureRepo :: FilePath -> FilePath -> IO Text
fixtureRepo repo sub = do
  removePathForcibly repo
  let elm = "packs/elm/languages/grammar/elm"
      to = repo </> sub </> "src"
  createDirectoryIfMissing True (to </> "tree_sitter")
  forM_ ["parser.c", "scanner.c"] $ \f -> copyFile (elm </> f) (to </> f)
  hs <- listDirectory (elm </> "tree_sitter")
  forM_ hs $ \f -> copyFile (elm </> "tree_sitter" </> f) (to </> "tree_sitter" </> f)
  let git args = readProcess "git" (["-C", repo, "-c", "user.name=t", "-c", "user.email=t@t", "-c", "commit.gpgsign=false"] ++ args) ""
  _ <- readProcess "git" ["init", "--quiet", repo] ""
  _ <- git ["add", "-A"]
  _ <- git ["commit", "--quiet", "-m", "grammar"]
  first' <- T.strip . T.pack <$> git ["rev-parse", "HEAD"]
  writeFile (repo </> "README") "later\n"
  _ <- git ["add", "-A"]
  _ <- git ["commit", "--quiet", "-m", "readme"]
  pure first'

spec :: Spec
spec = do
 describe "ast-grep and the loader" $
  it "ast-grep's sgconfig names the loader's library" $ do
    elmSrc <- TIO.readFile "packs/elm/languages/elm.toml"
    let Right elm = parseOverShipped "elm" "packs/elm" elmSrc
    tmp <- getTemporaryDirectory
    let cache = tmp </> "olifant-test-cache"
    findExecutable "ast-grep" >>= \case
      Nothing -> pendingWith "ast-grep is not installed"
      Just _ -> do
        r <- querySyntax (astGrepSyntax cache) elm (object ["kind" .= ("value_declaration" :: Text)]) "main = 1\n"
        fmap length r `shouldBe` Right 1
        Right l <- loadGrammar (langGrammar elm)
        config <- TIO.readFile (cache </> "sg" </> takeBaseName (ldLibrary l) </> "sgconfig.yml")
        config `shouldSatisfy` T.isInfixOf ("libraryPath: " <> T.pack (ldLibrary l) <> "\n")
 describe "git sources" $ do
  it "fetches a pinned revision by file:// and builds it" $ do
    s <- fresh "git"
    let repo = gsCache s </> "repo"
    sha <- fixtureRepo repo "."
    Right l <- loadGrammarWith s (Grammar "elm" (Just (SourceGit ("file://" <> T.pack repo) sha Nothing)))
    ldAbi l `shouldBe` 15
    let checkout = gsCache s </> "grammars" </> "src" </> ("elm-" <> take 7 (T.unpack sha))
    doesFileExist (checkout </> "src" </> "parser.c") `shouldReturn` True
    doesFileExist (checkout </> "README") `shouldReturn` False
    leftovers <- listDirectory (gsCache s </> "grammars" </> "src")
    filter (".tmp-" `isInfixOf`) leftovers `shouldBe` []
  it "path picks the folder inside the checkout" $ do
    s <- fresh "git-path"
    let repo = gsCache s </> "repo"
    sha <- fixtureRepo repo "tsx"
    r <- loadGrammarWith s (Grammar "elm" (Just (SourceGit ("file://" <> T.pack repo) sha (Just "tsx"))))
    fmap ldAbi r `shouldBe` Right 15
  it "a local source folder wins over a fetch" $ do
    s <- fresh "git-local"
    let local = gsCache s </> "local"
    copyPython (local </> "python")
    r <- loadGrammarWith s {gsLocalSources = [local]} (Grammar "python" (Just (SourceGit "file:///nonexistent" "293fdc02038ee2bf0e2e206711b69c90ac0d413f" Nothing)))
    fmap ldAbi r `shouldBe` Right 15
    doesDirectoryExist (gsCache s </> "grammars" </> "src") `shouldReturn` False
  it "a failed fetch is tried once per process" $ do
    s <- fresh "git-fail"
    let g = Grammar "python" (Just (SourceGit "file:///nonexistent" "0123456789012345678901234567890123456789" Nothing))
    loadGrammarWith s g >>= \case
      Left (FetchFailed t) -> T.unpack t `shouldSatisfy` ("fetching file:///nonexistent @0123456 failed: fatal: " `isPrefixOf`)
      other -> expectationFailure (show (fmap ldLibrary other))
    t0 <- getMonotonicTime
    r <- loadGrammarWith s g
    t1 <- getMonotonicTime
    either (\case FetchFailed _ -> True; _ -> False) (const False) r `shouldBe` True
    (t1 - t0) `shouldSatisfy` (< 0.05)
    doesDirectoryExist (gsCache s </> "grammars" </> "src" </> "python-0123456") `shouldReturn` False
  it "a revision the repository does not have is a FetchFailed, and nothing is left" $ do
    s <- fresh "git-norev"
    let repo = gsCache s </> "repo"
    _ <- fixtureRepo repo "."
    r <- loadGrammarWith s (Grammar "elm" (Just (SourceGit ("file://" <> T.pack repo) "0123456789012345678901234567890123456789" Nothing)))
    either (\case FetchFailed _ -> True; _ -> False) (const False) r `shouldBe` True
    leftovers <- listDirectory (gsCache s </> "grammars" </> "src")
    leftovers `shouldBe` []
 describe "loadGrammar" $ do
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
