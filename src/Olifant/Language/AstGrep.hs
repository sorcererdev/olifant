-- | The `Syntax` adapter: ast-grep's CLI (`scan --stdin --inline-rules --json`).
-- Custom grammars (pack sources) are compiled once with `cc` into the cache.
module Olifant.Language.AstGrep (astGrepSyntax) where

import Data.Aeson (FromJSON (..), eitherDecodeStrict, encode, withObject, (.:))
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word64)
import Numeric (showHex)
import Olifant.Language.Adapter
import Olifant.Language.Process (runBytes)
import Olifant.Language.Syntax
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, makeAbsolute)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Info (os)

newtype Found = Found Match

instance FromJSON Found where
  parseJSON = withObject "match" $ \o -> do
    r <- o .: "range"
    s <- r .: "start"
    e <- r .: "end"
    t <- o .: "text"
    pos <- (,) <$> ((,) <$> s .: "line" <*> s .: "column") <*> ((,) <$> e .: "line" <*> e .: "column")
    pure (Found (Match (fst pos) (snd pos) t))

astGrepSyntax :: FilePath -> Syntax IO
astGrepSyntax cache = Syntax $ \lang rule source ->
  findExecutable "ast-grep" >>= \case
    Nothing -> pure (Left (SyntaxToolMissing "ast-grep is not installed (brew install ast-grep)"))
    Just sg ->
      prepare cache lang >>= \case
        Left f -> pure (Left f)
        Right dir -> do
          let yaml = "id: olifant\nlanguage: " <> langName lang <> "\nrule: " <> TE.decodeUtf8 (BL.toStrict (encode rule)) <> "\n"
          ran <- runBytes sg ["scan", "--stdin", "--inline-rules", T.unpack yaml, "--json=compact"] (Just dir) (TE.encodeUtf8 source)
          pure $ case ran of
            Left why -> Left (SyntaxFailed ("ast-grep failed: " <> why))
            Right (ExitSuccess, out, _) -> either (Left . SyntaxFailed . T.pack) (Right . map (\(Found m) -> m)) (eitherDecodeStrict out)
            Right (ExitFailure _, _, err) -> Left (SyntaxFailed ("ast-grep failed: " <> T.strip (TE.decodeUtf8With lenientDecode err)))

-- | A directory whose sgconfig.yml registers the language (custom grammars only).
prepare :: FilePath -> Language -> IO (Either SyntaxFailure FilePath)
prepare cache lang = case langGrammar lang of
  BuiltInGrammar _ -> createDirectoryIfMissing True cache >> pure (Right cache)
  GrammarSource src0 -> do
    src <- makeAbsolute src0
    parser <- BS.readFile (src </> "parser.c")
    let key = T.unpack (langName lang) <> "-" <> showHex (fnv parser) ""
        lib = cache </> "grammars" </> key <> (if os == "darwin" then ".dylib" else ".so")
        dir = cache </> "sg" </> key
    built <- doesFileExist lib
    compiled <- if built then pure (Right ()) else compile src lib
    case compiled of
      Left f -> pure (Left f)
      Right () -> do
        createDirectoryIfMissing True dir
        writeFile (dir </> "sgconfig.yml") $
          "ruleDirs: []\ncustomLanguages:\n  " <> T.unpack (langName lang) <> ":\n    libraryPath: " <> lib <> "\n    extensions: [" <> T.unpack (langName lang) <> "]\n"
        pure (Right dir)
  where
    compile src lib =
      findExecutable "cc" >>= \case
        Nothing -> pure (Left (SyntaxToolMissing ("a C compiler (cc) is needed to build the " <> langName lang <> " grammar (xcode-select --install)")))
        Just cc -> do
          createDirectoryIfMissing True (cache </> "grammars")
          hasScanner <- doesFileExist (src </> "scanner.c")
          let sources = (src </> "parser.c") : [src </> "scanner.c" | hasScanner]
          ran <- runBytes cc (["-shared", "-fPIC", "-O2", "-I", src] ++ sources ++ ["-o", lib]) Nothing ""
          pure $ case ran of
            Right (ExitSuccess, _, _) -> Right ()
            Right (ExitFailure _, _, err) -> Left (SyntaxFailed ("building the " <> langName lang <> " grammar failed: " <> T.strip (TE.decodeUtf8With lenientDecode err)))
            Left why -> Left (SyntaxFailed ("building the " <> langName lang <> " grammar failed: " <> why))
    fnv = BS.foldl' (\h b -> (h `xor` fromIntegral b) * 1099511628211) (14695981039346656037 :: Word64)
