-- | The `Syntax` adapter: ast-grep's CLI (`scan --stdin --inline-rules --json`).
-- Custom grammars (pack sources) are compiled once with `cc` into the cache.
module Olifant.Language.AstGrep (astGrepSyntax) where

import Data.Aeson (FromJSON (..), ToJSON, Value, eitherDecodeStrict, encode, withObject, (.:), (.:?))
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word64)
import Numeric (showHex)
import Olifant.Provider (runProvider)
import Olifant.Language.Adapter
import Olifant.Process (runBytes)
import Olifant.Language.Syntax
import Olifant.Language.TreeSitter (treeSitterAnchors)
import Control.Concurrent (myThreadId)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, getFileSize, getModificationTime, makeAbsolute, renameFile)
import System.Posix.Process (getProcessID)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Info (os)

-- | One ast-grep match: the node and, when the rule had a fix, its replacement.
data Found = Found Match (Maybe Text)

instance FromJSON Found where
  parseJSON = withObject "match" $ \o -> do
    r <- o .: "range"
    s <- r .: "start"
    e <- r .: "end"
    bytes <- r .: "byteOffset"
    t <- o .: "text"
    replacement <- o .:? "replacement"
    pos <- (,) <$> ((,) <$> s .: "line" <*> s .: "column") <*> ((,) <$> e .: "line" <*> e .: "column")
    range <- (,) <$> bytes .: "start" <*> bytes .: "end"
    pure (Found (Match (fst pos) (snd pos) t range) replacement)

astGrepSyntax :: FilePath -> Syntax IO
astGrepSyntax cache =
  Syntax
    { querySyntax = \lang rule source -> fmap (map (\(Found m _) -> m)) <$> scan cache lang rule Nothing source,
      rewriteSyntax = \lang rule fixed source ->
        fmap (map (\(Found (Match s e t b) r) -> Rewrite s e t (fromMaybe t r) b)) <$> scan cache lang rule (Just fixed) source,
      runSlotProvider = runProvider,
      -- the generic anchor phrases: tree-sitter in process, not ast-grep
      anchorSyntax = treeSitterAnchors
    }

-- | `sg scan` one inline rule (with its `fix`, if any) over a text.
scan :: FilePath -> Language -> Value -> Maybe Text -> Text -> IO (Either SyntaxFailure [Found])
scan cache lang rule fixed source =
  findExecutable "ast-grep" >>= \case
    Nothing -> pure (Left (SyntaxToolMissing "ast-grep is not installed (brew install ast-grep)"))
    Just sg ->
      prepare cache lang >>= \case
        Left f -> pure (Left f)
        Right dir -> do
          let yaml = "id: olifant\nlanguage: " <> langName lang <> "\nrule: " <> asJson rule <> "\n" <> maybe "" (\f -> "fix: " <> asJson f <> "\n") fixed
              asJson :: ToJSON a => a -> Text
              asJson = TE.decodeUtf8 . BL.toStrict . encode
          ran <- runBytes sg ["scan", "--stdin", "--inline-rules", T.unpack yaml, "--json=compact"] (Just dir) (TE.encodeUtf8 source)
          pure $ case ran of
            Left why -> Left (SyntaxFailed ("ast-grep failed: " <> why))
            Right (ExitSuccess, out, _) -> either (Left . SyntaxFailed . T.pack) Right (eitherDecodeStrict out)
            Right (ExitFailure _, _, err) -> Left (SyntaxFailed ("ast-grep failed: " <> T.strip (TE.decodeUtf8With lenientDecode err)))

-- | A directory whose sgconfig.yml registers the language (custom grammars only).
prepare :: FilePath -> Language -> IO (Either SyntaxFailure FilePath)
prepare cache lang = case langGrammar lang of
  Grammar _ (Just (SourceDir src0)) -> do
    src <- makeAbsolute src0
    -- Keyed on parser.c's size and modification time: a stat per query
    -- rather than hashing a grammar that can be megabytes (every AST query
    -- runs this once or twice per file).
    size <- getFileSize (src </> "parser.c")
    stamp <- getModificationTime (src </> "parser.c")
    let key = T.unpack (langName lang) <> "-" <> showHex (fnv (BS.pack (map (fromIntegral . fromEnum) (show (size, stamp) <> src)))) ""
        lib = cache </> "grammars" </> key <> (if os == "darwin" then ".dylib" else ".so")
        dir = cache </> "sg" </> key
    built <- doesFileExist lib
    compiled <- if built then pure (Right ()) else compile src lib
    case compiled of
      Left f -> pure (Left f)
      Right () -> do
        let config = dir </> "sgconfig.yml"
        present <- doesFileExist config
        if present
          then pure ()
          else do
            createDirectoryIfMissing True dir
            -- written aside, then renamed: a query at the same moment reads
            -- the whole config or none
            mine <- unique config
            writeFile mine $
              "ruleDirs: []\ncustomLanguages:\n  " <> T.unpack (langName lang) <> ":\n    libraryPath: " <> lib <> "\n    extensions: [" <> T.unpack (langName lang) <> "]\n"
            renameFile mine config
        pure (Right dir)
  -- ast-grep's built-in language (a git source is loaded in process only, for now)
  _ -> createDirectoryIfMissing True cache >> pure (Right cache)
  where
    compile src lib =
      findExecutable "cc" >>= \case
        Nothing -> pure (Left (SyntaxToolMissing ("a C compiler (cc) is needed to build the " <> langName lang <> " grammar (xcode-select --install)")))
        Just cc -> do
          createDirectoryIfMissing True (cache </> "grammars")
          hasScanner <- doesFileExist (src </> "scanner.c")
          let sources = (src </> "parser.c") : [src </> "scanner.c" | hasScanner]
          -- built aside, then renamed: queries (parallel tests, the server's
          -- threads, other olifant processes) never load a half-written
          -- library, and builds at the same moment do not collide
          mine <- unique lib
          ran <- runBytes cc (["-shared", "-fPIC", "-O2", "-I", src] ++ sources ++ ["-o", mine]) Nothing ""
          case ran of
            Right (ExitSuccess, _, _) -> renameFile mine lib
            _ -> pure ()
          pure $ case ran of
            Right (ExitSuccess, _, _) -> Right ()
            Right (ExitFailure _, _, err) -> Left (SyntaxFailed ("building the " <> langName lang <> " grammar failed: " <> T.strip (TE.decodeUtf8With lenientDecode err)))
            Left why -> Left (SyntaxFailed ("building the " <> langName lang <> " grammar failed: " <> why))
    -- a name of this process and thread beside @path@
    unique path = do
      pid <- getProcessID
      thread <- myThreadId
      pure (path <> ".tmp-" <> show pid <> "-" <> filter (/= ' ') (show thread))
    fnv = BS.foldl' (\h b -> (h `xor` fromIntegral b) * 1099511628211) (14695981039346656037 :: Word64)
