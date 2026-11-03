-- | The `Syntax` adapter: ast-grep's CLI (`scan --stdin --inline-rules --json`).
-- A grammar ast-grep lacks is registered from the loader's compiled library.
module Olifant.Language.AstGrep (astGrepSyntax, astGrepBuiltIn) where

import Data.Aeson (FromJSON (..), ToJSON, Value, eitherDecodeStrict, encode, withObject, (.:), (.:?))
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Olifant.Provider (runProvider)
import Olifant.Language.Adapter
import Olifant.Process (runBytes)
import Olifant.Language.Syntax
import Olifant.Language.TreeSitter (treeSitterAnchors)
import Olifant.Language.Grammar (grammarFailureText, grammarLibrary, stopping)
import Control.Concurrent (myThreadId)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, renameFile)
import System.Posix.Process (getProcessID)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, (</>))

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
          let yaml = "id: olifant\nlanguage: " <> grName (langGrammar lang) <> "\nrule: " <> asJson rule <> "\n" <> maybe "" (\f -> "fix: " <> asJson f <> "\n") fixed
              asJson :: ToJSON a => a -> Text
              asJson = TE.decodeUtf8 . BL.toStrict . encode
          ran <- runBytes sg ["scan", "--stdin", "--inline-rules", T.unpack yaml, "--json=compact"] (Just dir) (TE.encodeUtf8 source)
          pure $ case ran of
            Left why -> Left (SyntaxFailed ("ast-grep failed: " <> why))
            Right (ExitSuccess, out, _) -> either (Left . SyntaxFailed . T.pack) Right (eitherDecodeStrict out)
            Right (ExitFailure _, _, err) -> Left (SyntaxFailed ("ast-grep failed: " <> T.strip (TE.decodeUtf8With lenientDecode err)))

-- | ast-grep 0.45.3's built-in languages: a grammar of one of these names
-- is ast-grep's own; any other with a source is registered as a custom
-- language from the loader's library. A fact about the tool, not a language.
astGrepBuiltIn :: [Text]
astGrepBuiltIn = ["bash", "c", "cpp", "csharp", "css", "elixir", "go", "haskell", "html", "java", "javascript", "json", "kotlin", "lua", "nix", "php", "python", "ruby", "rust", "scala", "solidity", "swift", "typescript", "tsx", "yaml"]

-- | A directory whose sgconfig.yml registers the language (a grammar
-- ast-grep lacks, with a source): the library is the loader's, built once
-- for both ('grammarLibrary').
prepare :: FilePath -> Language -> IO (Either SyntaxFailure FilePath)
prepare cache lang = case langGrammar lang of
  g@(Grammar name (Just _))
    | name `notElem` astGrepBuiltIn ->
        grammarLibrary g >>= \case
          -- no fallback: a grammar that cannot load stops the command
          Left f -> stopping name f >> pure (Left (SyntaxFailed (grammarFailureText name f)))
          Right lib -> do
            let dir = cache </> "sg" </> takeBaseName lib
                config = dir </> "sgconfig.yml"
            present <- doesFileExist config
            if present
              then pure ()
              else do
                createDirectoryIfMissing True dir
                -- written aside, then renamed: a query at the same moment reads
                -- the whole config or none
                mine <- unique config
                writeFile mine $
                  "ruleDirs: []\ncustomLanguages:\n  " <> T.unpack name <> ":\n    libraryPath: " <> lib <> "\n    extensions: [" <> T.unpack name <> "]\n"
                renameFile mine config
            pure (Right dir)
  _ -> createDirectoryIfMissing True cache >> pure (Right cache)
  where
    -- a name of this process and thread beside @path@
    unique path = do
      pid <- getProcessID
      thread <- myThreadId
      pure (path <> ".tmp-" <> show pid <> "-" <> filter (/= ' ') (show thread))
