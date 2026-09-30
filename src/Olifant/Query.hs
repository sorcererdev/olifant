-- | Queries (spec §4.5): facts read out of the repo as a list of records,
-- for owned regions, `choices_from` and generators. Globs are evaluated over
-- a list of paths (pure); commands go through the 'Commands' port.
module Olifant.Query
  ( Query (..)
  , Record
  , queryFromTree
  , describeQuery
  , globRecords
  , Commands (..)
  , noCommands
  , processCommands
  , runQuery
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Olifant.Glob (globPrefix, matchesGlob)
import Olifant.Language.Process (runBytes)
import Olifant.Naming (parseName)
import Olifant.Template (Value (..))
import Olifant.Toml
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, makeRelative, splitDirectories, takeBaseName, takeDirectory)

data Query = GlobQuery Text [Text] | CommandQuery [Text] (Maybe Text) (Maybe Text)
  deriving stock (Eq, Show)

type Record = Map Text Value

queryFromTree :: Tree -> Either Text Query
queryFromTree tree = do
  t <- table tree
  case (M.member "glob" t, M.member "command" t) of
    (True, False) -> do
      onlyKeys ["glob", "exclude"] t
      GlobQuery <$> required "glob" text t <*> (maybe [] id <$> optional "exclude" (list text) t)
    (False, True) -> do
      onlyKeys ["command", "select", "sort"] t
      argv <- required "command" (list text) t
      if null argv then Left "`command` needs at least the program" else pure ()
      CommandQuery argv <$> optional "select" text t <*> optional "sort" text t
    _ -> Left "a query needs exactly one of `glob` or `command`"

describeQuery :: Query -> Text
describeQuery = \case
  GlobQuery g _ -> "glob " <> g
  CommandQuery argv _ _ -> "command " <> T.unwords argv

ignoredDirs :: [FilePath]
ignoredDirs = ["elm-stuff", "node_modules", ".git", "dist-newstyle", "target", ".olifant"]

globRecords :: Text -> [Text] -> [FilePath] -> [Record]
globRecords g excludes paths =
  [ record p
  | p <- sortOn id paths
  , not (any (`elem` ignoredDirs) (splitDirectories p))
  , matchesGlob g p
  , not (any (`matchesGlob` p) excludes)
  ]
  where
    prefix = globPrefix g
    record p =
      let rel = dropExtension (if null prefix then p else makeRelative prefix p)
          stem = T.pack (takeBaseName p)
       in M.fromList
            [ ("path", VText (T.pack p))
            , ("dir", VText (T.pack (let d = takeDirectory p in if d == "." then "" else d)))
            , ("stem", VText stem)
            , ("name", either (const (VText stem)) VName (parseName stem))
            , ("rel", VText (T.pack rel))
            , ("dotted", VText (T.intercalate "." (map T.pack (splitDirectories rel))))
            ]

newtype Commands m = Commands {runCommand :: [Text] -> m (Either Text ByteString)}

noCommands :: Applicative m => Commands m
noCommands = Commands (\argv -> pure (Left ("commands are not available here (" <> T.unwords argv <> ")")))

processCommands :: FilePath -> Commands IO
processCommands dir = Commands $ \case
  [] -> pure (Left "empty command")
  (exe : args) ->
    findExecutable (T.unpack exe) >>= \case
      Nothing -> pure (Left ("`" <> exe <> "` is not installed"))
      Just path ->
        runBytes path (map T.unpack args) (Just dir) "" >>= \case
          Left why -> pure (Left why)
          Right (ExitSuccess, out, _) -> pure (Right out)
          Right (ExitFailure c, _, err) -> pure (Left ("exited " <> T.pack (show c) <> ": " <> T.strip (TE.decodeUtf8With lenientDecode err)))

runQuery :: Monad m => Commands m -> [FilePath] -> Query -> m (Either Text [Record])
runQuery commands paths q = case q of
  GlobQuery g excludes -> pure (Right (globRecords g excludes paths))
  CommandQuery argv select sortKey ->
    runCommand commands argv >>= \case
      Left why -> pure (Left (describeQuery q <> " failed: " <> why))
      Right out -> pure $ case Aeson.eitherDecodeStrict out of
        Left err -> Left (describeQuery q <> " did not print JSON: " <> T.pack err)
        Right v -> do
          arr <- case (select, v) of
            (Nothing, Aeson.Array xs) -> Right (toList xs)
            (Just k, Aeson.Object o) | Just (Aeson.Array xs) <- KM.lookup (Key.fromText k) o -> Right (toList xs)
            _ -> Left (describeQuery q <> ": expected a JSON array of objects" <> maybe "" (\k -> " under `" <> k <> "`") select)
          let records = mapMaybe toRecord arr
          Right (maybe records (\k -> sortOn (fmap render . M.lookup k) records) sortKey)
  where
    toRecord = \case
      Aeson.Object o -> Just (M.fromList [(Key.toText k, scalar x) | (k, x) <- KM.toList o])
      _ -> Nothing
    scalar = \case
      Aeson.String s -> VText s
      Aeson.Bool b -> VBool b
      Aeson.Number n -> either (VText . T.pack . show) VInt (floatingOrInteger n :: Either Double Integer)
      _ -> VNull
    render = \case
      VText t -> t
      VInt i -> T.pack (show i)
      other -> T.pack (show other)
