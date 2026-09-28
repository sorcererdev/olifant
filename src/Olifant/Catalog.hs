-- | Finding generators (project-local and in packs) and resolving their
-- template and exemplar sources into a runnable 'Generator'.
module Olifant.Catalog
  ( Catalog
  , catalogEntries
  , CatalogEntry (..)
  , CatalogError (..)
  , describeCatalogError
  , openProjectCatalog
  , openPackCatalog
  , findEntry
  , loadGenerator
  ) where

import Data.Bifunctor (bimap, first)
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generator
import Olifant.Template (describeExemplarError, describeTemplateError, exemplar, parseTemplate)
import Olifant.Toml qualified as Toml
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (splitDirectories, takeDirectory, takeFileName, (</>))

data CatalogEntry = CatalogEntry
  { entryId :: GeneratorId
  , entryPack :: Maybe Text
  , entryDir :: FilePath
  , entryPackRoot :: Maybe FilePath
  , entryDefinition :: GeneratorDef
  }
  deriving stock (Show)

data Catalog = Catalog {catalogProject :: Maybe FilePath, catalogEntries :: [CatalogEntry]}

data CatalogError
  = ConfigInvalid Text
  | GeneratorInvalid FilePath Text
  | GeneratorNotFound Text [GeneratorId]
  | GeneratorAmbiguous Text [GeneratorId]
  | SourceMissing GeneratorId FilePath
  | SourceInvalid GeneratorId FilePath Text
  deriving stock (Eq, Show)

describeCatalogError :: CatalogError -> Text
describeCatalogError = \case
  ConfigInvalid e -> ".olifant/config.toml: " <> e
  GeneratorInvalid p e -> T.pack p <> ": " <> e
  GeneratorNotFound r _ -> "no generator named `" <> r <> "`"
  GeneratorAmbiguous r ids -> "`" <> r <> "` matches several generators: " <> T.intercalate ", " (map generatorIdText ids)
  SourceMissing g p -> generatorIdText g <> ": missing source file " <> T.pack p
  SourceInvalid g p e -> generatorIdText g <> ": " <> T.pack p <> ": " <> e

openProjectCatalog :: Monad m => Workspace m -> FilePath -> m (Either CatalogError Catalog)
openProjectCatalog root project = do
  config <- readText root (project </> ".olifant" </> "config.toml")
  case maybe (Right []) packsFromConfig config of
    Left err -> pure (Left (ConfigInvalid err))
    Right packs -> do
      let packRoots = [(name, normalisePath (project </> p)) | (name, p) <- packs]
      result <- openCatalogAt root (Just (project </> ".olifant" </> "generators")) packRoots
      pure (fmap (\c -> c {catalogProject = Just project}) result)

openPackCatalog :: Monad m => Workspace m -> [(Text, FilePath)] -> m (Either CatalogError Catalog)
openPackCatalog root = openCatalogAt root Nothing

packsFromConfig :: Text -> Either Text [(Text, FilePath)]
packsFromConfig src = do
  top <- Toml.parseToml src
  packs <- Toml.optional "packs" Toml.table top
  traverse
    (\(name, v) -> Toml.inKey "packs" (Toml.inKey name (Toml.table v >>= \t -> Toml.onlyKeys ["path"] t >> (name,) . T.unpack <$> Toml.required "path" Toml.text t)))
    (maybe [] M.toList packs)

openCatalogAt :: Monad m => Workspace m -> Maybe FilePath -> [(Text, FilePath)] -> m (Either CatalogError Catalog)
openCatalogAt root localDir packs = do
  local <- maybe (pure []) (discover Nothing Nothing) localDir
  fromPacks <- concat <$> traverse (\(name, r) -> discover (Just name) (Just r) (r </> "generators")) packs
  pure (Catalog Nothing . sortOn entryId <$> sequence (local ++ fromPacks))
  where
    discover pack packRoot dir = do
      files <- listFiles root dir
      let gens = [takeDirectory f | f <- files, takeFileName f == "generator.toml", length (splitDirectories f) == 2]
      traverse (load pack packRoot dir) gens
    load pack packRoot dir g = do
      let gdir = dir </> g
          gname = T.pack g
          gid = GeneratorId (maybe gname (\p -> p <> "/" <> gname) pack)
          path = gdir </> "generator.toml"
      src <- readText root path
      pure $ case src of
        Nothing -> Left (GeneratorInvalid path "unreadable")
        Just s -> bimap (GeneratorInvalid path) (CatalogEntry gid pack gdir packRoot) (parseGenerator gid s)

findEntry :: Catalog -> Text -> Either CatalogError CatalogEntry
findEntry cat ref =
  case [e | e <- entries, entryId e == GeneratorId ref] of
    (e : _) -> Right e
    [] -> case [e | e <- entries, isJust (entryPack e), T.pack (takeFileName (entryDir e)) == ref] of
      [e] -> Right e
      [] -> Left (GeneratorNotFound ref (map entryId entries))
      several -> Left (GeneratorAmbiguous ref (map entryId several))
  where
    entries = catalogEntries cat

loadGenerator :: Monad m => Workspace m -> Catalog -> CatalogEntry -> m (Either CatalogError Generator)
loadGenerator root cat entry = sequenceA <$> traverse resolve (entryDefinition entry)
  where
    gid = entryId entry
    resolve = \case
      InlineText t -> pure (Right t)
      TemplateFile f -> fromFile (entryDir entry </> "templates" </> f) (first describeTemplateError . parseTemplate)
      ExemplarFile ref binds -> case exemplarPath ref of
        Left err -> pure (Left err)
        Right path -> fromFile path (first describeExemplarError . exemplar binds)
    fromFile path build =
      readText root path <&> \case
        Nothing -> Left (SourceMissing gid path)
        Just src -> first (SourceInvalid gid path) (build src)
    exemplarPath ref = case T.stripPrefix "fixture:" ref of
      Just rest -> case entryPackRoot entry of
        Just r -> Right (r </> "fixtures" </> "app" </> T.unpack rest)
        Nothing -> Left (SourceInvalid gid (T.unpack ref) "`fixture:` exemplars are only available in packs")
      Nothing -> Right (fromMaybe "." (catalogProject cat) </> T.unpack ref)
