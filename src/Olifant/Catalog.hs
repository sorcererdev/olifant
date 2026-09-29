-- | Finding generators (project-local and in packs) and resolving their
-- template and exemplar sources into a runnable 'Generator'.
module Olifant.Catalog
  ( Catalog
  , catalogEntries
  , catalogBroken
  , CatalogEntry (..)
  , CatalogError (..)
  , describeCatalogError
  , CatalogPaths (..)
  , openProjectCatalog
  , openPackCatalog
  , findEntry
  , loadGenerator
  , resolver
  , resolveEntry
  , packOfEntry
  ) where

import Data.Bifunctor (bimap, first)
import Data.Functor ((<&>))
import Data.Either (lefts, rights)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generation.Compose (ResolveFailure (..))
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

-- | Valid generators, plus broken ones (id, generator.toml path, error) kept
-- aside so one bad definition never breaks unrelated commands.
data Catalog = Catalog
  { catalogProject :: Maybe FilePath
  , catalogEntries :: [CatalogEntry]
  , catalogBroken :: [(GeneratorId, FilePath, Text)]
  , catalogPacks :: [Text]
  }

-- | Where configuration lives. Packs come from the user config
-- (@<userConfigDir>/config.toml@, paths relative to that dir) and the project
-- config (@<project>/.olifant/config.toml@, paths relative to the project);
-- the project wins when both name the same pack. @~/@ expands to the home dir.
data CatalogPaths = CatalogPaths
  { catalogProjectDir :: FilePath
  , catalogUserConfigDir :: Maybe FilePath
  , catalogHomeDir :: Maybe FilePath
  }

data CatalogError
  = ConfigInvalid FilePath Text
  | PackNotConfigured Text Text -- ^ pack, requested generator
  | GeneratorInvalid FilePath Text
  | GeneratorNotFound Text [GeneratorId]
  | GeneratorAmbiguous Text [GeneratorId]
  | SourceMissing GeneratorId FilePath
  | SourceInvalid GeneratorId FilePath Text
  deriving stock (Eq, Show)

describeCatalogError :: CatalogError -> Text
describeCatalogError = \case
  ConfigInvalid p e -> T.pack p <> ": " <> e
  PackNotConfigured pack r ->
    "no generator named `" <> r <> "`: no pack named `" <> pack
      <> "` is configured (add it under [packs] in .olifant/config.toml for this project, or ~/.config/olifant/config.toml for every project)"
  GeneratorInvalid p e -> T.pack p <> ": " <> e
  GeneratorNotFound r _ -> "no generator named `" <> r <> "`"
  GeneratorAmbiguous r ids -> "`" <> r <> "` matches several generators: " <> T.intercalate ", " (map generatorIdText ids)
  SourceMissing g p -> generatorIdText g <> ": missing source file " <> T.pack p
  SourceInvalid g p e -> generatorIdText g <> ": " <> T.pack p <> ": " <> e

openProjectCatalog :: Monad m => Workspace m -> CatalogPaths -> m (Either CatalogError Catalog)
openProjectCatalog root paths = do
  let project = catalogProjectDir paths
      userConfig = fmap (</> "config.toml") (catalogUserConfigDir paths)
      projectConfig = project </> ".olifant" </> "config.toml"
  userPacks <- maybe (pure (Right [])) (packsIn (fromMaybe "." (catalogUserConfigDir paths))) userConfig
  projectPacks <- packsIn project projectConfig
  case (,) <$> userPacks <*> projectPacks of
    Left err -> pure (Left err)
    Right (fromUser, fromProject) -> do
      let packRoots = M.toList (M.fromList (fromUser ++ fromProject))
      result <- openCatalogAt root (Just (project </> ".olifant" </> "generators")) packRoots
      pure (fmap (\c -> c {catalogProject = Just project}) result)
  where
    packsIn base configPath =
      readText root configPath <&> \case
        Nothing -> Right []
        Just src -> case packsFromConfig src of
          Left err -> Left (ConfigInvalid configPath err)
          Right packs -> Right [(name, normalisePath (base </> expandHome p)) | (name, p) <- packs]
    expandHome p = case (catalogHomeDir paths, p) of
      (Just home, '~' : '/' : rest) -> home </> rest
      (Just home, "~") -> home
      _ -> p

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
  let loaded = local ++ fromPacks
  pure (Right (Catalog Nothing (sortOn entryId (rights loaded)) (sortOn (\(gid, _, _) -> gid) (lefts loaded)) (map fst packs)))
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
        Nothing -> Left (gid, path, "unreadable")
        Just s -> bimap (gid,path,) (CatalogEntry gid pack gdir packRoot) (parseGenerator gid s)

findEntry :: Catalog -> Text -> Either CatalogError CatalogEntry
findEntry cat ref =
  case [e | e <- entries, entryId e == GeneratorId ref] of
    (e : _) -> Right e
    [] -> case [e | e <- entries, isJust (entryPack e), T.pack (takeFileName (entryDir e)) == ref] of
      [e] -> Right e
      [] -> case [(p, err) | (gid, p, err) <- catalogBroken cat, gid == GeneratorId ref || bareName gid == ref] of
        ((p, err) : _) -> Left (GeneratorInvalid p err)
        []
          | Just pack <- packPrefix, pack `notElem` catalogPacks cat -> Left (PackNotConfigured pack ref)
          | otherwise -> Left (GeneratorNotFound ref (map entryId entries))
      several -> Left (GeneratorAmbiguous ref (map entryId several))
  where
    entries = catalogEntries cat
    bareName (GeneratorId t) = snd (T.breakOnEnd "/" t)
    packPrefix = case T.breakOn "/" ref of
      (pack, rest) | not (T.null pack), not (T.null rest) -> Just pack
      _ -> Nothing

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

-- | Which catalog entry a [[uses]] ref names. Inside a pack, a bare name
-- means that pack's generator first (spec §7); otherwise the usual lookup.
-- A broken candidate is an error, never a reason to fall through to another.
resolveEntry :: Catalog -> Maybe Text -> Text -> Either CatalogError CatalogEntry
resolveEntry cat pack ref = go candidates
  where
    candidates = [p <> "/" <> ref | Just p <- [pack], not ("/" `T.isInfixOf` ref)] ++ [ref]
    go [] = findEntry cat ref
    go (r : rest) = case findEntry cat r of
      Left (GeneratorNotFound _ _) | not (null rest) -> go rest
      Left (PackNotConfigured _ _) | not (null rest) -> go rest
      other -> other

-- | Resolve and load a generator named in a [[uses]] step.
resolver :: Monad m => Workspace m -> Catalog -> Maybe Text -> Text -> m (Either ResolveFailure Generator)
resolver root cat pack ref = case resolveEntry cat pack ref of
  Right entry -> first (ResolveInvalid . describeCatalogError) <$> loadGenerator root cat entry
  Left err@(GeneratorNotFound _ _) -> pure (Left (ResolveNotFound (describeCatalogError err)))
  Left err@(PackNotConfigured _ _) -> pure (Left (ResolveNotFound (describeCatalogError err)))
  Left err -> pure (Left (ResolveInvalid (describeCatalogError err)))

-- | The pack an entry belongs to, if any (for resolving its [[uses]]).
packOfEntry :: CatalogEntry -> Maybe Text
packOfEntry = entryPack
