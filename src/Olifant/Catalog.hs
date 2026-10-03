-- | Finding generators (project-local and in packs) and resolving their
-- template and exemplar sources into a runnable 'Generator'.
module Olifant.Catalog
  ( Catalog
  , catalogEntries
  , catalogBroken
  , catalogLanguages
  , catalogBrokenLanguages
  , catalogRegions
  , catalogBrokenRegions
  , catalogModel
  , CatalogEntry (..)
  , CatalogError (..)
  , describeCatalogError
  , CatalogPaths (..)
  , openProjectCatalog
  , openPackCatalog
  , findEntry
  , loadGenerator
  , checkEntry
  , resolver
  , resolveEntry
  , packOfEntry
  ) where

import Data.Bifunctor (bimap, first)
import Data.Functor ((<&>))
import Data.Either (lefts, rights)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Olifant.Model (ModelConfig (..), parseModelConfig)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generator
import Olifant.Anchor (Phrase (..), Target (..), itemsOfTarget, parsePhrase, positionWords)
import Olifant.Language (Language (..), Match (..), Syntax, languageFor, matchShape, parseLanguage)
import Text.Regex.TDFA (Regex, makeRegexM, matchTest)
import Olifant.Region (RegionDef, RegionDefWith (..), parseRegions)
import Olifant.Template (describeExemplarError, describeTemplateError, exemplar, freeVariables, parseTemplate, templateSource)
import Olifant.Toml qualified as Toml
import Olifant.Workspace (PathState (..), Workspace (..), isSafeRelative, normalisePath)
import System.FilePath (splitDirectories, takeBaseName, takeDirectory, takeExtension, takeFileName, (</>))

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
  , -- | language adapters from every pack's `languages/*.toml` (spec §12), in pack order
    catalogLanguages :: [Language]
  , catalogBrokenLanguages :: [(FilePath, Text)]
  , -- | owned regions (spec §5.8): packs' then the project's; a later one replaces an earlier one of the same name
    catalogRegions :: [RegionDef]
  , catalogBrokenRegions :: [(FilePath, Text)]
  , -- | `[model]` (spec §6.4): the project's if it has one, else the user's
    catalogModel :: ModelConfig
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
  | PackMissing Text FilePath
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
  PackMissing name p -> "pack `" <> name <> "`: " <> T.pack p <> " is not a directory (check its path under [packs])"
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
      own <- regionsIn root (project </> ".olifant")
      userModel <- maybe (pure (Right Nothing)) modelIn userConfig
      projectModel <- modelIn projectConfig
      pure $ case (,) <$> userModel <*> projectModel of
        Left err -> Left err
        Right (userM, projectM) ->
          let model = fromMaybe (ModelConfig Nothing 2 []) (maybe userM Just projectM)
           in fmap (\c -> addRegions own c {catalogProject = Just project, catalogModel = model}) result
  where
    -- A config's `[model]`, if it has one.
    modelIn configPath =
      readText root configPath <&> \case
        Nothing -> Right Nothing
        Just src -> case Toml.parseToml src >>= \top -> if M.member "model" top then Just <$> parseModelConfig top else Right Nothing of
          Left err -> Left (ConfigInvalid configPath err)
          Right m -> Right m
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
  states <- traverse (\(name, r) -> (name,r,) <$> inspectPath root r) packs
  case [(name, r) | (name, r, st) <- states, st /= Directory] of
    (name, r) : _ -> pure (Left (PackMissing name r))
    [] -> Right <$> openAt root localDir packs

openAt :: Monad m => Workspace m -> Maybe FilePath -> [(Text, FilePath)] -> m Catalog
openAt root localDir packs = do
  local <- maybe (pure []) (discover Nothing Nothing) localDir
  fromPacks <- concat <$> traverse (\(name, r) -> discover (Just name) (Just r) (r </> "generators")) packs
  let loaded = local ++ fromPacks
  langs <- concat <$> traverse (languagesIn . snd) packs
  regions <- traverse (regionsIn root . snd) packs
  let base = Catalog Nothing (sortOn entryId (rights loaded)) (sortOn (\(gid, _, _) -> gid) (lefts loaded)) (map fst packs) (rights langs) (lefts langs) [] [] (ModelConfig Nothing 2 [])
  pure (foldl (flip addRegions) base regions)
  where
    languagesIn r = do
      let dir = r </> "languages"
      files <- listFiles root dir
      traverse
        ( \f -> do
            let path = dir </> f
            src <- readText root path
            pure $ case src of
              Nothing -> Left (path, "unreadable")
              Just t -> first (path,) (parseLanguage (T.pack (takeBaseName f)) r t)
        )
        [f | f <- files, takeExtension f == ".toml", length (splitDirectories f) == 1]
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

-- | Resolve a generator's sources (templates, exemplars, fragments) into
-- templates. Fragments need the syntax port and the catalog's adapters.
loadGenerator :: Monad m => Workspace m -> Syntax m -> Catalog -> CatalogEntry -> m (Either CatalogError Generator)
loadGenerator root syntax cat entry = sequenceA <$> traverse resolve (entryDefinition entry)
  where
    gid = entryId entry
    resolve = \case
      InlineText t -> pure (Right t)
      TemplateFile f -> fromFile (entryDir entry </> "templates" </> f) (first describeTemplateError . parseTemplate)
      ExemplarFile ref binds -> case exemplarPath ref of
        Left err -> pure (Left err)
        Right path -> fromFile path (first describeExemplarError . exemplar binds)
      FragmentFile ref target matching binds -> case exemplarPath ref of
        Left err -> pure (Left err)
        Right path ->
          readText root path >>= \case
            Nothing -> pure (Left (SourceMissing gid path))
            Just content -> case languageFor (catalogLanguages cat) path of
              Nothing -> pure (Left (SourceInvalid gid path "no language adapter for this file, so its fragments cannot be found"))
              Just lang ->
                itemsOfTarget syntax lang "items of" (T.words target) content <&> \case
                  Left (why, _) -> Left (SourceInvalid gid path ("fragment `" <> target <> "`: " <> why))
                  Right Nothing -> Left (SourceInvalid gid path ("fragment: `" <> target <> "` not found"))
                  Right (Just (_, items)) -> case [m | m <- items, maybe False (`matchTest` T.unpack (matchText m)) (makeRegexM (T.unpack matching) :: Maybe Regex)] of
                    [m] -> first (SourceInvalid gid path . describeExemplarError) (exemplar binds (dedent m))
                    ms -> Left (SourceInvalid gid path ("fragment `" <> target <> "` matching `" <> matching <> "` matched " <> T.pack (show (length ms)) <> " items (expected 1)"))
    -- A node's first line starts at its column; later lines keep the file's
    -- indentation, so remove the node's column from each of them.
    dedent m = case T.lines (T.replace "\r\n" "\n" (matchText m)) of
      [] -> ""
      (firstLine : rest) ->
        let col = snd (matchStart m)
            strip l = let ws = T.takeWhile (\c -> c == ' ' || c == '\t') l in T.drop (min col (T.length ws)) l
         in T.intercalate "\n" (firstLine : map strip rest)
    fromFile path build =
      readText root path <&> \case
        Nothing -> Left (SourceMissing gid path)
        Just src -> first (SourceInvalid gid path) (build src)
    -- Exemplars and fragments are read from inside the pack's fixture or
    -- the project, never from an absolute path or through `..`.
    exemplarPath ref = case T.stripPrefix "fixture:" ref of
      Just rest -> case entryPackRoot entry of
        Just r -> inside rest (r </> "fixtures" </> "app")
        Nothing -> Left (SourceInvalid gid (T.unpack ref) "`fixture:` exemplars are only available in packs")
      Nothing -> inside ref (fromMaybe "." (catalogProject cat))
      where
        inside rel base
          | isSafeRelative (T.unpack rel) = Right (base </> T.unpack rel)
          | otherwise = Left (SourceInvalid gid (T.unpack ref) "the path is outside the fixture or project (use a relative path without `..`)")

-- | What `list` and `show` report as INVALID beyond a parse error: sources
-- that do not load (a missing template, a broken exemplar or fragment), and
-- a fixed `at` target the file's language adapter does not have. Targets
-- with variables are left to run time.
checkEntry :: Monad m => Workspace m -> Syntax m -> Catalog -> CatalogEntry -> m (Maybe Text)
checkEntry root syntax cat entry =
  loadGenerator root syntax cat entry >>= \case
    Left err -> pure (Just (describeCatalogError err))
    Right _ -> listToMaybe . catMaybes <$> traverse targetProblem (concatMap fixedTargets (genEdits (entryDefinition entry)))
  where
    fixedTargets = \case
      Insert path anchors _ _ -> [(templateSource path, templateSource t) | AtPhrase t <- anchors, S.null (freeVariables t)]
      _ -> []
    targetProblem (path, phrase) = case (parsePhrase phrase, languageFor (catalogLanguages cat) (T.unpack path)) of
      (Right (Phrase pos (NamedTarget ws)), Just lang)
        | ws /= ["file"], not (any (\t -> isJust (matchShape t ws)) (langTargets lang)) ->
            -- An unknown target fails before any syntax query: this only builds the message.
            itemsOfTarget syntax lang (positionWords pos) ws "" <&> \case
              Left (why, _) -> Just ("`" <> phrase <> "` in " <> path <> ": " <> why)
              Right _ -> Nothing
      _ -> pure Nothing

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
resolver :: Monad m => Workspace m -> Syntax m -> Catalog -> Maybe Text -> Text -> m (Either ResolveFailure Generator)
resolver root syntax cat pack ref = case resolveEntry cat pack ref of
  Right entry -> first (ResolveInvalid . describeCatalogError) <$> loadGenerator root syntax cat entry
  Left err@(GeneratorNotFound _ _) -> pure (Left (ResolveNotFound (describeCatalogError err)))
  Left err@(PackNotConfigured _ _) -> pure (Left (ResolveNotFound (describeCatalogError err)))
  Left err -> pure (Left (ResolveInvalid (describeCatalogError err)))

-- | The pack an entry belongs to, if any (for resolving its [[uses]]).
packOfEntry :: CatalogEntry -> Maybe Text
packOfEntry = entryPack

-- | Read `regions.toml` in a directory (a pack root, or a project's `.olifant`):
-- the regions, with whole-region templates read relative to that directory.
regionsIn :: Monad m => Workspace m -> FilePath -> m (Either (FilePath, Text) [RegionDef])
regionsIn root dir = do
  let path = dir </> "regions.toml"
  readText root path >>= \case
    Nothing -> pure (Right [])
    Just src -> case parseRegions src of
      Left err -> pure (Left (path, err))
      Right defs -> do
        resolved <- traverse (fmap sequenceA . traverse template) defs
        pure (first (path,) (sequence resolved))
  where
    template f =
      readText root (dir </> f) <&> \case
        Nothing -> Left ("missing template " <> T.pack f)
        Just t -> first describeTemplateError (parseTemplate t)

-- | Add regions (or a broken file); a region replaces an earlier one with its name.
addRegions :: Either (FilePath, Text) [RegionDef] -> Catalog -> Catalog
addRegions found cat = case found of
  Left broken -> cat {catalogBrokenRegions = catalogBrokenRegions cat ++ [broken]}
  Right defs -> cat {catalogRegions = foldl place (catalogRegions cat) defs}
  where
    place acc r
      | any ((== regionName r) . regionName) acc = [if regionName x == regionName r then r else x | x <- acc]
      | otherwise = acc ++ [r]
