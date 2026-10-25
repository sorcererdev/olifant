-- | Finding generators (project-local and in packs) and resolving their
-- template and exemplar sources into a runnable 'Generator'.
module Olifant.Catalog
  ( Catalog
  , packsFromConfig
  , catalogEntries
  , catalogProject
  , catalogName
  , catalogRecognize
  , RecognizeConfig (..)
  , catalogMigrate
  , MigrateConfig (..)
  , catalogDecideModel
  , catalogConjureWire
  , catalogPlans
  , PlansConfig (..)
  , defaultPlans
  , catalogPacks
  , catalogPackRoots
  , catalogBroken
  , catalogLanguages
  , catalogBrokenLanguages
  , catalogRegions
  , catalogBrokenRegions
  , catalogModel
  , catalogRules
  , catalogBrokenRules
  , catalogRulesConfig
  , catalogModelMaps
  , catalogBrokenModelMaps
  , CatalogEntry (..)
  , CatalogError (..)
  , describeCatalogError
  , CatalogPaths (..)
  , openProjectCatalog
  , PackCache
  , newPackCache
  , openProjectCatalogCached
  , packFingerprint
  , openPackCatalog
  , findEntry
  , loadGenerator
  , checkEntry
  , resolver
  , resolveEntry
  , packOfEntry
  ) where

import Data.Bifunctor (bimap, first)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import Numeric (showHex)
import Data.Functor ((<&>))
import Data.Either (lefts, rights)
import Data.List (sort, sortOn)
import Data.Map.Strict qualified as M
import Olifant.Model (ModelConfig (..), defaultModelConfig, parseModelConfig)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generator
import Olifant.Snippet (withHoleSources)
import Olifant.Anchor (Phrase (..), Target (..), itemsOfTarget, parsePhrase, positionWords)
import Olifant.Language (Language (..), Match (..), Syntax, languageFor, matchShape, parseLanguage)
import Text.Regex.TDFA (Regex, makeRegexM, matchTest)
import Olifant.ProjectModel (ModelMap, parseModelMap)
import Olifant.Region (RegionDef, RegionDefWith (..), parseRegions)
import Olifant.Rules (FixDef (..), RuleDef (..), RulesConfig (..), parseRules, parseRulesConfig)
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
  , -- | rules (spec §5.9): packs' then the project's; a later one replaces an earlier one with its id
    catalogRules :: [RuleDef]
  , catalogBrokenRules :: [(FilePath, Text)]
  , -- | the project's `[rules]`: severities and allowed paths
    catalogRulesConfig :: RulesConfig
  , -- | how each pack's conventions map onto the shared project model (its `model.toml`)
    catalogModelMaps :: [ModelMap]
  , catalogBrokenModelMaps :: [(FilePath, Text)]
  , -- | where each pack is (resolved): the language server watches them
    catalogPackRoots :: [FilePath]
  , -- | the project's `name` in its config.toml (its model's ids), if given
    catalogName :: Maybe Text
  , -- | `[recognize]`: convention recognition on or off, and the model asked
    -- when it is unsure
    catalogRecognize :: RecognizeConfig
  , -- | `[migrate]`: strangling a legacy codebase (none: not migrating)
    catalogMigrate :: Maybe MigrateConfig
  , -- | `[decide] model` (typed decisions; `[recognize] model` is its alias)
    catalogDecideModel :: Maybe Text
  , -- | `[conjure] wire`: the generator id that wires a missing port
    catalogConjureWire :: Maybe Text
  , -- | `[plans]`: where plans live, and the runner that drafts and does them
    catalogPlans :: PlansConfig
  }

-- | `[plans]`: the plans directory, the runner command, its model, a timeout
-- in seconds, and extra `--allowedTools` entries for `do`.
data PlansConfig = PlansConfig
  { pcDir :: FilePath
  , pcRunner :: FilePath
  , pcModel :: Text
  , pcTimeout :: Int
  , pcDoTools :: [Text]
  }
  deriving (Eq, Show)

defaultPlans :: PlansConfig
defaultPlans = PlansConfig "plans" "claude" "sonnet" 300 []

-- | `[recognize] enabled` (default true) and `model` (a [model.backends]
-- name; none: deterministic only).
-- | `[migrate]`: the legacy globs, the base ref new code is measured from,
-- the switches registry, how many new lines in one legacy function make a
-- change look like new work, and the node kinds counted as functions.
data MigrateConfig = MigrateConfig {mgLegacy :: [Text], mgBase :: Text, mgSwitches :: Maybe FilePath, mgNewWorkLines :: Int, mgCount :: [Text]}
  deriving stock (Eq, Show)

data RecognizeConfig = RecognizeConfig {recEnabled :: Bool, recModel :: Maybe Text}
  deriving stock (Eq, Show)

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
openProjectCatalog root = openProjectCatalogWith (loadPack root) root

-- | Packs parsed once and shared by the projects that use them (the
-- language server's many projects), by name and resolved path; a pack whose
-- files changed (its 'packFingerprint') is parsed again.
newtype PackCache = PackCache (IORef (M.Map (Text, FilePath) (Text, LoadedPack)))

newPackCache :: IO PackCache
newPackCache = PackCache <$> newIORef M.empty

-- | 'openProjectCatalog', its packs from the cache.
openProjectCatalogCached :: PackCache -> Workspace IO -> CatalogPaths -> IO (Either CatalogError Catalog)
openProjectCatalogCached (PackCache cache) root = openProjectCatalogWith cached root
  where
    cached (name, dir) = do
      fingerprint <- packFingerprint root dir
      known <- M.lookup (name, dir) <$> readIORef cache
      case known of
        Just (seen, pack) | seen == fingerprint -> pure pack
        _ -> do
          pack <- loadPack root (name, dir)
          modifyIORef' cache (M.insert (name, dir) (fingerprint, pack))
          pure pack

-- | A hash of a pack's files, their paths and contents: equal for an
-- unchanged pack, different when any file changed.
packFingerprint :: Monad m => Workspace m -> FilePath -> m Text
packFingerprint root dir = do
  files <- sort <$> listFiles root dir
  texts <- traverse (\f -> (f,) <$> readText root (dir </> f)) files
  let bytes = BS.unpack (TE.encodeUtf8 (T.concat [T.pack f <> "\0" <> fromMaybe "" t <> "\0" | (f, t) <- texts]))
      fnv = foldl (\h b -> (h `xor` fromIntegral b) * 1099511628211) (14695981039346656037 :: Word64) bytes
  pure (T.pack (showHex fnv ""))

-- | 'openProjectCatalog' with packs loaded by @loadP@.
openProjectCatalogWith :: Monad m => ((Text, FilePath) -> m LoadedPack) -> Workspace m -> CatalogPaths -> m (Either CatalogError Catalog)
openProjectCatalogWith loadP root paths = do
  let project = catalogProjectDir paths
      userConfig = fmap (</> "config.toml") (catalogUserConfigDir paths)
      projectConfig = project </> ".olifant" </> "config.toml"
  userPacks <- maybe (pure (Right [])) (packsIn (fromMaybe "." (catalogUserConfigDir paths))) userConfig
  projectPacks <- packsIn project projectConfig
  case (,) <$> userPacks <*> projectPacks of
    Left err -> pure (Left err)
    Right (fromUser, fromProject) -> do
      let packRoots = M.toList (M.fromList (fromUser ++ fromProject))
      result <- openCatalogAt root loadP (Just (project </> ".olifant" </> "generators")) packRoots
      own <- regionsIn root (project </> ".olifant")
      ownRules <- rulesIn root (project </> ".olifant")
      rulesConfig <- readText root projectConfig <&> \case
        Nothing -> Right (RulesConfig [] [])
        Just src -> first (ConfigInvalid projectConfig) (Toml.parseToml src >>= parseRulesConfig)
      userModel <- maybe (pure (Right Nothing)) modelIn userConfig
      projectModel <- modelIn projectConfig
      name <- readText root projectConfig <&> \case
        Nothing -> Right Nothing
        Just src -> first (ConfigInvalid projectConfig) (Toml.parseToml src >>= Toml.optional "name" Toml.text)
      recognize <- readText root projectConfig <&> \case
        Nothing -> Right (RecognizeConfig True Nothing)
        Just src -> first (ConfigInvalid projectConfig) (Toml.parseToml src >>= recognizeIn)
      decide <- readText root projectConfig <&> \case
        Nothing -> Right Nothing
        Just src -> first (ConfigInvalid projectConfig) (Toml.parseToml src >>= decideIn)
      conjure <- readText root projectConfig <&> \case
        Nothing -> Right Nothing
        Just src -> first (ConfigInvalid projectConfig) (Toml.parseToml src >>= conjureIn)
      plans <- readText root projectConfig <&> \case
        Nothing -> Right Nothing
        Just src -> first (ConfigInvalid projectConfig) (Toml.parseToml src >>= plansIn)
      migrate <- readText root projectConfig <&> \case
        Nothing -> Right Nothing
        Just src -> first (ConfigInvalid projectConfig) (Toml.parseToml src >>= migrateIn)
      pure $ case (,,,,,,,,) <$> userModel <*> projectModel <*> rulesConfig <*> name <*> recognize <*> migrate <*> decide <*> conjure <*> plans of
        Left err -> Left err
        Right (userM, projectM, rc, named, rec', mig, dec, conj, plns) ->
          let model = fromMaybe defaultModelConfig (maybe userM Just projectM)
           in fmap (\c -> checkFixes (addRules ownRules (addRegions own c {catalogProject = Just project, catalogModel = model, catalogRulesConfig = rc, catalogName = named, catalogRecognize = rec', catalogMigrate = mig, catalogDecideModel = maybe (recModel rec') Just dec, catalogConjureWire = conj, catalogPlans = fromMaybe defaultPlans plns}))) result
  where
    recognizeIn top =
      Toml.optional "recognize" Toml.table top >>= \case
        Nothing -> Right (RecognizeConfig True Nothing)
        Just t -> Toml.inKey "recognize" $ do
          Toml.onlyKeys ["enabled", "model"] t
          RecognizeConfig <$> (fromMaybe True <$> Toml.optional "enabled" Toml.bool t) <*> Toml.optional "model" Toml.text t
    decideIn top =
      Toml.optional "decide" Toml.table top >>= \case
        Nothing -> Right Nothing
        Just t -> Toml.inKey "decide" (Toml.onlyKeys ["model"] t >> Toml.optional "model" Toml.text t)
    conjureIn top =
      Toml.optional "conjure" Toml.table top >>= \case
        Nothing -> Right Nothing
        Just t -> Toml.inKey "conjure" (Toml.onlyKeys ["wire"] t >> Toml.optional "wire" Toml.text t)
    plansIn top =
      Toml.optional "plans" Toml.table top >>= \case
        Nothing -> Right Nothing
        Just t -> Toml.inKey "plans" $ do
          Toml.onlyKeys ["dir", "runner", "model", "timeout", "do_tools"] t
          fmap Just $
            PlansConfig
              <$> (maybe (pcDir defaultPlans) T.unpack <$> Toml.optional "dir" Toml.text t)
              <*> (maybe (pcRunner defaultPlans) T.unpack <$> Toml.optional "runner" Toml.text t)
              <*> (fromMaybe (pcModel defaultPlans) <$> Toml.optional "model" Toml.text t)
              <*> (maybe (pcTimeout defaultPlans) fromInteger <$> Toml.optional "timeout" Toml.int t)
              <*> (fromMaybe (pcDoTools defaultPlans) <$> Toml.optional "do_tools" (Toml.list Toml.text) t)
    migrateIn top =
      Toml.optional "migrate" Toml.table top >>= \case
        Nothing -> Right Nothing
        Just t -> Toml.inKey "migrate" $ do
          Toml.onlyKeys ["legacy", "base", "switches", "new_work_lines", "count"] t
          fmap Just $
            MigrateConfig
              <$> Toml.required "legacy" (Toml.list Toml.text) t
              <*> (fromMaybe "main" <$> Toml.optional "base" Toml.text t)
              <*> (fmap T.unpack <$> Toml.optional "switches" Toml.text t)
              <*> (maybe 15 fromInteger <$> Toml.optional "new_work_lines" Toml.int t)
              <*> (fromMaybe ["function_definition"] <$> Toml.optional "count" (Toml.list Toml.text) t)
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
openPackCatalog root = openCatalogAt root (loadPack root) Nothing

packsFromConfig :: Text -> Either Text [(Text, FilePath)]
packsFromConfig src = do
  top <- Toml.parseToml src
  packs <- Toml.optional "packs" Toml.table top
  traverse
    (\(name, v) -> Toml.inKey "packs" (Toml.inKey name (Toml.table v >>= \t -> Toml.onlyKeys ["path"] t >> (name,) . T.unpack <$> Toml.required "path" Toml.text t)))
    (maybe [] M.toList packs)

openCatalogAt :: Monad m => Workspace m -> ((Text, FilePath) -> m LoadedPack) -> Maybe FilePath -> [(Text, FilePath)] -> m (Either CatalogError Catalog)
openCatalogAt root loadP localDir packs = do
  states <- traverse (\(name, r) -> (name,r,) <$> inspectPath root r) packs
  case [(name, r) | (name, r, st) <- states, st /= Directory] of
    (name, r) : _ -> pure (Left (PackMissing name r))
    [] -> Right <$> openAt root loadP localDir packs

openAt :: Monad m => Workspace m -> ((Text, FilePath) -> m LoadedPack) -> Maybe FilePath -> [(Text, FilePath)] -> m Catalog
openAt root loadP localDir packs = do
  local <- maybe (pure []) (discover root Nothing Nothing) localDir
  fromPacks <- traverse loadP packs
  let loaded = local ++ concatMap lpEntries fromPacks
      langs = concatMap lpLanguages fromPacks
      base = Catalog Nothing (sortOn entryId (rights loaded)) (sortOn (\(gid, _, _) -> gid) (lefts loaded)) (map fst packs) (rights langs) (lefts langs) [] [] defaultModelConfig [] [] (RulesConfig [] []) [m | Just (Right m) <- map lpModel fromPacks] [b | Just (Left b) <- map lpModel fromPacks] (map snd packs) Nothing (RecognizeConfig True Nothing) Nothing Nothing Nothing defaultPlans
  pure (foldl (flip addRules) (foldl (flip addRegions) base (map lpRegions fromPacks)) (map lpRules fromPacks))

-- | What one pack brings, parsed: its generators (or why each is broken),
-- language adapters, regions and rules.
data LoadedPack = LoadedPack
  { lpEntries :: [Either (GeneratorId, FilePath, Text) CatalogEntry]
  , lpLanguages :: [Either (FilePath, Text) Language]
  , lpRegions :: Either (FilePath, Text) [RegionDef]
  , lpRules :: Either (FilePath, Text) [RuleDef]
  , lpModel :: Maybe (Either (FilePath, Text) ModelMap)
  }

loadPack :: Monad m => Workspace m -> (Text, FilePath) -> m LoadedPack
loadPack root (name, r) =
  LoadedPack <$> discover root (Just name) (Just r) (r </> "generators") <*> languagesIn <*> regionsIn root r <*> rulesIn root r <*> modelIn
  where
    modelIn = readText root (r </> "model.toml") <&> fmap (first (r </> "model.toml",) . parseModelMap)
    languagesIn = do
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

-- | The generators in a folder (each `NAME/generator.toml`), as a pack's or
-- the project's own.
discover :: Monad m => Workspace m -> Maybe Text -> Maybe FilePath -> FilePath -> m [Either (GeneratorId, FilePath, Text) CatalogEntry]
discover root pack packRoot dir = do
  files <- listFiles root dir
  let gens = [takeDirectory f | f <- files, takeFileName f == "generator.toml", length (splitDirectories f) == 2]
  traverse load gens
  where
    load g = do
      let gdir = dir </> g
          gname = T.pack g
          gid = GeneratorId (maybe gname (\p -> p <> "/" <> gname) pack)
          path = gdir </> "generator.toml"
      src <- readText root path
      pure $ case src of
        Nothing -> Left (gid, path, "unreadable")
        Just src' -> bimap (gid,path,) (CatalogEntry gid pack gdir packRoot) (parseGenerator gid src')

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
loadGenerator root syntax cat entry = (>>= sourced) . sequenceA <$> traverse resolve (entryDefinition entry)
  where
    -- `[holes.NAME]` sources go into the hole markers (spec §4.9)
    sourced g = first (SourceInvalid gid (entryDir entry </> "generator.toml")) (withHoleSources g)
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
    Right _ -> case newProblems of
      why : _ -> pure (Just why)
      [] -> listToMaybe . catMaybes <$> traverse targetProblem (concatMap fixedTargets (genEdits (entryDefinition entry)))
  where
    -- an enum's `new`: generators of the pack, each template over their values
    newProblems =
      [ why
      | i <- genInputs (entryDefinition entry)
      , (n, t) <- inputNew i
      , Just why <- [newProblem (inputName i) n t]
      ]
    newProblem input n t = case resolveEntry cat (packOfEntry entry) n of
      Left _ -> Just ("input " <> input <> ": new names no generator `" <> n <> "`")
      Right other ->
        let d = entryDefinition other
            known = S.fromList (map inputName (genInputs d) ++ map fst (genDerived d))
         in case S.toList (freeVariables t `S.difference` known) of
              v : _ -> Just ("input " <> input <> ": new " <> n <> ": {= " <> v <> " =} is not an input or derived value of " <> n)
              [] -> Nothing
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

-- | Read `rules.toml` in a directory (a pack root, or a project's `.olifant`).
rulesIn :: Monad m => Workspace m -> FilePath -> m (Either (FilePath, Text) [RuleDef])
rulesIn root dir = do
  let path = dir </> "rules.toml"
  readText root path <&> \case
    Nothing -> Right []
    Just src -> first (path,) (parseRules src)

-- | Add rules (or a broken file); a rule replaces an earlier one with its id.
addRules :: Either (FilePath, Text) [RuleDef] -> Catalog -> Catalog
addRules found cat = case found of
  Left broken -> cat {catalogBrokenRules = catalogBrokenRules cat ++ [broken]}
  Right defs -> cat {catalogRules = [r | r <- catalogRules cat, ruleId r `notElem` map ruleId defs] ++ defs}

-- | A rule's fix must name a generator that takes the rule's path slots
-- (review C1); the project's `[rules]` must name rules that exist (review
-- I8). Anything else is broken, and `check` fails on it.
checkFixes :: Catalog -> Catalog
checkFixes cat =
  let entries = [(generatorIdText (entryId e), map inputName (genInputs (entryDefinition e))) | e <- catalogEntries cat]
      slots r = [T.takeWhile (/= '}') piece | piece <- drop 1 (T.splitOn "{" (ruleFiles r)), not ("=" `T.isPrefixOf` piece)]
      problem r = listToMaybe (mapMaybe (fixProblem r) (ruleFixes r))
      fixProblem r d = case lookup (fdGenerator d) entries of
        Nothing -> Just ("rule `" <> ruleId r <> "`: fix names no generator `" <> fdGenerator d <> "`")
        Just inputs ->
          let given = maybe [("`{" <> x <> "}`", x) | x <- slots r] (map (\(k, _) -> ("`" <> k <> "` (in with)", k))) (fdWith d)
           in case [shown | (shown, x) <- given, x `notElem` inputs] of
                shown : _ -> Just ("rule `" <> ruleId r <> "`: " <> shown <> " is not an input of " <> fdGenerator d <> " (its inputs: " <> T.intercalate ", " inputs <> ")")
                [] -> Nothing
      bad = [(r, why) | r <- catalogRules cat, Just why <- [problem r]]
      known = map ruleId (catalogRules cat)
      config = catalogRulesConfig cat
      unknownSeverity = [("[rules]", "no rule named `" <> k <> "`") | (k, _) <- rcSeverity config, k `notElem` known]
      unknownAllow = [("[rules.allow]", "no rule named `" <> k <> "`") | (k, _, _) <- rcAllow config, k `notElem` known]
   in cat
        { catalogRules = [r | r <- catalogRules cat, ruleId r `notElem` map (ruleId . fst) bad]
        , catalogBrokenRules = catalogBrokenRules cat ++ [("rules.toml", why) | (_, why) <- bad] ++ unknownSeverity ++ unknownAllow
        }

-- | Add regions (or a broken file); a region replaces an earlier one with its name.
addRegions :: Either (FilePath, Text) [RegionDef] -> Catalog -> Catalog
addRegions found cat = case found of
  Left broken -> cat {catalogBrokenRegions = catalogBrokenRegions cat ++ [broken]}
  Right defs -> cat {catalogRegions = foldl place (catalogRegions cat) defs}
  where
    place acc r
      | any ((== regionName r) . regionName) acc = [if regionName x == regionName r then r else x | x <- acc]
      | otherwise = acc ++ [r]
