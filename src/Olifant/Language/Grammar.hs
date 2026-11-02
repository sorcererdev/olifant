-- | The grammar loader: a tree-sitter grammar is C source (a pack's or the
-- project's folder, or a git repository at a pinned commit), compiled once
-- with @cc@ into olifant's cache (@<cache>/grammars/<name>-<key>.dylib@) and
-- loaded with @dlopen@, its ABI checked against the runtime compiled into
-- olifant (@cbits/tree-sitter@). Nothing is compiled into the binary.
--
-- A load is once per process ('loaded', keyed by the library's path), a
-- failure too: a request that needs an absent grammar pays one failed fetch
-- or build per process, not one per request. Nothing here runs while a
-- language file is read or listed: only a parse ('loadGrammar'), ast-grep's
-- registration ('grammarLibrary') and doctor ('probeGrammar') build.
module Olifant.Language.Grammar
  ( GrammarSettings (..)
  , defaultSettings
  , setGrammarSettings
  , grammarSettings
  , GrammarFailure (..)
  , grammarFailureText
  , failureDetail
  , TSLanguage
  , Loaded (..)
  , grammarSources
  , grammarKey
  , grammarLibrary
  , loadGrammar
  , loadGrammarWith
  , loadIfBuilt
  , grammarLabel
  , abiRange
  , GrammarInfo (..)
  , probeGrammar
  ) where

import Control.Concurrent (myThreadId)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word16, Word32, Word64)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (peek)
import GHC.Clock (getMonotonicTime)
import Numeric (showHex)
import Olifant.Language.Adapter (Grammar (..), GrammarSource (..))
import Olifant.Process (runBytes, runBytesWithin)
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, getFileSize, getModificationTime, getXdgDirectory, makeAbsolute, removePathForcibly, renameDirectory, renameFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Info (arch, os)
import System.Posix.DynamicLinker (RTLDFlags (..), dlopen, dlsym)
import System.Posix.Process (getProcessID)

data TSLanguage

foreign import ccall "dynamic" callLanguage :: FunPtr (IO (Ptr TSLanguage)) -> IO (Ptr TSLanguage)
foreign import ccall unsafe "ts_language_abi_version" c_abiVersion :: Ptr TSLanguage -> IO Word32
foreign import ccall unsafe "ts_language_symbol_count" c_symbolCount :: Ptr TSLanguage -> IO Word32
foreign import ccall unsafe "ts_language_symbol_name" c_symbolName :: Ptr TSLanguage -> Word16 -> IO CString
foreign import ccall unsafe "ts_language_symbol_type" c_symbolType :: Ptr TSLanguage -> Word16 -> IO CInt
foreign import ccall unsafe "ots_abi_range" c_abiRange :: Ptr Word32 -> Ptr Word32 -> IO ()

-- | Where grammars are built and where local sources are looked for.
-- 'gsFind' finds a program on PATH (a spec stubs it: the suite runs in
-- parallel, so it cannot empty PATH).
data GrammarSettings = GrammarSettings
  { gsCache :: FilePath
  , gsLocalSources :: [FilePath]
  , gsFind :: String -> IO (Maybe FilePath)
  }

-- | Settings over a cache and local source folders, finding programs on PATH.
defaultSettings :: FilePath -> [FilePath] -> GrammarSettings
defaultSettings cache sources = GrammarSettings cache sources findExecutable

settingsRef :: IORef (Maybe GrammarSettings)
settingsRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE settingsRef #-}

-- | Once, before any load: 'Main' and the spec hook.
setGrammarSettings :: GrammarSettings -> IO ()
setGrammarSettings = writeIORef settingsRef . Just

-- | The set settings, else olifant's cache (@~/.cache/olifant@) and the
-- folders of @$OLIFANT_GRAMMAR_SOURCES@ (colon-separated).
grammarSettings :: IO GrammarSettings
grammarSettings =
  readIORef settingsRef >>= \case
    Just s -> pure s
    Nothing -> do
      cache <- getXdgDirectory XdgCache "olifant"
      local <- maybe [] (filter (not . null) . splitOn ':') <$> lookupEnv "OLIFANT_GRAMMAR_SOURCES"
      pure (defaultSettings cache local)
  where
    splitOn c xs = case break (== c) xs of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitOn c rest

-- | Why a grammar is not loaded.
data GrammarFailure
  = NoSource
  | NeedsCompiler Text
  | FetchFailed Text
  | BuildFailed Text
  | LoadFailed Text
  | AbiMismatch Word32 (Word32, Word32)
  deriving stock (Eq, Show)

-- | A failure as doctor's grammar row says it (the grammar's name for a bare one).
failureDetail :: Text -> GrammarFailure -> Text
failureDetail g = \case
  NoSource -> "no source (grammar = \"" <> g <> "\" names it only): units, generic anchors and counts need one"
  NeedsCompiler why -> "needs a C compiler to build: " <> why
  FetchFailed why -> why
  BuildFailed why -> "building failed: " <> why
  LoadFailed why -> "loading failed: " <> why
  AbiMismatch n (lo, hi) -> "ABI " <> tshow n <> "; this olifant reads " <> tshow lo <> "–" <> tshow hi

-- | A failure as a reader's Left says it: the grammar named first.
grammarFailureText :: Text -> GrammarFailure -> Text
grammarFailureText g f = "the `" <> g <> "` grammar is not loaded: " <> failureDetail g f

-- | A loaded grammar: the language, its ABI, its symbol names (named and
-- anonymous) and named node types, read once at load, and the library.
data Loaded = Loaded
  { ldLanguage :: Ptr TSLanguage
  , ldAbi :: Word32
  , ldSymbols :: M.Map Word32 Text
  , ldNodeTypes :: [Text]
  , ldLibrary :: FilePath
  }

-- | The ABI versions the runtime reads (its minimum compatible and its own).
abiRange :: (Word32, Word32)
abiRange = unsafePerformIO $ alloca $ \lo -> alloca $ \hi -> do
  c_abiRange lo hi
  (,) <$> peek lo <*> peek hi
{-# NOINLINE abiRange #-}

-- | How a grammar is named to a person: a git source's repository and short
-- revision (@tree-sitter-python \@293fdc0@), a folder's name, else the name.
grammarLabel :: Grammar -> Text
grammarLabel g = case grSource g of
  Just (SourceGit url rev _) -> lastSegment url <> " @" <> T.take 7 rev
  Just (SourceDir d) -> T.pack (takeFileName d)
  Nothing -> grName g
  where
    lastSegment u =
      let s = last ("" : filter (not . T.null) (T.splitOn "/" u))
       in fromMaybe s (T.stripSuffix ".git" s)

-- | The folder holding the grammar's @parser.c@, and whether it is a
-- checkout of its pinned revision (the key's identity is then the revision).
-- A git source is looked for under each local source folder
-- (@<d>/<name>/parser.c@) first; else it is the cache's checkout, fetched
-- when absent ('fetch' when asked to).
grammarSources :: GrammarSettings -> Grammar -> IO (Either GrammarFailure FilePath)
grammarSources s g = fmap fst <$> located s g True

located :: GrammarSettings -> Grammar -> Bool -> IO (Either GrammarFailure (FilePath, Bool))
located s g doFetch = case grSource g of
  Nothing -> pure (Left NoSource)
  Just (SourceDir d) -> Right . (,False) <$> makeAbsolute d
  Just (SourceGit url rev path) -> do
    local <- firstM (\d -> doesFileExist (d </> T.unpack (grName g) </> "parser.c")) (gsLocalSources s)
    case local of
      Just d -> Right . (,False) <$> makeAbsolute (d </> T.unpack (grName g))
      Nothing -> do
        let checkout = checkoutDir s g rev
            src = maybe checkout (checkout </>) path </> "src"
        present <- doesDirectoryExist checkout
        if present || not doFetch
          then pure (Right (src, True))
          else fmap (const (src, True)) <$> fetch s url rev checkout
  where
    firstM p = \case
      [] -> pure Nothing
      x : xs -> p x >>= \b -> if b then pure (Just x) else firstM p xs

-- | A git source's checkout: @<cache>/grammars/src/<name>-<rev7>@.
checkoutDir :: GrammarSettings -> Grammar -> Text -> FilePath
checkoutDir s g rev = gsCache s </> "grammars" </> "src" </> T.unpack (grName g <> "-" <> T.take 7 rev)

-- | A git source fetched at its pinned revision into its checkout:
-- @git init@, @git fetch --depth 1 URL REV@, @git checkout FETCH_HEAD@ in
-- an aside folder renamed on success, so a half-fetched checkout is never
-- used. Git's own transports only (no remote helpers), no redirects, no
-- hooks, symlinks checked out as plain files (nothing points out of the
-- checkout), and no prompt for credentials.
fetch :: GrammarSettings -> Text -> Text -> FilePath -> IO (Either GrammarFailure ())
fetch s url rev checkout =
  gsFind s "git" >>= \case
    Nothing -> pure (Left (failed "git is not on PATH"))
    Just git -> do
      createDirectoryIfMissing True (takeDirectory checkout)
      mine <- aside checkout
      removePathForcibly mine
      let run args = do
            ran <- runBytesWithin 600 "/usr/bin/env" (["GIT_TERMINAL_PROMPT=0", "GIT_SSH_COMMAND=ssh -o BatchMode=yes", "GIT_CONFIG_NOSYSTEM=1", git] ++ safe ++ args) Nothing ""
            pure $ case ran of
              Right (Just (ExitSuccess, out, _)) -> Right (T.strip (TE.decodeUtf8With lenientDecode out))
              Right (Just (ExitFailure _, _, err)) -> Left (lastLine (TE.decodeUtf8With lenientDecode err))
              Right Nothing -> Left "timed out after 600 s"
              Left why -> Left why
      r <- runSteps
        [ run ["init", "--quiet", "--template=", mine]
        , run ["-C", mine, "fetch", "--quiet", "--depth", "1", "--no-tags", T.unpack url, T.unpack rev]
        , run ["-C", mine, "checkout", "--quiet", "FETCH_HEAD"]
        , run ["-C", mine, "rev-parse", "HEAD"] >>= \case
            Right head' | head' /= rev -> pure (Left ("fetched " <> head' <> ", not " <> rev))
            other -> pure other
        ]
      case r of
        Left why -> removePathForcibly mine >> pure (Left (failed why))
        Right () -> do
          -- another process may have finished first: keep theirs
          there <- doesDirectoryExist checkout
          if there then removePathForcibly mine else renameDirectory mine checkout
          pure (Right ())
  where
    failed why = FetchFailed ("fetching " <> url <> " @" <> T.take 7 rev <> " failed: " <> why)
    safe =
      concatMap (\c -> ["-c", c])
        [ "protocol.allow=never", "protocol.https.allow=always", "protocol.ssh.allow=always"
        , "protocol.git.allow=always", "protocol.file.allow=always"
        , "http.followRedirects=false", "core.symlinks=false", "core.hooksPath=/dev/null"
        , "advice.detachedHead=false", "credential.helper="
        ]
    runSteps = \case
      [] -> pure (Right ())
      step : rest -> step >>= either (pure . Left) (const (runSteps rest))
    lastLine t = case reverse (filter (not . T.null) (map T.strip (T.lines t))) of
      l : _ -> l
      [] -> "git failed"

-- | The library's base name: the grammar's name and 8 hex of an fnv64 over
-- the source's identity (a checkout's revision; a folder's path and its
-- parser.c and scanner.c sizes and times), the runtime's ABI, the os and
-- the arch. A changed key is a new file; the old one is left.
grammarKey :: GrammarSettings -> Grammar -> FilePath -> IO Text
grammarKey s g dir = do
  identity <- case grSource g of
    Just (SourceGit _ rev path) | dir == maybe (checkoutDir s g rev) (checkoutDir s g rev </>) path </> "src" -> pure (T.unpack rev <> maybe "" ("/" <>) path)
    _ -> do
      stamps <- forM ["parser.c", "scanner.c"] $ \f -> do
        let p = dir </> f
        there <- doesFileExist p
        if there then (\sz t -> show (f, sz, t)) <$> getFileSize p <*> getModificationTime p else pure ""
      pure (dir <> concat stamps)
  let h = fnv (TE.encodeUtf8 (T.pack (show (identity, snd abiRange, os, arch))))
      hex = showHex h ""
  pure (grName g <> "-" <> T.pack (take 8 (replicate (16 - length hex) '0' <> hex)))
  where
    fnv = BS.foldl' (\acc b -> (acc `xor` fromIntegral b) * 1099511628211) (14695981039346656037 :: Word64)

-- | Every load of this process, by library path; failures too. Read
-- without a lock: a remembered answer never waits on another grammar's build.
loaded :: IORef (M.Map FilePath (Either GrammarFailure Loaded, Maybe Double))
loaded = unsafePerformIO (newIORef M.empty)
{-# NOINLINE loaded #-}

-- | One lock per library path: the first caller fetches, builds and loads,
-- the others for that library wait; other grammars do not.
building :: MVar (M.Map FilePath (MVar ()))
building = unsafePerformIO (newMVar M.empty)
{-# NOINLINE building #-}

-- | The library's answer, computed once per process, with the seconds its
-- fetch and build took when this process built it.
once :: FilePath -> IO (Either GrammarFailure Loaded, Maybe Double) -> IO (Either GrammarFailure Loaded, Maybe Double)
once lib act =
  M.lookup lib <$> readIORef loaded >>= \case
    Just r -> pure r
    Nothing -> do
      lock <- modifyMVar building $ \m -> case M.lookup lib m of
        Just l -> pure (m, l)
        Nothing -> newMVar () >>= \l -> pure (M.insert lib l m, l)
      withMVar lock $ \() ->
        M.lookup lib <$> readIORef loaded >>= \case
          Just r -> pure r
          Nothing -> do
            r <- try act
            let result = either (\(e :: SomeException) -> (Left (LoadFailed (firstLine (T.pack (show e)))), Nothing)) id r
            atomicModifyIORef' loaded (\m -> (M.insert lib result m, ()))
            pure result

-- | The grammar, loaded once per process under the set settings.
loadGrammar :: Grammar -> IO (Either GrammarFailure Loaded)
loadGrammar g = grammarSettings >>= \s -> loadGrammarWith s g

-- | The compiled library's path, built (and loaded) when absent: ast-grep
-- registers the same file.
grammarLibrary :: Grammar -> IO (Either GrammarFailure FilePath)
grammarLibrary g = fmap ldLibrary <$> loadGrammar g

-- | The grammar when it is loaded or built already, else Nothing: nothing
-- is fetched or compiled (reading config, 'Olifant.Catalog').
loadIfBuilt :: Grammar -> IO (Maybe (Either GrammarFailure Loaded))
loadIfBuilt g = do
  s <- grammarSettings
  located s g False >>= \case
    Left _ -> pure Nothing
    Right (dir, _) -> do
      lib <- libraryPath s g dir
      M.lookup lib <$> readIORef loaded >>= \case
        Just r -> pure (Just (fst r))
        Nothing -> doesFileExist lib >>= \built -> if built then Just . fst <$> loadTimed s g else pure Nothing

libraryPath :: GrammarSettings -> Grammar -> FilePath -> IO FilePath
libraryPath s g dir = (\key -> gsCache s </> "grammars" </> T.unpack key <> libraryExtension) <$> grammarKey s g dir

-- | 'loadGrammar' under given settings: the source located (a git source
-- not yet fetched still has its library's path), then, once per library
-- path: fetched, built and loaded.
loadGrammarWith :: GrammarSettings -> Grammar -> IO (Either GrammarFailure Loaded)
loadGrammarWith s g = fst <$> loadTimed s g

-- | 'loadGrammarWith', and the seconds the fetch and build took when this
-- process built it.
loadTimed :: GrammarSettings -> Grammar -> IO (Either GrammarFailure Loaded, Maybe Double)
loadTimed s g =
  located s g False >>= \case
    Left f -> pure (Left f, Nothing)
    Right (dir, _) -> do
      lib <- libraryPath s g dir
      once lib (buildAndLoad dir lib)
  where
    buildAndLoad dir lib = do
      built <- doesFileExist lib
      if built
        then (,Nothing) <$> open lib
        else
          getMonotonicTime >>= \t0 -> located s g True >>= \case
            Left f -> pure (Left f, Nothing)
            Right _ -> do
              compileLibrary s dir lib >>= \case
                Left f -> pure (Left f, Nothing)
                Right () -> do
                  t1 <- getMonotonicTime
                  (,Just (t1 - t0)) <$> open lib
    open lib =
      try (dlopen lib [RTLD_NOW, RTLD_LOCAL]) >>= \case
        Left (e :: SomeException) -> pure (Left (LoadFailed (firstLine (T.pack (show e)))))
        Right dl ->
          try (dlsym dl ("tree_sitter_" <> T.unpack (grName g))) >>= \case
            Left (e :: SomeException) -> pure (Left (LoadFailed ("no tree_sitter_" <> grName g <> " in " <> T.pack lib <> ": " <> firstLine (T.pack (show e)))))
            Right fp -> do
              lang <- callLanguage fp
              if lang == nullPtr
                then pure (Left (LoadFailed ("tree_sitter_" <> grName g <> " returned no language")))
                else do
                  abi <- c_abiVersion lang
                  let (lo, hi) = abiRange
                  if abi < lo || abi > hi
                    then pure (Left (AbiMismatch abi abiRange))
                    else do
                      symbols <- symbolsOf lang
                      types <- nodeTypesOf lang
                      pure (Right (Loaded lang abi symbols types lib))

libraryExtension :: String
libraryExtension = if os == "darwin" then ".dylib" else ".so"

-- | @cc -shared -fPIC -O2 -I <src> parser.c [scanner.c]@, written aside and
-- renamed: a load (a parallel spec, the server's threads, another olifant
-- process) never reads a half-written library, and builds at the same
-- moment do not collide. A failure is not cached on disk.
compileLibrary :: GrammarSettings -> FilePath -> FilePath -> IO (Either GrammarFailure ())
compileLibrary s src lib = do
  hasParser <- doesFileExist (src </> "parser.c")
  hasScanner <- doesFileExist (src </> "scanner.c")
  cppScanner <- doesFileExist (src </> "scanner.cc")
  if not hasParser
    then pure (Left (BuildFailed ("no parser.c in " <> T.pack src)))
    else
      if cppScanner && not hasScanner
        then pure (Left (BuildFailed "C++ scanners (scanner.cc) are not supported"))
        else
          gsFind s "cc" >>= \case
            Nothing -> pure (Left (NeedsCompiler "cc is not on PATH"))
            Just cc -> do
              createDirectoryIfMissing True (takeDirectory lib)
              mine <- aside lib
              let sources = (src </> "parser.c") : [src </> "scanner.c" | hasScanner]
              ran <- runBytes cc (["-shared", "-fPIC", "-O2", "-I", src] ++ sources ++ ["-o", mine]) Nothing ""
              case ran of
                Right (ExitSuccess, _, _) -> Right <$> renameFile mine lib
                Right (ExitFailure _, _, err) -> pure (Left (BuildFailed (firstErrorLine (TE.decodeUtf8With lenientDecode err))))
                Left why -> pure (Left (BuildFailed why))
  where
    firstErrorLine err = case filter ("error" `T.isInfixOf`) (T.lines err) of
      l : _ -> T.strip l
      [] -> firstLine err

-- | A name of this process and thread beside @path@.
aside :: FilePath -> IO FilePath
aside path = do
  pid <- getProcessID
  thread <- myThreadId
  pure (path <> ".tmp-" <> show pid <> "-" <> filter (/= ' ') (show thread))

symbolsOf :: Ptr TSLanguage -> IO (M.Map Word32 Text)
symbolsOf lang = do
  count <- c_symbolCount lang
  fmap M.fromList . forM (if count == 0 then [] else [0 .. count - 1]) $ \i ->
    (i,) . T.pack <$> (c_symbolName lang (fromIntegral i) >>= peekCString)

-- | The named node types (symbol type 0: regular), each once, in order.
nodeTypesOf :: Ptr TSLanguage -> IO [Text]
nodeTypesOf lang = do
  count <- c_symbolCount lang
  fmap (M.keys . M.fromList . map (,()) . concat) . forM (if count == 0 then [] else [0 .. fromIntegral count - 1]) $ \i -> do
    ty <- c_symbolType lang i
    if ty /= 0 then pure [] else (\n -> [T.pack n]) <$> (c_symbolName lang i >>= peekCString)

-- | What doctor's grammar row shows of a loaded grammar.
data GrammarInfo = GrammarInfo {giLabel :: Text, giAbi :: Word32, giLibrary :: FilePath, giBuiltNow :: Maybe Double}
  deriving stock (Eq, Show)

-- | Doctor's port: the grammar loaded as any first use loads it (fetched,
-- built, loaded), with the fetch and build's seconds when this process
-- built it (doctor's own reads may have been the first use).
probeGrammar :: Grammar -> IO (Either GrammarFailure GrammarInfo)
probeGrammar g = do
  s <- grammarSettings
  (r, secs) <- loadTimed s g
  pure (fmap (\l -> GrammarInfo (grammarLabel g) (ldAbi l) (ldLibrary l) secs) r)

firstLine :: Text -> Text
firstLine t = case filter (not . T.null) (map T.strip (T.lines t)) of
  l : _ -> l
  [] -> T.strip t

tshow :: Show a => a -> Text
tshow = T.pack . show
