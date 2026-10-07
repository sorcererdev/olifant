-- | The filesystem port. Everything Olifant reads or writes goes through a
-- 'Workspace'; tests use the in-memory adapter.
module Olifant.Workspace
  ( Workspace (..)
  , PathState (..)
  , pruned
  , memoryWorkspace
  , diskWorkspace
  , within
  , overlay
  , overlayMany
  , hidePath
  , normalisePath
  , isSafeRelative
  ) where

import Control.Monad (filterM, forM)
import Data.ByteString qualified as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, nub, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Olifant.Process (runBytes)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, listDirectory)
import System.Directory qualified as Dir
import System.Exit (ExitCode (..))
import System.FilePath (addTrailingPathSeparator, isAbsolute, joinPath, splitDirectories, takeDirectory, (</>))

data Workspace m = Workspace
  { readText :: FilePath -> m (Maybe Text)
  , writeText :: FilePath -> Text -> m ()
  , fileExists :: FilePath -> m Bool
  , listFiles :: FilePath -> m [FilePath]
  , inspectPath :: FilePath -> m PathState
  , removeFile :: FilePath -> m ()
  -- ^ Delete a file if it exists (blessing snapshots; generation never deletes).
  }

-- | What is at a path. 'Unreadable' means a file exists but is not UTF-8
-- text: it must never be mistaken for 'Absent' (that would overwrite it).
data PathState = Absent | Readable Text | Unreadable | Directory
  deriving stock (Eq, Show)

memoryWorkspace :: [(FilePath, Text)] -> IO (Workspace IO, IO (Map FilePath Text))
memoryWorkspace initial = do
  ref <- newIORef (M.fromList [(normalisePath p, t) | (p, t) <- initial])
  let ws =
        Workspace
          { readText = \p -> M.lookup (normalisePath p) <$> readIORef ref
          , writeText = \p t -> modifyIORef' ref (M.insert (normalisePath p) t)
          , fileExists = \p -> M.member (normalisePath p) <$> readIORef ref
          , listFiles = \dir -> do
              files <- readIORef ref
              let prefix = case normalisePath dir of
                    "." -> ""
                    d -> addTrailingPathSeparator d
              pure [drop (length prefix) f | f <- M.keys files, prefix `isPrefixOf` f]
          , inspectPath = \p -> do
              files <- readIORef ref
              let key = normalisePath p
                  prefix = if key == "." then "" else addTrailingPathSeparator key
              pure $ case M.lookup key files of
                Just t -> Readable t
                Nothing
                  | any (prefix `isPrefixOf`) (M.keys files) -> Directory
                  | otherwise -> Absent
          , removeFile = \p -> modifyIORef' ref (M.delete (normalisePath p))
          }
  pure (ws, readIORef ref)

diskWorkspace :: FilePath -> Workspace IO
diskWorkspace root =
  Workspace
    { readText = \p -> do
        let full = root </> p
        exists <- doesFileExist full
        if exists then either (const Nothing) Just . decodeUtf8' <$> BS.readFile full else pure Nothing
    , writeText = \p t -> do
        let full = root </> p
        createDirectoryIfMissing True (takeDirectory full)
        BS.writeFile full (encodeUtf8 t)
    , fileExists = doesFileExist . (root </>)
    , listFiles = \dir -> do
        let base = root </> dir
        isDir <- doesDirectoryExist base
        if not isDir
          then pure []
          else gitFiles base >>= maybe (sort <$> walk base "") pure
    , inspectPath = \p -> do
        let full = root </> p
        isFile <- doesFileExist full
        isDir <- doesDirectoryExist full
        if isFile
          then either (const Unreadable) Readable . decodeUtf8' <$> BS.readFile full
          else pure (if isDir then Directory else Absent)
    , removeFile = \p -> do
        let full = root </> p
        exists <- doesFileExist full
        if exists then Dir.removeFile full else pure ()
    }
  where
    -- Inside a git work tree, the project is what git says it is: tracked and
    -- new files, minus anything .gitignore excludes (and tracked files that
    -- were deleted). Outside one, walk, never descending into 'pruned'.
    gitFiles base =
      findExecutable "git" >>= \case
        Nothing -> pure Nothing
        Just git -> do
          inside <- runBytes git ["-C", base, "rev-parse", "--is-inside-work-tree"] Nothing ""
          case inside of
            Right (ExitSuccess, out, _) | BS.take 4 out == "true" ->
              runBytes git ["-C", base, "ls-files", "-z", "--cached", "--others", "--exclude-standard"] Nothing "" >>= \case
                Right (ExitSuccess, listing, _) -> do
                  let paths = [p | raw <- BS.split 0 listing, not (BS.null raw), Right t <- [decodeUtf8' raw], let p = T.unpack t, not (any (`elem` pruned) (splitDirectories p))]
                  Just . sort . nub <$> filterM (doesFileExist . (base </>)) paths
                _ -> pure Nothing
            _ -> pure Nothing
    walk base rel = do
      entries <- filter (`notElem` pruned) <$> listDirectory (if null rel then base else base </> rel)
      concat
        <$> forM
          entries
          ( \e -> do
              let relE = if null rel then e else rel </> e
              isDir <- doesDirectoryExist (base </> relE)
              if isDir then walk base relE else pure [relE]
          )

-- | View a workspace from a subdirectory.
-- | The workspace with one file's text replaced (an editor's unsaved
-- buffer, spec §4.8). Plans only; it is never written.
overlay :: Monad m => FilePath -> Text -> Workspace m -> Workspace m
overlay path text = overlayMany (M.singleton path text)

-- | The workspace with open documents' texts over it (the language
-- server's unsaved buffers): reads, existence and listing see them, and a
-- document not on disk yet is listed; everything else is the workspace's.
-- Plans only; it is never written.
-- | The workspace without one file: it reads as absent and is not listed
-- (planning a convention's version of a file someone is writing).
hidePath :: Monad m => FilePath -> Workspace m -> Workspace m
hidePath path0 ws =
  ws
    { readText = \p -> if hidden p then pure Nothing else readText ws p
    , fileExists = \p -> if hidden p then pure False else fileExists ws p
    , inspectPath = \p -> if hidden p then pure Absent else inspectPath ws p
    , listFiles = \dir -> do
        fs <- listFiles ws dir
        let prefix = case normalisePath dir of
              "." -> ""
              d -> d <> "/"
        pure [f | f <- fs, not (hidden (prefix <> f))]
    }
  where
    path = normalisePath path0
    hidden p = normalisePath p == path

overlayMany :: Monad m => Map FilePath Text -> Workspace m -> Workspace m
overlayMany docs0 ws =
  ws
    { readText = \p -> maybe (readText ws p) (pure . Just) (open p)
    , fileExists = \p -> maybe (fileExists ws p) (const (pure True)) (open p)
    , inspectPath = \p -> maybe (inspectPath ws p) (pure . Readable) (open p)
    , listFiles = \dir -> do
        fs <- listFiles ws dir
        let prefix = case normalisePath dir of
              "." -> ""
              d -> d <> "/"
            listed = S.fromList fs
            extra = [rel | t <- M.keys docs, prefix `isPrefixOf` t, let rel = drop (length prefix) t, rel `S.notMember` listed]
        pure (fs ++ extra)
    }
  where
    docs = M.mapKeys normalisePath docs0
    open p = M.lookup (normalisePath p) docs

within :: FilePath -> Workspace m -> Workspace m
within dir ws =
  Workspace
    { readText = readText ws . inside
    , writeText = writeText ws . inside
    , fileExists = fileExists ws . inside
    , listFiles = listFiles ws . inside
    , inspectPath = inspectPath ws . inside
    , removeFile = removeFile ws . inside
    }
  where
    inside p = normalisePath (dir </> p)

normalisePath :: FilePath -> FilePath
normalisePath p = case foldl step [] (splitDirectories p) of
  [] -> "."
  parts -> joinPath (reverse parts)
  where
    step acc "." = acc
    step acc@["/"] ".." = acc
    step (x : acc) ".." | x /= ".." = acc
    step acc part = part : acc

isSafeRelative :: FilePath -> Bool
isSafeRelative p = not (isAbsolute p) && n /= "." && ".." `notElem` splitDirectories n
  where
    n = normalisePath p

-- | Directories that hold dependencies, build output or VCS data: never part
-- of what generators and queries see, and never walked.
pruned :: [FilePath]
pruned = [".git", "node_modules", "elm-stuff", "dist-newstyle", "target", ".venv", "__pycache__", ".pytest_cache"]
