-- | The filesystem port. Everything Olifant reads or writes goes through a
-- 'Workspace'; tests use the in-memory adapter.
module Olifant.Workspace
  ( Workspace (..)
  , PathState (..)
  , memoryWorkspace
  , diskWorkspace
  , within
  , normalisePath
  , isSafeRelative
  ) where

import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (addTrailingPathSeparator, isAbsolute, joinPath, splitDirectories, takeDirectory, (</>))

data Workspace m = Workspace
  { readText :: FilePath -> m (Maybe Text)
  , writeText :: FilePath -> Text -> m ()
  , fileExists :: FilePath -> m Bool
  , listFiles :: FilePath -> m [FilePath]
  , inspectPath :: FilePath -> m PathState
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
        if isDir then sort <$> walk base "" else pure []
    , inspectPath = \p -> do
        let full = root </> p
        isFile <- doesFileExist full
        isDir <- doesDirectoryExist full
        if isFile
          then either (const Unreadable) Readable . decodeUtf8' <$> BS.readFile full
          else pure (if isDir then Directory else Absent)
    }
  where
    walk base rel = do
      entries <- listDirectory (if null rel then base else base </> rel)
      concat
        <$> forM
          entries
          ( \e -> do
              let relE = if null rel then e else rel </> e
              isDir <- doesDirectoryExist (base </> relE)
              if isDir then walk base relE else pure [relE]
          )

-- | View a workspace from a subdirectory.
within :: FilePath -> Workspace m -> Workspace m
within dir ws =
  Workspace
    { readText = readText ws . inside
    , writeText = writeText ws . inside
    , fileExists = fileExists ws . inside
    , listFiles = listFiles ws . inside
    , inspectPath = inspectPath ws . inside
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
