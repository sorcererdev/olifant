{-# LANGUAGE MultiWayIf #-}

-- | Internal to Olifant.Server: a plan as edits an editor applies — per
-- file, the smallest span of lines that changed (so the cursor and folds
-- stay put), at the document's version when it is open — and positions
-- in the editor's units (0-based lines, UTF-16 columns).
module Olifant.Server.Edits
  ( FileEdit (..)
  , editsOfPlan
  , smallestChange
  , editorPosition
  , bytePosition
  ) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Olifant.Generation (Plan (..))
import Olifant.Server.State (ServerState (..))
import Olifant.Workspace (normalisePath)
import System.FilePath ((</>))

-- | One file's change: its absolute path, the open document's version (none
-- when it is not open), whether it is new, and the span replaced (0-based
-- line, UTF-16 column; end exclusive) with its new text.
data FileEdit = FileEdit
  { fePath :: FilePath
  , feVersion :: Maybe Int
  , feCreate :: Bool
  , feRange :: ((Int, Int), (Int, Int))
  , feText :: Text
  }
  deriving stock (Eq, Show)

-- | The edits a plan makes in the project at @root@, file by file.
editsOfPlan :: ServerState -> FilePath -> Plan -> [FileEdit]
editsOfPlan s root plan =
  [ case M.findWithDefault Nothing rel (planBefore plan) of
      Nothing -> FileEdit path version True ((0, 0), (0, 0)) new
      Just old -> let (range, text) = smallestChange old new in FileEdit path version False range text
  | (rel, new) <- M.toList (planWrites plan)
  , let path = normalisePath (root </> rel)
        version = fst <$> M.lookup path (ssDocs s)
  , M.lookup rel (planBefore plan) /= Just (Just new)
  ]

-- | The smallest run of whole lines to replace to turn @old@ into @new@:
-- the lines both share at the start and at the end stay.
smallestChange :: Text -> Text -> (((Int, Int), (Int, Int)), Text)
smallestChange old new =
  let os = T.splitOn "\n" old
      ns = T.splitOn "\n" new
      common = length (takeWhile id (zipWith (==) os ns))
      maxTail = min (length os) (length ns) - common
      shared = length (takeWhile id (take maxTail (zipWith (==) (reverse os) (reverse ns))))
      middle = take (length ns - common - shared) (drop common ns)
      lastLine = length os - 1
      end = (lastLine, utf16Length (fromMaybe "" (lookupAt lastLine os)))
   in if
        | shared > 0 -> (((common, 0), (length os - shared, 0)), T.concat [l <> "\n" | l <- middle])
        | common == 0 -> (((0, 0), end), T.intercalate "\n" middle)
        -- to the end of the text, from the end of the last shared line: the
        -- newline after it goes with what follows (there may be none now)
        | otherwise -> (((common - 1, utf16Length (fromMaybe "" (lookupAt (common - 1) os))), end), T.concat ["\n" <> l | l <- middle])
  where
    lookupAt i xs = case drop i xs of
      x : _ -> Just x
      [] -> Nothing

-- | A finding's position (1-based line, 1-based UTF-8 byte column) in the
-- editor's units (0-based line, UTF-16 column), given the line's text.
editorPosition :: Text -> (Int, Int) -> (Int, Int)
editorPosition lineText (line, byteCol) =
  (line - 1, utf16Length (TE.decodeUtf8Lenient (BS.take (byteCol - 1) (TE.encodeUtf8 lineText))))

-- | An editor position (0-based line, UTF-16 column) as Olifant's (1-based
-- line, 1-based UTF-8 byte column), given the line's text.
bytePosition :: Text -> (Int, Int) -> (Int, Int)
bytePosition lineText (line, col) = (line + 1, 1 + BS.length (TE.encodeUtf8 (takeUtf16 col lineText)))
  where
    takeUtf16 n t = case T.uncons t of
      Just (c, rest) | n > 0 -> T.cons c (takeUtf16 (n - (if fromEnum c > 0xFFFF then 2 else 1)) rest)
      _ -> ""

utf16Length :: Text -> Int
utf16Length = T.foldl' (\n c -> n + if fromEnum c > 0xFFFF then 2 else 1) 0
