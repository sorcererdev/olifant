module Olifant.Generation.Marker
  ( MarkerProblem (..)
  , insertBeforeMarker
  , alreadyPresent
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

data MarkerProblem = MarkerMissing | MarkerAmbiguous Int
  deriving stock (Eq, Show)

-- | Insert a snippet on the lines before the (unique) marker line, indented
-- like the marker. Returns the 1-based line where the snippet starts.
insertBeforeMarker :: Text -> Text -> Text -> Either MarkerProblem (Int, Text)
insertBeforeMarker marker snippet content =
  case [i | (i, l) <- zip [0 ..] ls, T.strip l == T.strip marker] of
    [] -> Left MarkerMissing
    [i] ->
      let indent = T.takeWhile (\c -> c == ' ' || c == '\t') (ls !! i)
          new = [if T.null (T.strip l) then "" else indent <> l | l <- T.splitOn "\n" (dropFinalNewline snippet)]
       in Right (i + 1, T.intercalate newline (take i ls ++ new ++ drop i ls))
    hits -> Left (MarkerAmbiguous (length hits))
  where
    newline = if "\r\n" `T.isInfixOf` content then "\r\n" else "\n"
    ls = T.splitOn newline content
    dropFinalNewline t = fromMaybe t (T.stripSuffix "\n" t)

-- | Whitespace-insensitive "is this snippet already in the file?"
alreadyPresent :: Text -> Text -> Bool
alreadyPresent snippet content = normalise snippet `T.isInfixOf` normalise content
  where
    normalise = T.unwords . T.words
