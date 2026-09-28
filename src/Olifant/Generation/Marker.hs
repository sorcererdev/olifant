module Olifant.Generation.Marker
  ( MarkerProblem (..)
  , insertBeforeMarker
  , markerBlock
  , alreadyPresent
  ) where

import Data.Char (isAlphaNum)
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

-- | The block a marker closes: the lines above it that are blank or indented
-- at least as deeply as the marker, up to the first shallower line. For a
-- cabal module list that is the list; for a Rust impl marker it is the body.
-- Already-present checks look only here (spec §5.4, "within the anchor's region").
markerBlock :: Text -> Text -> Either MarkerProblem Text
markerBlock marker content =
  case [i | (i, l) <- zip [0 ..] ls, T.strip l == T.strip marker] of
    [] -> Left MarkerMissing
    [i] ->
      let depth = indentOf (ls !! i)
          inBlock l = T.null (T.strip l) || indentOf l >= depth
       in Right (T.unlines (reverse (takeWhile inBlock (reverse (take i ls)))))
    hits -> Left (MarkerAmbiguous (length hits))
  where
    ls = T.lines (T.replace "\r\n" "\n" content)
    indentOf = T.length . T.takeWhile (\c -> c == ' ' || c == '\t')

-- | Whitespace-insensitive "is this snippet already in the file?". A match
-- only counts if it doesn't continue an identifier on either side, so
-- `Olifant.Gen` is not "present" in a file containing `Olifant.Generation`.
alreadyPresent :: Text -> Text -> Bool
alreadyPresent snippet content = T.null needle || any bounded (T.breakOnAll needle haystack)
  where
    normalise = T.unwords . T.words
    needle = normalise snippet
    haystack = normalise content
    bounded (before, rest) =
      not (joins (lastChar before) (firstChar needle))
        && not (joins (lastChar needle) (firstChar (T.drop (T.length needle) rest)))
    joins a b = maybe False isIdent a && maybe False isIdent b
    isIdent c = isAlphaNum c || c == '_'
    firstChar = fmap fst . T.uncons
    lastChar = fmap snd . T.unsnoc
