-- | What a compiler check reports, and the per-format support the check loop
-- uses to read it and act on it (spec §5.6). Language-neutral.
module Olifant.Language.Diagnostics
  ( MissingPatterns (..)
  , missing
  , CheckReport (..)
  , CompilerSupport (..)
  , declarationAt
  , typeNames
  ) where

import Data.ByteString (ByteString)
import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text)
import Data.Text qualified as T

-- | A `case` (0-based start and end line), the patterns it lacks, and the
-- type of the scrutinee when the compiler names it (GHC does; Elm does not).
data MissingPatterns = MissingPatterns {missingFile :: FilePath, missingCase :: (Int, Int), missingPatterns :: [Text], missingType :: Maybe Text}
  deriving stock (Eq, Show)

missing :: FilePath -> (Int, Int) -> [Text] -> MissingPatterns
missing f c ps = MissingPatterns f c ps Nothing

-- | What a check found: `case`s missing constructors, files with any other
-- problem (path, problem title), and sites the loop cannot extend (with the
-- function they are in). A broken file hides its dependents' missing
-- patterns, so callers must not read "nothing missing" as "complete".
data CheckReport = CheckReport {reportMissing :: [MissingPatterns], reportBroken :: [(FilePath, Text)], reportUnplaced :: [(MissingPatterns, Text)]}
  deriving stock (Eq, Show)

-- | The language half of a compiler check, one per `[check] format`.
data CompilerSupport = CompilerSupport
  { supportFormat :: Text
  , supportParse :: ByteString -> Either Text CheckReport
  , supportPattern :: Text -> Text -> Text -> Text
  -- ^ site source, constructor as the edit names it, reported pattern: the pattern as the site writes it
  , supportVisible :: MissingPatterns -> Text -> Text -> Bool
  -- ^ the diagnostic, site source, constructor: can it appear here?
  , supportFunction :: Text -> Int -> Text
  -- ^ site source, 0-based line: the enclosing top-level declaration
  , supportArmSeparator :: Text
  -- ^ appended to the last alternative before a new one, unless it already
  -- ends with it or ends a block (`,` in Rust; nothing in Elm and Haskell)
  , supportReadsStdout :: Bool
  -- ^ the report is on stdout (cargo), not stderr (elm, GHC)
  }

-- | The declaration a 0-based line belongs to: the first word of the nearest
-- line at or above it that starts in column 0 (not blank, not a comment).
-- True for Elm and Haskell alike.
declarationAt :: Text -> Int -> Text
declarationAt content line =
  case [l | l <- reverse (take (line + 1) (T.lines content)), Just (c, _) <- [T.uncons l], not (isSpace c), not ("--" `T.isPrefixOf` l)] of
    (l : _) -> T.takeWhile (\c -> isAlphaNum c || c == '_' || c == '\'') l
    [] -> "?"

-- | The type names in a printed type, unqualified: `Maybe (M.T, Bool)` →
-- Maybe, T, Bool; `&Failure` → Failure.
typeNames :: Text -> [Text]
typeNames = map (snd . T.breakOnEnd ".") . filter (not . T.null) . T.split (\c -> not (isAlphaNum c || c == '_' || c == '.' || c == '\''))
