-- | The cases a `match` at the cursor lacks, from the compiler (spec §4.9):
-- what the next-arm snippet offers. The check runs only where there is a
-- cursor (`actions --at`, `gen --at`), never for a whole file's hints.
module Olifant.Uncovered
  ( uncoveredAt
  , uncoveredQuery
  ) where

import Control.Applicative ((<|>))
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Map.Strict qualified as M
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Glob (matchesGlob)
import Olifant.Holes (placeholderize)
import Olifant.Language (Language (..), languageFor)
import Olifant.Language.Adapter (CheckDef (..))
import Olifant.Language.Check (Checker (..), describeCheckFailure)
import Olifant.Language.Diagnostics (CheckReport (..), MissingPatterns (..))
import Olifant.Query (QueryFailure (..), Record)
import Olifant.Template (Value (..))
import Olifant.Workspace (Workspace (..), pruned)
import System.FilePath (splitDirectories)
import Text.Regex.TDFA (Regex, makeRegexM, matchTest)

-- | The patterns the match at 1-based @line@ of @file@ lacks, matching @re@:
-- of the report's sites in that file starting at or above the line, the
-- latest (a blank line after the last arm is still in its match).
uncoveredAt :: Text -> FilePath -> Int -> CheckReport -> [Text]
uncoveredAt re file line report =
  case sortOn (Down . fst . missingCase) [m | m <- reportMissing report, missingFile m == file, fst (missingCase m) <= line - 1] of
    site : _ -> [p | p <- map unref (missingPatterns site), maybe False (`matchTest` T.unpack p) rx]
    [] -> []
  where
    rx = makeRegexM (T.unpack re) :: Maybe Regex
    -- rustc: `&E::A` for a match on a reference; the arm is `E::A`
    unref p = T.stripStart (fromMaybe p (T.stripPrefix "&mut " p <|> T.stripPrefix "&" p))

-- | 'uncoveredAt' by running the file's language check over the workspace
-- (holes as their placeholders); records carry the pattern as `name`.
uncoveredQuery :: Monad m => Checker m -> [Language] -> Workspace m -> FilePath -> Int -> Text -> m (Either QueryFailure [Record])
uncoveredQuery checker langs ws file line re = case languageFor langs file of
  Nothing -> pure (Left (QueryFailure ("no language adapter for " <> T.pack file) False))
  Just lang -> case langCheck lang of
    Nothing -> pure (Left (QueryFailure ("the " <> langName lang <> " adapter has no [check]: the compiler cannot say which cases are missing") False))
    Just check -> do
      paths <- listFiles ws "."
      let globs = langFiles lang ++ checkContext check
          wanted p = not (any (`elem` pruned) (splitDirectories p)) && any (`matchesGlob` p) globs
      texts <- traverse (\p -> (p,) <$> readText ws p) (filter wanted paths)
      let files = M.fromList [(p, t) | (p, Just t) <- texts]
      runCheck checker lang (placeholderize langs files) >>= \case
        Left f -> pure (Left (QueryFailure ("uncovered " <> re <> ": " <> describeCheckFailure f) False))
        Right report -> pure (Right [M.fromList [("name", VText p)] | p <- uncoveredAt re file line report])
