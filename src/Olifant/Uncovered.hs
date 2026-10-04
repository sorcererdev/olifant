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

-- | The patterns the match at 1-based @line@ of @file@ lacks, matching @re@
-- (@lineAt@ gives the file's 1-based lines): the latest of the report's
-- sites in that file that reaches the cursor — its arms span the line, or
-- only blank lines lie between its last arm and the line (review I2: code
-- in between is another match).
uncoveredAt :: Text -> FilePath -> Int -> (Int -> Text) -> CheckReport -> [Text]
uncoveredAt re file line lineAt report =
  case sortOn (Down . fst . missingCase) [m | m <- reportMissing report, missingFile m == file, reaches (missingCase m)] of
    site : _ -> [p | p <- map unref (missingPatterns site), maybe False (`matchTest` T.unpack p) rx]
    [] -> []
  where
    c0 = line - 1
    reaches (start, end) = start <= c0 && (c0 <= end || all (T.null . T.strip . lineAt) [end + 2 .. line - 1])
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
        Right report -> do
          ls <- maybe [] (T.splitOn "\n") <$> readText ws file
          let lineAt n = case drop (n - 1) ls of
                l : _ | n >= 1 -> l
                _ -> ""
          pure (Right [M.fromList [("name", VText p)] | p <- uncoveredAt re file line lineAt report])
