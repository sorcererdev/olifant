-- | The cases a `match` at the cursor lacks, from the compiler (spec §4.9):
-- what the next-arm snippet offers. The check runs only where there is a
-- cursor (`actions --at`, `gen --at`), never for a whole file's hints.
module Olifant.Uncovered
  ( uncoveredAt
  , uncoveredQuery
  ) where

import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Holes (placeholderize)
import Olifant.Language (Language (..), languageFor)
import Olifant.Language.Adapter (CheckDef (..), supportOf)
import Olifant.Language.Check (Checker (..), checkScope, describeCheckFailure, runCheckIn)
import Olifant.Language.Diagnostics (CheckReport (..), CompilerSupport (..), MissingPatterns (..))
import Olifant.Query (QueryFailure (..), Record)
import Olifant.Template (Value (..))
import Olifant.Workspace (Workspace (..))
import Text.Regex.TDFA (Regex, makeRegexM, matchTest)

-- | The patterns the match at 1-based @line@ of @file@ lacks, matching @re@
-- (@lineAt@ gives the file's 1-based lines): the latest of the report's
-- sites in that file that reaches the cursor — its arms span the line, or
-- only blank lines lie between its last arm and the line (review I2: code
-- in between is another match). Each is written as the site writes it
-- (@write@: the compiler support's 'supportPattern', given the site and the
-- file's source): rustc's `&E::A` is `E::A`; pyright's `A()` is
-- `command.A()` where the other arms qualify.
uncoveredAt :: (MissingPatterns -> Text -> Text -> Text) -> Text -> FilePath -> Int -> (Int -> Text) -> CheckReport -> [Text]
uncoveredAt write re file line lineAt report =
  case sortOn (Down . fst . missingCase) [m | m <- reportMissing report, missingFile m == file, reaches (missingCase m)] of
    site : _ ->
      let source = T.intercalate "\n" (map lineAt [1 .. snd (missingCase site) + 1])
       in [p | p <- map (T.stripStart . write site source) (missingPatterns site), maybe False (`matchTest` T.unpack p) rx]
    [] -> []
  where
    c0 = line - 1
    reaches (start, end) = start <= c0 && (c0 <= end || all (T.null . T.strip . lineAt) [end + 2 .. line - 1])
    rx = makeRegexM (T.unpack re) :: Maybe Regex

-- | 'uncoveredAt' by running the file's language check over the workspace
-- (holes as their placeholders); records carry the pattern as `name`.
uncoveredQuery :: Monad m => Checker m -> [Language] -> Workspace m -> FilePath -> Int -> Text -> m (Either QueryFailure [Record])
uncoveredQuery checker langs ws file line re = case languageFor langs file of
  Nothing -> pure (Left (QueryFailure ("no language adapter for " <> T.pack file) False))
  Just lang -> case langCheck lang of
    Nothing -> pure (Left (QueryFailure ("the " <> langName lang <> " adapter has no [check]: the compiler cannot say which cases are missing") False))
    Just check -> do
      paths <- listFiles ws "."
      let (scope, wanted) = checkScope lang paths [file]
      texts <- traverse (\p -> (p,) <$> readText ws p) wanted
      let files = M.fromList [(p, t) | (p, Just t) <- texts]
      runCheckIn checker lang scope (placeholderize langs files) >>= \case
        Left f -> pure (Left (QueryFailure ("uncovered " <> re <> ": " <> describeCheckFailure f) False))
        Right report -> do
          ls <- maybe [] (T.splitOn "\n") <$> readText ws file
          let lineAt n = case drop (n - 1) ls of
                l : _ | n >= 1 -> l
                _ -> ""
              write d source p = maybe p (\s -> supportPattern s d source p p) (supportOf check)
          pure (Right [M.fromList [("name", VText p)] | p <- uncoveredAt write re file line lineAt report])
