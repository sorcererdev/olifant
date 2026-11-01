-- | Where a plan's work landed: the commits on the first-parent line of
-- HEAD that touch the plan file, so `plans cost` can say what a PR cost.
-- A squash commit, a merge commit and a plain commit are all landings.
module Olifant.Landings
  ( Landing (..)
  , landingsArgs
  , parseLandings
  , labelOf
  , shortSubject
  , landingFor
  , landingsOf
  ) where

import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Traversable (for)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read (decimal)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- | A commit on the first-parent line that touches a plan: a PR when it has a number.
data Landing = Landing
  { ldCommit :: Text -- ^ full hash
  , ldLabel :: Text -- ^ "#126", or the 7-char hash
  , ldSubject :: Text -- ^ the subject, "(#N)" suffix kept
  , ldAt :: UTCTime -- ^ committer time
  , ldPlan :: FilePath -- ^ root-relative plan path
  }
  deriving (Eq, Show)

-- | The arguments for 'Olifant.Process.gitIn': the first-parent log of HEAD
-- touching PLAN, one line per commit: "<hash>\t<committer epoch>\t<subject>".
landingsArgs :: FilePath -> [String]
landingsArgs plan = ["log", "--first-parent", "--format=%H%x09%ct%x09%s", "HEAD", "--", plan]

-- | The landings in that output, oldest first; a line that is not three
-- tab-separated fields with a numeric epoch is skipped.
parseLandings :: FilePath -> Text -> [Landing]
parseLandings plan = sortOn ldAt . reverse . mapMaybe line . T.lines
  where
    line l = case T.splitOn "\t" l of
      [h, ct, subject] | Right (n, "") <- decimal ct, not (T.null h) ->
        Just (Landing h (labelOf h subject) subject (posixSecondsToUTCTime (fromInteger n)) plan)
      _ -> Nothing

-- | "#N" from a subject ending "(#N)" or starting "Merge pull request #N"; else the short hash.
labelOf :: Text -> Text -> Text
labelOf hash subject = case prNumber subject of
  Just n -> "#" <> n
  Nothing -> T.take 7 hash

prNumber :: Text -> Maybe Text
prNumber s
  | Just rest <- T.stripPrefix "Merge pull request #" s, Just n <- digits rest = Just n
  | Just inner <- T.stripSuffix ")" (T.stripEnd s), (_, tailPart) <- T.breakOnEnd "(#" inner, tailPart /= inner
  , T.isSuffixOf "(#" (T.dropEnd (T.length tailPart) inner), T.all (`elem` ['0' .. '9']) tailPart, not (T.null tailPart) = Just tailPart
  | otherwise = Nothing
  where
    digits r = let d = T.takeWhile (`elem` ['0' .. '9']) r in if T.null d then Nothing else Just d

-- | The subject for a table: the "(#N)" suffix dropped, cut to N chars with "…".
shortSubject :: Int -> Text -> Text
shortSubject width subject
  | T.length bare <= width = bare
  | otherwise = T.take (width - 1) bare <> "…"
  where
    bare = case T.breakOnEnd " (#" (T.stripEnd subject) of
      (pre, rest) | not (T.null pre), Just n <- T.stripSuffix ")" rest, not (T.null n), T.all (`elem` ['0' .. '9']) n -> T.dropEnd 3 pre
      _ -> subject

-- | The first landing (of a list sorted oldest first) at or after a time; Nothing when past the last.
landingFor :: [Landing] -> UTCTime -> Maybe Landing
landingFor landings t = case filter ((>= t) . ldAt) landings of
  l : _ -> Just l
  [] -> Nothing

-- | Each plan's landings through a git function (directory, arguments, stdout):
-- the first failure is the whole result (no repository, no history).
landingsOf :: (FilePath -> [String] -> IO (Either Text Text)) -> FilePath -> [FilePath] -> IO (Either Text [(FilePath, [Landing])])
landingsOf git root plans = sequenceA <$> for plans one
  where
    one plan = fmap (\out -> (plan, parseLandings plan out)) <$> git root (landingsArgs plan)
