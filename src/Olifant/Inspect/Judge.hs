-- | Inspect's judging steps after the find call: place a finding (in a changed
-- unit, or elsewhere), match it against the store (Jev's same-problem
-- question picks the stored finding; the design's step-4 table says what a
-- match does), Jev's nitpick filter, and verify (citations exist, then the
-- plans' claim check for bugs and assumptions).
module Olifant.Inspect.Judge
  ( place
  , readablePath
  , resolveRule
  , Match (..)
  , matchStored
  , samePairs
  , sameProblem
  , sameCut
  , findingText
  , nitpick
  , keepOf
  , citedCode
  , citationsOk
  , claimVerdict
  ) where

import Control.Applicative ((<|>))
import Data.List (isPrefixOf, sortOn)
import System.FilePath (isAbsolute, splitDirectories, takeFileName)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.App (Project)
import Olifant.Catalog (PlansConfig)
import Olifant.Decide (Answer (No, Yes), Confidence (..), Question (..), Shape (..), noul)
import Olifant.Server.Checker (CheckReply (..), Citation (..), Confirmed (..), Judgement (..), Print, confirm, fences)
import Olifant.Server.Claims (CheckRecord (..), Failure)
import Olifant.Server.State (ServerState)
import Olifant.Server.Unit (Named (..), fingerprint)
import Olifant.Inspect.Find (RawFinding (..))
import Olifant.Inspect.Steering (Steering (..), ruleText, rulesSection)
import Olifant.Inspect.Store (Place (..), Status (..), InspectFinding (..), InspectStore (..))

-- | Where a new finding is, its rule (dropped when not a heading in inspect.md),
-- and its first status: Open (provisional) inside a changed unit of the
-- inspected file, else Elsewhere. FILE is the inspected file, TEXT its text now,
-- UNITS its units, CHANGED the changed unit names.
place :: Steering -> FilePath -> Text -> [Named] -> Set Text -> RawFinding -> (Place, Maybe Text, Status)
place st file text units changed f = (Place (rfFile f) (namedName <$> unit) (rfLines f) fp (fst . namedLines <$> unit), rule, status)
  where
    (first, final) = rfLines f
    sameFile = rfFile f == file
    -- 1a's rule: in a changed unit when the range overlaps one (the innermost
    -- holding the first line first); else the unit holding the first line
    overlapping = [n | n <- units, namedName n `S.member` changed, let (a, z) = namedLines n, a <= final, first <= z]
    holding = [n | n <- units, let (a, z) = namedLines n, a <= first, first <= z]
    unit
      | not sameFile = Nothing
      | otherwise = listToMaybe (sortOn (\n -> (not (n `elem` holding), width n)) overlapping) <|> listToMaybe (sortOn width holding)
    width n = let (a, z) = namedLines n in z - a
    fp = (\n -> fingerprint text (namedLines n)) <$> unit
    rule = rfRule f >>= resolveRule st
    status = if sameFile && not (null overlapping) then Open else Elsewhere

-- | A path inspect may read and send on (to Jev, to the finder): relative, inside
-- the root, not `.env*`, not under a fence (the plans' `not_evidence`, the
-- plans directory, `.olifant`).
readablePath :: [FilePath] -> FilePath -> Bool
readablePath fenced p =
  not (null parts)
    && not (isAbsolute p)
    && ".." `notElem` parts
    && not (".env" `isPrefixOf` takeFileName p)
    && not (any (\f -> take (length (splitDirectories f)) parts == splitDirectories f) fenced)
  where
    parts = filter (/= ".") (splitDirectories p)

-- | The heading a cited rule names, matched as 1a did: ignoring case, the
-- cited text inside the heading or the heading inside it; the first wins.
resolveRule :: Steering -> Text -> Maybe Text
resolveRule st cited
  | T.null c = Nothing
  | otherwise = listToMaybe [name | (name, _) <- stRules st, let h = T.toLower name, c `T.isInfixOf` h || h `T.isInfixOf` c]
  where
    c = T.toLower (T.strip cited)

data Match = New | Merge | Drop
  deriving stock (Eq, Show)

-- | What a new finding does when Jev says it is the same problem as a
-- stored one (the design's step-4 table); UNIT is the new finding's unit name.
matchStored :: InspectFinding -> InspectFinding -> Text -> Match
matchStored _ stored unit = case vfStatus stored of
  Open -> Merge
  Waiting -> Merge
  Withheld -> Merge
  Refuted -> Merge
  Unsure -> Merge
  Elsewhere -> Merge
  DismissedHere -> if plUnit (vfPlace stored) == Just unit then Drop else New
  DismissedEverywhere -> Drop
  Resolved -> New
  Gone -> New

-- | The stored findings worth asking Jev about: same file, same rule, or a
-- cited line in common.
samePairs :: InspectStore -> RawFinding -> Place -> [InspectFinding]
samePairs vs f pl = filter near (vsFindings vs)
  where
    near g =
      plFile (vfPlace g) == plFile pl
        || (isJust (rfRule f) && vfRule g == rfRule f)
        || or [p == q && overlaps a b | (p, a) <- rfEvidence f, (q, b) <- vfEvidence g]
    overlaps (a, z) (b, y) = a <= y && b <= z

-- | P(same) at or past this merges (1a's cut).
sameCut :: Double
sameCut = 0.5

-- | A finding as 1a showed it to Jev: `kind at file:line; rule: X; statement`.
findingText :: Text -> FilePath -> Int -> Maybe Text -> Text -> Text
findingText kind file line rule statement =
  kind <> " at " <> T.pack file <> ":" <> T.pack (show line) <> "; rule: " <> fromMaybe "none" rule <> "; " <> statement

-- | Jev's same-problem question, worded as 1a measured it.
sameProblem :: Text -> Text -> Text -> Text -> Question
sameProblem a b codeA codeB =
  (noul "same" "Do these two code review findings describe the same problem?" [("finding A", a), ("finding B", b), ("code at A", codeA), ("code at B", codeB)])
    { qShape = Noul (Just ("one fix would fix both: the same defect or rule break at the same code, however it is worded", "they need separate fixes: different defects, or the same kind of defect at different code"))
    , qUnsure = False
    }

-- | Jev's nitpick question (1a's wording), with the cited rule's text, or
-- the whole Rules section when the finding cites none; inspect.md's two
-- sections replace the criteria.
nitpick :: Steering -> RawFinding -> Text -> Question
nitpick st f code =
  (noul "nitpick" "Is this code review finding a nitpick?" ctx)
    { qShape = Noul (Just (fromMaybe nitpickTrue (stNitpick st), fromMaybe nitpickFalse (stWorth st)))
    , qUnsure = False
    }
  where
    ctx =
      [ ("finding kind", rfKind f)
      , ("finding", rfStatement f)
      , ("place", T.pack (rfFile f) <> ":" <> T.pack (show (fst (rfLines f))))
      , ("cited code", code)
      ]
        ++ case rfRule f >>= ruleText st of
          Just r -> [("project rule", r)]
          Nothing -> [("project rules", rulesSection st)]
    nitpickTrue = "it is about taste or polish: naming, wording, formatting, style, a small optional refactor, or speculative future-proofing; nothing would go wrong if it were ignored"
    nitpickFalse = "ignoring it leaves wrong behaviour, a failure, lost data, a security hole, or a real maintenance cost"

-- | 1 - P(nitpick); an unsure answer keeps the finding (truth is checked after).
keepOf :: (Answer, [(Text, Double)]) -> Double
keepOf (a, _) = case a of
  Yes (Confidence c) -> 1 - c
  No (Confidence c) -> c
  Yes Unknown -> 0
  No Unknown -> 1
  _ -> 1

-- | Eight lines either side of a line, numbered as 1a showed them.
citedCode :: Text -> Int -> Text
citedCode text n =
  let ls = T.lines text
      lo = max 1 (n - 8)
      hi = min (length ls) (n + 8)
   in T.intercalate "\n" [T.justifyRight 4 ' ' (T.pack (show i)) <> " " <> l | (i, l) <- zip [lo .. hi] (drop (lo - 1) ls)]

-- | Each evidence range checked by its first line through 'confirm' (so an
-- end line past the file's end is clamped, ruling 5): the prints, or Left
-- when a citation is not found or there is none.
citationsOk :: ServerState -> Project -> PlansConfig -> RawFinding -> IO (Either Text [Print])
citationsOk s p cfg f
  | null (rfEvidence f) = pure (Left "no evidence cited")
  | otherwise = do
      c <- confirm s p (fences cfg) (CheckReply JRight "" [Citation path Nothing (Just a) | (path, (a, _)) <- rfEvidence f] Nothing Nothing [])
      pure (if cfJudgement c == JRight then Right (cfPrints c) else Left "cited code not found")

-- | The claim check's verdict as a status: right shows, wrong is refuted,
-- cannot tell is unsure, a check that did not finish waits.
claimVerdict :: Either Failure CheckRecord -> Status
claimVerdict = \case
  Right r | cfJudgement (cjConfirmed r) == JRight -> Open
  Right r | cfJudgement (cjConfirmed r) == JWrong -> Refuted
  Right _ -> Unsure
  -- the check did not finish (cancelled, failed): verified again later
  Left _ -> Waiting
