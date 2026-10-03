module Olifant.Generation.Plan
  ( Plan (..)
  , PlannedEdit (..)
  , Site (..)
  , planned
  , EditAction (..)
  , SkipReason (..)
  , Conflict (..)
  , HoleStatus (..)
  , PlannedHole (..)
  , hasConflicts
  , describeConflict
  , conflictToolMissing
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Olifant.Anchor (Miss (..), describeMisses)
import Olifant.Generator (GeneratorId)
import Olifant.Holes (HoleSite (..))

data Plan = Plan
  { planGenerator :: GeneratorId
  , planSteps :: [(Text, Map Text Text)]
  , planNow :: UTCTime
  , planEdits :: [PlannedEdit]
  , planConflicts :: [Conflict]
  , -- | problems that did not stop the plan but mean it may be incomplete
    planWarnings :: [Text]
  , planWrites :: Map FilePath Text
  , -- | every hole this plan wrote, and whether it was filled (spec §6.4)
    planHoles :: [PlannedHole]
  , -- | each written file's text before this plan (Nothing: a new file), for editors
    planBefore :: Map FilePath (Maybe Text)
  }
  deriving stock (Eq, Show)

-- | 'editAnchor': which `at` anchor placed an insert (spec §11.2
-- `anchor_used`); 'editSite': for a compiler-found branch, where and whether
-- it was left as a todo (spec §5.6).
data PlannedEdit = PlannedEdit
  { editStep :: [Text]
  , editNumber :: Int
  , editPath :: FilePath
  , editAction :: EditAction
  , editAnchor :: Maybe Text
  , editSite :: Maybe Site
  }
  deriving stock (Eq, Show)

data Site = Site {siteFile :: FilePath, siteFunction :: Text, sitePattern :: Text, siteTodo :: Bool}
  deriving stock (Eq, Show)

planned :: [Text] -> Int -> FilePath -> EditAction -> PlannedEdit
planned st n p a = PlannedEdit st n p a Nothing Nothing

-- | 'HoleFilled': by which model, after how many attempts; 'HoleUnfilled':
-- why it keeps its placeholder; 'HoleSkipped': why it was not tried.
data HoleStatus = HoleFilled Text Int | HoleUnfilled Text | HoleSkipped Text
  deriving stock (Eq, Show)

data PlannedHole = PlannedHole {phSite :: HoleSite, phStatus :: HoleStatus, phFill :: Maybe Text}
  deriving stock (Eq, Show)

-- | 'Conflicted': this edit produced a conflict (see 'planConflicts').
-- | 'WillRefresh': an owned region (name, lines added, lines removed, new file content).
data EditAction = WillCreate Text | WillOverwrite Text | WillInsert Int Text | WillRefresh Text Int Int Text | Skipped SkipReason | Conflicted
  deriving stock (Eq, Show)

-- | 'UnlessMatched': `unless.exists` found this file; 'UnlessFound': a line
-- (1-based) of the edit's own target matched `unless.matches`.
data SkipReason = AlreadyExists | AlreadyPresent | UnchangedContent | UnlessMatched FilePath | UnlessFound FilePath Int
  deriving stock (Eq, Show)

data Conflict
  = TargetExists Int FilePath
  | InsertTargetMissing Int FilePath
  | AnchorUnresolved Int FilePath [Miss]
  | -- | step, why, whether a tool is missing
    CompleteFailed [Text] Text Bool
  | -- | region, file, why
    RegionBroken Text FilePath Text
  | UnsafePath Int FilePath
  | RequirementUnmet [Text] [FilePath] (Maybe Text)
  | -- | edit, path, why it cannot be written (a directory, non-text file, or a file where a directory must be)
    TargetUnusable Int FilePath Text
  | -- | a hole that cannot be placed (no adapter, unknown kind)
    HoleProblem Text
  deriving stock (Eq, Show)

hasConflicts :: Plan -> Bool
hasConflicts = not . null . planConflicts

describeConflict :: Conflict -> Text
describeConflict = \case
  TargetExists n p -> edit n <> T.pack p <> " already exists (on_exists = \"error\")"
  InsertTargetMissing n p -> edit n <> "cannot insert into " <> T.pack p <> ": it does not exist"
  AnchorUnresolved n p misses -> edit n <> "cannot place text in " <> T.pack p <> ": " <> describeMisses misses
  CompleteFailed step why _ -> T.intercalate " › " step <> ": cannot complete cases: " <> why
  RegionBroken _ _ why -> why
  UnsafePath n p -> edit n <> "path " <> T.pack p <> " is outside the project"
  TargetUnusable n p why -> edit n <> "cannot write " <> T.pack p <> ": " <> why
  HoleProblem why -> "hole: " <> why
  RequirementUnmet step ps hint -> T.intercalate " › " step <> ": required files missing: " <> T.intercalate ", " (map T.pack ps) <> maybe "" (". " <>) hint
  where
    edit n = "edit " <> T.pack (show n) <> ": "

-- | A conflict caused only by a missing tool (pack tests skip rather than fail).
conflictToolMissing :: Conflict -> Bool
conflictToolMissing = \case
  AnchorUnresolved _ _ misses -> not (null misses) && any missTool misses
  CompleteFailed _ _ tool -> tool
  _ -> False
