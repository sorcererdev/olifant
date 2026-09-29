module Olifant.Generation.Plan
  ( Plan (..)
  , PlannedEdit (..)
  , EditAction (..)
  , SkipReason (..)
  , Conflict (..)
  , hasConflicts
  , describeConflict
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Olifant.Generator (GeneratorId)

data Plan = Plan
  { planGenerator :: GeneratorId
  , planSteps :: [(Text, Map Text Text)]
  , planNow :: UTCTime
  , planEdits :: [PlannedEdit]
  , planConflicts :: [Conflict]
  , planWrites :: Map FilePath Text
  }
  deriving stock (Eq, Show)

data PlannedEdit = PlannedEdit {editStep :: [Text], editNumber :: Int, editPath :: FilePath, editAction :: EditAction}
  deriving stock (Eq, Show)

-- | 'Conflicted': this edit produced a conflict (see 'planConflicts').
data EditAction = WillCreate Text | WillOverwrite Text | WillInsert Int Text | Skipped SkipReason | Conflicted
  deriving stock (Eq, Show)

data SkipReason = AlreadyExists | AlreadyPresent | UnchangedContent
  deriving stock (Eq, Show)

data Conflict
  = TargetExists Int FilePath
  | InsertTargetMissing Int FilePath
  | MarkerNotFound Int FilePath Text
  | MarkerRepeated Int FilePath Text Int
  | UnsafePath Int FilePath
  | RequirementUnmet [Text] [FilePath] (Maybe Text)
  | -- | edit, path, why it cannot be written (a directory, non-text file, or a file where a directory must be)
    TargetUnusable Int FilePath Text
  deriving stock (Eq, Show)

hasConflicts :: Plan -> Bool
hasConflicts = not . null . planConflicts

describeConflict :: Conflict -> Text
describeConflict = \case
  TargetExists n p -> edit n <> T.pack p <> " already exists (on_exists = \"error\")"
  InsertTargetMissing n p -> edit n <> "cannot insert into " <> T.pack p <> ": it does not exist"
  MarkerNotFound n p m -> edit n <> "marker `" <> m <> "` not found in " <> T.pack p
  MarkerRepeated n p m k -> edit n <> "marker `" <> m <> "` appears " <> T.pack (show k) <> " times in " <> T.pack p <> "; it must appear once"
  UnsafePath n p -> edit n <> "path " <> T.pack p <> " is outside the project"
  TargetUnusable n p why -> edit n <> "cannot write " <> T.pack p <> ": " <> why
  RequirementUnmet step ps hint -> T.intercalate " › " step <> ": required files missing: " <> T.intercalate ", " (map T.pack ps) <> maybe "" (". " <>) hint
  where
    edit n = "edit " <> T.pack (show n) <> ": "
