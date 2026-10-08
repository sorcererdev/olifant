-- | Strangling a legacy codebase: how far the migration has come — legacy
-- functions now and at the base, the switches (on, off), what a branch
-- extracted and what legacy it added — as a report and the PR line.
module Olifant.Migration
  ( Switch (..)
  , MigrationReport (..)
  , readRegistry
  , switchMarkers
  , unregistered
  , reportLine
  , migrationJson
  , bigChanges
  , readsOrWrites
  , tablesTouched
  , functionText
  ) where

import Olifant.Newness (NewLines (..), newCount, wholeNew)

import Data.Aeson (Value, object, (.=))

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | A switch in the registry, and whether it is on.
data Switch = Switch {swName :: Text, swOn :: Bool}
  deriving stock (Eq, Show)

data MigrationReport = MigrationReport
  { mrLegacyNow :: Int
  , -- | legacy functions at the base (none: no base)
    mrLegacyBase :: Maybe Int
  , mrSwitches :: [Switch]
  , -- | the registry's names at the base (none: no base)
    mrSwitchesBase :: Maybe [Text]
  , -- | legacy definitions new since the base (not delegates)
    mrAddedLegacy :: [Text]
  , -- | switch markers in code that the registry lacks
    mrUnregistered :: [Text]
  }
  deriving stock (Eq, Show)

-- | The registry's entries: `SWITCHES["name"] = True|False` lines, or
-- `"name": True|False,` lines inside `SWITCHES = {…}`.
readRegistry :: Text -> [Switch]
readRegistry text = mapMaybe line (T.lines text) ++ mapMaybe entry (takeWhile (not . closes) (drop 1 (dropWhile (not . opens) (T.lines text))))
  where
    line l = do
      rest <- T.stripPrefix "SWITCHES[\"" (T.strip l)
      let (name, after) = T.breakOn "\"]" rest
      value <- T.stripPrefix "\"]" after >>= T.stripPrefix "=" . T.strip
      pure (Switch name (T.strip value == "True"))
    opens l = "SWITCHES" `T.isPrefixOf` T.stripStart l && "{" `T.isSuffixOf` T.stripEnd l
    closes l = T.strip l == "}"
    entry l = case T.splitOn ":" (T.strip l) of
      [k, v] | Just name <- quoted (T.strip k) -> Just (Switch name (T.strip (T.dropWhileEnd (== ',') (T.strip v)) == "True"))
      _ -> Nothing
    quoted k = T.stripPrefix "\"" k >>= T.stripSuffix "\""

-- | The names in `olifant:switch NAME` comments.
switchMarkers :: Text -> [Text]
switchMarkers text = [T.takeWhile (\c -> c /= ' ' && c /= '\t') (T.strip rest) | l <- T.lines text, (_, rest) <- [T.breakOnEnd "olifant:switch " l], "olifant:switch " `T.isInfixOf` l]

-- | Markers whose names the registry lacks.
unregistered :: [Text] -> [Switch] -> [Text]
unregistered markers switches = [m | m <- markers, m `notElem` map swName switches]

-- | The PR line: what this branch moved, what legacy it added, the legacy
-- left, and the switches.
reportLine :: MigrationReport -> Text
reportLine r = case (mrLegacyBase r, mrSwitchesBase r) of
  (Just before, Just names) ->
    let extracted = [swName s | s <- mrSwitches r, swName s `notElem` names]
        added = mrAddedLegacy r
     in "+" <> count extracted <> " extracted" <> listed extracted <> ", " <> count added <> " legacy function" <> plural added <> " added" <> listed added
          <> ", legacy " <> showT before <> " → " <> showT (mrLegacyNow r) <> " functions; " <> switches
  _ -> "legacy " <> showT (mrLegacyNow r) <> " functions (no base); " <> switches
  where
    switches = "switches: " <> showT (length (filter swOn (mrSwitches r))) <> " on, " <> showT (length (filter (not . swOn) (mrSwitches r))) <> " off"
    count = showT . length
    listed xs = if null xs then "" else " (" <> T.intercalate ", " xs <> ")"
    plural xs = if length xs == 1 then "" else "s"
    showT = T.pack . show

-- | The report as editors and CI read it (`check` JSON, the project model).
migrationJson :: MigrationReport -> Value
migrationJson r =
  object
    [ "legacy_now" .= mrLegacyNow r
    , "legacy_base" .= mrLegacyBase r
    , "switches" .= [object ["name" .= swName s, "on" .= swOn s] | s <- mrSwitches r]
    , "switches_base" .= mrSwitchesBase r
    , "added_legacy" .= mrAddedLegacy r
    , "unregistered" .= mrUnregistered r
    , "line" .= reportLine r
    ]

-- | Legacy functions that are not wholly new but whose new lines reach the
-- threshold (a change that may be new work in disguise): each function's
-- range, with its count of new lines.
bigChanges :: Int -> NewLines -> [((Int, Int), (Int, Int))] -> [(((Int, Int), (Int, Int)), Int)]
bigChanges threshold newness fns = [(fn, n) | fn <- fns, not (wholeNew newness fn), let n = newCount newness fn, n >= threshold]

-- | Whether a legacy function is a command (it writes: SQL that writes, a
-- commit, a save, a module global assigned) or a query (it only reads: SQL
-- that selects, a fetch), or unsure — decision 2's deterministic answer.
readsOrWrites :: Text -> Maybe Text
readsOrWrites text
  | any (`T.isInfixOf` upper) ["INSERT ", "UPDATE ", "DELETE "] || any (`T.isInfixOf` text) [".commit(", ".save(", ".add(", "global "] = Just "command"
  | "SELECT " `T.isInfixOf` upper || any (`T.isInfixOf` text) [".fetch", "db.rows(", "db.row("] = Just "query"
  | otherwise = Nothing
  where
    upper = T.toUpper text

-- | The tables a function's SQL names (FROM, INTO, UPDATE, JOIN …).
tablesTouched :: Text -> [Text]
tablesTouched text = go (T.words (T.map (\c -> if c `elem` ("\"'(),;" :: String) then ' ' else c) text))
  where
    go (w : t : rest) | T.toUpper w `elem` ["FROM", "INTO", "UPDATE", "JOIN"] = T.toLower t : go rest
    go (_ : rest) = go rest
    go [] = []

-- | A function's text by its name (`def NAME(` and its indented body).
functionText :: Text -> Text -> Maybe Text
functionText content name =
  case break isDef (T.lines content) of
    (_, def : rest) ->
      let level = indentOf def
       in Just (T.unlines (def : takeWhile (\l -> T.null (T.strip l) || indentOf l > level) rest))
    _ -> Nothing
  where
    isDef l = ("def " <> name <> "(") `T.isPrefixOf` T.stripStart l
    indentOf = T.length . T.takeWhile (== ' ')

