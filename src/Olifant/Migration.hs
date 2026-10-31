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
  , Strangler (..)
  , Extract (..)
  , Owner (..)
  , parseStrangler
  , extractKind
  , extractDecision
  , tablesTouched
  , functionText
  , switchPath
  , codeOf
  ) where

import Olifant.Newness (NewLines (..), newCount, wholeNew)

import Data.Aeson (Value, object, (.=))

import Control.Monad ((>=>))
import Data.Array (elems)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Toml qualified as Toml
import Text.Regex.TDFA (AllTextSubmatches (..), CompOption (..), ExecOption (..), Regex, defaultCompOpt, defaultExecOpt, makeRegexOptsM, matchAllText, matchM, matchTest)

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

-- | The registry's entries, as the strangler pack's `[switches]` reads
-- them: lines its `line` expressions match anywhere (group 1 the name,
-- group 2 the value), and lines its `block` `line` expressions match
-- between the first line `open` matches and the next `close` matches; on
-- when the value is `on`. COMMENT: the registry's language's comment marker
-- (a trailing comment is not part of a value). Each line is stripped first.
readRegistry :: Strangler -> Text -> Text -> [Switch]
readRegistry st comment text = mapMaybe (entry (stSwitchLine st)) ls ++ inBlock
  where
    ls = map (T.strip . cut) (T.lines text)
    cut l = if T.null comment then l else fst (T.breakOn comment l)
    inBlock = case stSwitchBlock st of
      Nothing -> []
      Just (open, close, lineRes) -> mapMaybe (entry lineRes) (takeWhile (not . matches close) (drop 1 (dropWhile (not . matches open) ls)))
    entry res l = listToMaybe [Switch name (T.strip value == stSwitchOn st) | re <- res, Just (name : value : _) <- [groupsOf re l]]

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

-- | Which extract a legacy function reads as (decision 2's deterministic
-- answer): the first of EXTRACTS (the strangler's `[[extract.into]]`, in
-- order) one of whose `signs` its code matches, its comments cut (COMMENT,
-- the language's marker: `# update the cache` is prose); none: unsure.
extractKind :: Text -> [Extract] -> Text -> Maybe Text
extractKind comment extracts text0 = exName <$> find (\e -> any (`signIn` text) (exSigns e)) extracts
  where
    text = T.unlines (map (codeOf comment) (T.lines text0))

-- | Decision 2 as the decider is asked it: its id (the extracts' names,
-- joined by @-or-@), its question and its options (each name and what it
-- means).
extractDecision :: [Extract] -> (Text, Text, [(Text, Text)])
extractDecision extracts =
  ( T.intercalate "-or-" (map exName extracts)
  , "Is this function a " <> T.intercalate " or a " [exName e <> " (" <> exMeans e <> ")" | e <- extracts]
  , [(exName e, exMeans e) | e <- extracts]
  )

-- | A line less its comment (COMMENT outside a string); none: the line.
codeOf :: Text -> Text -> Text
codeOf comment l
  | T.null comment = l
  | otherwise = T.pack (go Nothing (T.unpack l))
  where
    marker = T.unpack comment
    go _ [] = []
    go Nothing cs@(c : rest)
      | take (length marker) cs == marker = []
      | c `elem` ("\"'" :: String) = c : go (Just c) rest
      | otherwise = c : go Nothing rest
    go (Just q) ('\\' : c : cs) = '\\' : c : go (Just q) cs
    go (Just q) (c : cs) | c == q = c : go Nothing cs
    go st (c : cs) = c : go st cs

-- | The tables a function's code names, by the strangler's `[extract.owner]
-- tables` expressions (each match's last group, lowercased).
tablesTouched :: [Text] -> Text -> [Text]
tablesTouched res text = [T.toLower (T.pack (fst (last (elems m)))) | re <- res, Just rx <- [compiled re], m <- matchAllText rx (T.unpack text)]

-- | A unit's text by its name: the first line one of the language's
-- `[unit] name` expressions (NAMERS) names NAME, and the lines after it that
-- are blank or deeper.
functionText :: [Text] -> Text -> Text -> Maybe Text
functionText namers content name =
  case break isDef (T.lines content) of
    (_, def : rest) ->
      let level = indentOf def
       in Just (T.unlines (def : takeWhile (\l -> T.null (T.strip l) || indentOf l > level) rest))
    _ -> Nothing
  where
    isDef l = any (\re -> lastGroup re l == Just name) namers
    indentOf = T.length . T.takeWhile (== ' ')

-- | A switched function's new path (the strangler's retire): the lines of
-- the block that opens at the first line the strangler's `[switches] guard`
-- matches (stripped), a level less indented; none without a guard.
switchPath :: Maybe Text -> Text -> Text
switchPath guard found = case guard of
  Nothing -> ""
  Just re ->
    let ls = T.lines found
        indentOf = T.length . T.takeWhile (== ' ')
     in case break (matches re . T.stripStart) ls of
          (_, opener : rest) ->
            let level = indentOf opener
                body = takeWhile (\l -> T.null (T.strip l) || indentOf l > level) rest
                inner = case filter (not . T.null . T.strip) body of
                  l : _ -> indentOf l - level
                  [] -> 0
             in T.intercalate "\n" [T.drop inner l | l <- body, not (T.null (T.strip l))]
          _ -> ""

-- | A strangler pack's `migrate.toml` (book chapter 17), what strangling
-- reads from the pack rather than the engine. 'stPack' names the pack (its
-- generator ids are qualified by it).
data Strangler = Strangler
  { stPack :: Text
  , stBugFixOnly :: Maybe Text
  -- ^ `bug_fix_only`: the rule that keeps legacy bug-fix-only. Its allow
  -- comment marks a delegate (not counted as legacy added), and a decider's
  -- "new behaviour" turns a new-work hint into its finding
  , stSwitches :: Maybe Text
  -- ^ `[switches] registry`: a glob; the registry, when `[migrate] switches`
  -- names none, is the first file it matches outside the legacy code
  , stSwitchOn :: Text
  -- ^ `[switches] on`: the value that means on
  , stSwitchLine :: [Text]
  -- ^ `[switches] line`: expressions over a registry line, group 1 the name, group 2 the value
  , stSwitchBlock :: Maybe (Text, Text, [Text])
  -- ^ `[switches.block]`: `open`, `close` and `line`, for entries only inside a block
  , stSwitchGuard :: Maybe Text
  -- ^ `[switches] guard`: the line opening a switched function's new path
  , stExtract :: [Extract]
  -- ^ `[[extract.into]]`: the extract generators new work in disguise offers, in order
  , stExtractFunction :: Text
  -- ^ `[extract] function`: the input every extract generator takes for the legacy function's name
  , stExtractFile :: Text
  -- ^ `[extract] file`: the input for its file
  , stOwner :: Maybe Owner
  -- ^ `[extract.owner]`: decision 3, which owner the work belongs to
  }
  deriving stock (Eq, Show)

-- | `[extract.owner]`: 'owInput' (`input`), the extract generators' input
-- whose choices are the owners; 'owModels' (`models`), a glob template over
-- the generator's inputs (the owner among them) whose files' stems are an
-- owner's models; 'owTables' (`tables`), expressions over the function's
-- code whose last group is a table it touches. The owner one of whose
-- models a touched table names (singular or plural) is suggested.
data Owner = Owner {owInput :: Text, owModels :: Text, owTables :: [Text]}
  deriving stock (Eq, Show)

-- | One `[[extract.into]]`: 'exGenerator' (`generator`, qualified by the
-- pack), 'exName' (`name`) and 'exMeans' (`means`), how decision 2 names it,
-- and 'exSigns' (`signs`), expressions over the function's code that make
-- it this one (a leading @(?i)@: any case).
data Extract = Extract {exGenerator :: Text, exName :: Text, exMeans :: Text, exSigns :: [Text]}
  deriving stock (Eq, Show)

-- | PACK's `migrate.toml`, parsed.
parseStrangler :: Text -> Text -> Either Text Strangler
parseStrangler pack src = do
  top <- Toml.parseToml src
  Toml.onlyKeys ["bug_fix_only", "switches", "extract"] top
  bug <- Toml.optional "bug_fix_only" Toml.text top
  sw <- fromMaybe mempty <$> Toml.optional "switches" Toml.table top
  (registry, on, line, block, guard) <- Toml.inKey "switches" $ do
    Toml.onlyKeys ["registry", "on", "line", "block", "guard"] sw
    (,,,,)
      <$> Toml.optional "registry" Toml.text sw
      <*> (fromMaybe "true" <$> Toml.optional "on" Toml.text sw)
      <*> (fromMaybe [] <$> Toml.optional "line" (Toml.list expression) sw)
      <*> Toml.optional "block" (Toml.table >=> \b -> Toml.inKey "block" (Toml.onlyKeys ["open", "close", "line"] b >> ((,,) <$> Toml.required "open" expression b <*> Toml.required "close" expression b <*> Toml.required "line" (Toml.list expression) b))) sw
      <*> Toml.optional "guard" expression sw
  ex <- fromMaybe mempty <$> Toml.optional "extract" Toml.table top
  (into, function, file, owner) <- Toml.inKey "extract" $ do
    Toml.onlyKeys ["into", "function", "file", "owner"] ex
    (,,,)
      <$> (fromMaybe [] <$> Toml.optional "into" (Toml.list (Toml.table >=> extractIn)) ex)
      <*> (fromMaybe "function" <$> Toml.optional "function" Toml.text ex)
      <*> (fromMaybe "file" <$> Toml.optional "file" Toml.text ex)
      <*> Toml.optional "owner" (Toml.table >=> \o -> Toml.inKey "owner" (Toml.onlyKeys ["input", "models", "tables"] o >> (Owner <$> Toml.required "input" Toml.text o <*> Toml.required "models" Toml.text o <*> (fromMaybe [] <$> Toml.optional "tables" (Toml.list expression) o)))) ex
  pure (Strangler pack bug registry on line block guard into function file owner)
  where
    expression = Toml.text >=> \re -> if isJust (compiled re) then Right re else Left ("not a valid expression: " <> re)
    extractIn t = Toml.inKey "into" $ do
      Toml.onlyKeys ["generator", "name", "means", "signs"] t
      Extract
        <$> ((\g -> pack <> "/" <> g) <$> Toml.required "generator" Toml.text t)
        <*> Toml.required "name" Toml.text t
        <*> (fromMaybe "" <$> Toml.optional "means" Toml.text t)
        <*> (fromMaybe [] <$> Toml.optional "signs" (Toml.list expression) t)

-- | An expression, compiled; a leading @(?i)@ makes it match any case.
compiled :: Text -> Maybe Regex
compiled re = case T.stripPrefix "(?i)" re of
  Just rest -> makeRegexOptsM defaultCompOpt {caseSensitive = False} defaultExecOpt (T.unpack rest)
  Nothing -> makeRegexOptsM defaultCompOpt defaultExecOpt (T.unpack re)

matches :: Text -> Text -> Bool
matches re l = maybe False (`matchTest` T.unpack l) (compiled re)

signIn :: Text -> Text -> Bool
signIn = matches

-- | An expression's groups in a line (none: no match).
groupsOf :: Text -> Text -> Maybe [Text]
groupsOf re l = drop 1 <$> allGroups re l

-- | What an expression names in a line: its last group (the whole match
-- when it has none), stripped.
lastGroup :: Text -> Text -> Maybe Text
lastGroup re l = T.strip . last <$> allGroups re l

allGroups :: Text -> Text -> Maybe [Text]
allGroups re l = do
  rx <- compiled re
  AllTextSubmatches gs <- matchM rx (T.unpack l) :: Maybe (AllTextSubmatches [] String)
  if null gs then Nothing else pure (map T.pack gs)
