-- | Vet's find call: one `claude -p` over a pack of changed, finished units,
-- with the project's steering file, the rule findings already reported and
-- the open findings to answer ("still holds" or "no longer holds"). The
-- prompt is 1a's (`find/prompt.py`), word for word.
module Olifant.Vet.Find
  ( FindUnit (..)
  , FindAsk (..)
  , RawFinding (..)
  , FindReply (..)
  , findKinds
  , findPrompt
  , parseFind
  , findKey
  , Finder
  , liveFinder
  , recordedFinder
  , packUnits
  , findTimeout
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (withMVar)
import Data.Aeson (Value (..), decodeStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.List (sort)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger, toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Olifant.Catalog (PlansConfig (..), VetConfig (..))
import Olifant.Process (Cancel)
import Olifant.Server.Checker (readReplies, recordLock, resultOf)
import Olifant.Server.Errand (RunFailure (..), RunResult, Status (..), Verb (..), runClaude)
import Olifant.Vet.Store (Place (..), VetFinding (..))

data FindUnit = FindUnit {fuFile :: FilePath, fuName :: Text, fuLines :: (Int, Int), fuHeader :: Maybe Text, fuText :: Text}
  deriving stock (Eq, Show)

data FindAsk = FindAsk {faUnits :: [FindUnit], faDiff :: Text, faSteering :: Text, faReported :: [Text], faOpen :: [VetFinding]}
  deriving stock (Eq, Show)

data RawFinding = RawFinding {rfKind :: Text, rfFile :: FilePath, rfLines :: (Int, Int), rfStatement :: Text, rfRule :: Maybe Text, rfEvidence :: [(FilePath, (Int, Int))]}
  deriving stock (Eq, Show)

-- | The findings, and the answers on open findings: (id, still holds, why).
data FindReply = FindReply {frFindings :: [RawFinding], frOpen :: [(Text, Bool, Text)]}
  deriving stock (Eq, Show)

findKinds :: [Text]
findKinds = ["bug", "assumption", "convention", "design"]

-- | Seconds a find call may take (1a's cap).
findTimeout :: Int
findTimeout = 420

findPrompt :: FindAsk -> Text
findPrompt a =
  T.concat
    [ "A developer has just saved a change in this repository (the current directory is the tree after the change).\n\n"
    , "The changed units (functions and methods this change adds or changes), with line numbers as in the file after the change:\n"
    , if null (faUnits a) then "(none)" else T.intercalate "\n\n" (map numbered (faUnits a))
    , "\n\nThe change as a diff (tests included):\n"
    , T.stripEnd (faDiff a)
    , "\n\nThe project's steering file, which holds the rules this project keeps:\n<vet>\n"
    , T.stripEnd (faSteering a)
    , "\n</vet>\n\nFindings already reported on these files, which you must not raise again:\n"
    , if null (faReported a) then "none" else T.intercalate "\n" ["- " <> r | r <- faReported a]
    , "\n\nOpen findings from earlier saves on these units:\n"
    , if null (faOpen a) then "none" else T.intercalate "\n" (map openLine (faOpen a))
    , "\n"
    , if null (faOpen a) then "" else "\n" <> openAsk
    , "\n"
    , "Review the changed units. List problems this change has or introduces: nothing else. An empty list is a normal and often the right answer; do not look for something to say. Do not speculate: a finding needs a named code path (the line, the call and what happens), found with Read, Grep and Glob. Do not report style or taste unless it breaks a rule in the steering file.\n\n"
    , "Kinds:\n"
    , "- bug: the code does the wrong thing.\n"
    , "- assumption: the code relies on something about other code that is not so.\n"
    , "- convention: the code breaks a rule in the steering file.\n"
    , "- design: readability, maintainability, testability or architecture with no rule behind it.\n\n"
    , "End your answer with one JSON object and nothing after it:\n"
    , "{\"findings\": [{\"kind\": \"bug|assumption|convention|design\",\n"
    , "   \"file\": \"path/relative/to/repo\", \"lines\": [first line, last line] (in the file after the change),\n"
    , "   \"statement\": \"two to three sentences: what is wrong and why it matters\",\n"
    , "   \"rule\": \"the heading of the rule it breaks, exactly as written after ### in the steering file, or empty\",\n"
    , "   \"evidence\": [{\"file\": \"path/relative/to/repo\", \"lines\": [first line, last line]}]}],\n"
    , " \"open\": [{\"id\": \"the open finding's id\", \"holds\": \"still holds|no longer holds\", \"why\": \"one line\"}]}\n"
    ]
  where
    openAsk = "Answer every open finding in \"open\": \"still holds\" when the code still has that problem, \"no longer holds\" when the code no longer has it, with one line why. Do not raise an open finding again in \"findings\".\n"
    openLine f = "- id " <> vfId f <> ": [" <> vfKind f <> "] " <> T.pack (plFile (vfPlace f)) <> ":" <> tshow (fst (plLines (vfPlace f))) <> ": " <> vfStatement f
    numbered u =
      let (start, end) = fuLines u
          rows = zipWith (\i l -> T.justifyRight 5 ' ' (tshow (start + i)) <> "  " <> l) [0 :: Int ..] (T.lines (fuText u))
          headLine = T.pack (fuFile u) <> ":" <> tshow start <> "-" <> tshow end <> " " <> fuName u <> maybe "" (\h -> " (in `" <> h <> "`)") (nonEmpty (fuHeader u))
       in T.intercalate "\n" (("--- " <> headLine) : rows)
    nonEmpty = (>>= \h -> if T.null h then Nothing else Just h)

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | The reply's JSON (a fenced block, the last that decodes; else the text
-- from the first brace to the last): findings with a known kind, a file, a
-- line pair and a statement; open answers that say still or no longer holds.
-- Left only when no JSON is there at all.
parseFind :: Text -> Either Text FindReply
parseFind text = case jsonIn text of
  Nothing -> Left "no JSON in the find reply"
  Just (Array xs) -> Right (FindReply (mapMaybe finding (toList xs)) [])
  Just (Object o) ->
    Right
      ( FindReply
          (mapMaybe finding (list "findings" o))
          (mapMaybe open (list "open" o))
      )
  Just _ -> Right (FindReply [] [])
  where
    list k o = case KM.lookup k o of
      Just (Array xs) -> toList xs
      _ -> []
    str k o = case KM.lookup k o of
      Just (String s) | not (T.null (T.strip s)) -> Just (T.strip s)
      _ -> Nothing
    int = \case
      Number n -> toBoundedInteger n
      _ -> Nothing
    pairOf = \case
      Array xs | (a : rest) <- toList xs, Just x <- int a, Just y <- int (fromMaybe a (listToMaybe (reverse rest))) -> Just (x, y)
      _ -> Nothing
    finding = \case
      Object o -> do
        kind <- str "kind" o
        if kind `elem` findKinds then Just () else Nothing
        file <- str "file" o
        ls <- case KM.lookup "lines" o of
          Just v -> pairOf v
          Nothing -> KM.lookup "line" o >>= int >>= \l -> Just (l, l)
        statement <- str "statement" o
        let evidence = [(T.unpack f, ls') | Object e <- list "evidence" o, Just f <- [str "file" e], Just ls' <- [KM.lookup "lines" e >>= pairOf]]
        Just (RawFinding kind (T.unpack file) ls statement (str "rule" o) evidence)
      _ -> Nothing
    open = \case
      Object o -> do
        i <- case KM.lookup "id" o of
          Just (String s) -> Just s
          Just (Number n) -> Just (T.pack (show (toRealFloat n :: Double)))
          _ -> Nothing
        holds <- case KM.lookup "holds" o of
          Just (String "still holds") -> Just True
          Just (String "no longer holds") -> Just False
          _ -> Nothing
        Just (i, holds, fromMaybe "" (str "why" o))
      _ -> Nothing

jsonIn :: Text -> Maybe Value
jsonIn t = listToMaybe (mapMaybe decode (fenced ++ braced))
  where
    decode = decodeStrict . TE.encodeUtf8
    fenced
      | "```" `T.isInfixOf` t = [fromMaybe p (T.stripPrefix "json" p) | p <- map T.strip (reverse (T.splitOn "```" t))]
      | otherwise = []
    braced = [span' o c | (o, c) <- [('{', '}'), ('[', ']')]]
    span' o c =
      let (_, from) = T.breakOn (T.singleton o) t
          upto = T.dropWhileEnd (/= c) from
       in upto

-- | The pack's units, as "file:name" pairs, sorted (the recordings' key).
findKey :: FindAsk -> Text
findKey a = T.intercalate "," (sort [T.pack (fuFile u) <> ":" <> fuName u | u <- faUnits a])

type Finder = FindAsk -> IO (Either RunFailure RunResult)

liveFinder :: Cancel -> PlansConfig -> VetConfig -> FilePath -> Finder
liveFinder token cfg v root a =
  runClaude token cfg {pcCheckModel = vcFindModel v, pcTimeout = findTimeout} Check [] Nothing root (findPrompt a)

-- | Recorded replies (tests and the corpus gate): `{"replies": {KEY: TEXT |
-- {"text", "cost", "delay"}}}` by 'findKey'; each key asked is appended to
-- FILE.asked. A key not there is an Empty failure.
recordedFinder :: FilePath -> Finder
recordedFinder file a = do
  let key = findKey a
  withMVar recordLock $ \() -> appendFile (file <> ".asked") (T.unpack key <> "\n")
  replies <- readReplies file
  case KM.lookup (Key.fromText key) replies of
    Just v | Just r <- resultOf v -> do
      case v of
        Object o | Just (Number d) <- KM.lookup "delay" o -> threadDelay (round (toRealFloat d * 1000000 :: Double))
        _ -> pure ()
      pure (Right r)
    _ -> pure (Left (RunFailure Empty "no recorded reply" Nothing))

-- | Units in order, packed into calls of at most 600 numbered lines; a unit
-- longer than that goes alone.
packUnits :: [FindUnit] -> [[FindUnit]]
packUnits = go [] 0
  where
    size u = let (a, z) = fuLines u in z - a + 1
    go acc _ [] = [reverse acc | not (null acc)]
    go acc n (u : us)
      | null acc = go [u] (size u) us
      | n + size u > 600 = reverse acc : go [u] (size u) us
      | otherwise = go (u : acc) (n + size u) us
