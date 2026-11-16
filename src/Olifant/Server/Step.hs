{-# LANGUAGE OverloadedStrings #-}

-- | A conjure is one step (design 2026-11-16): the writer reads the situation at
-- the cursor, names a small goal and answers whole files. This module is the
-- pure part: the prompt ('stepSystem', 'situationText'), the reply
-- ('readStep'), which answered paths may land ('keptPath'), the tests' change
-- ('testDelta'), what is said in the step log ('stepLogLine') and the session
-- block ('sessionFilter'). The text was measured in
-- experiments/conjure-2026-11 (shape C: forward on every case with haiku).
module Olifant.Server.Step
  ( Situation (..)
  , StepReply (..)
  , TestDelta (..)
  , StepHeld (..)
  , stepSystem
  , situationText
  , readStep
  , keptPath
  , testDelta
  , stepLogLine
  , sessionFilter
  ) where

import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language (TestRun (..))
import System.FilePath (isAbsolute, joinPath, makeRelative, splitDirectories)

-- | What the writer is told about the cursor. 'siTestsBefore' is the
-- before-run in words; 'siSession' the person's uncommitted changes (empty:
-- none, or not a git repository).
data Situation = Situation
  { siLanguage :: Text
  , siTestNaming :: Text
  , siFile :: FilePath
  , siUnit :: Text
  , siLine :: Int
  , siSpan :: (Int, Int)
  , siModule :: Text
  , siTestFile :: FilePath
  , siTestText :: Maybe Text
  , siTestsBefore :: Text
  , siRelated :: [(FilePath, Text)]
  , siSession :: Text
  }
  deriving stock (Eq, Show)

-- | The writer's answer: its goal, the files it wrote (path as written, whole
-- text), and whether it said there is nothing to do.
data StepReply = StepReply {srGoal :: Maybe Text, srFiles :: [(FilePath, Text)], srNothing :: Bool}
  deriving stock (Eq, Show)

-- | How the tests moved: newly passing, newly failing (passing before: a
-- regression), still red (failing after and not passing before: failing, new,
-- or not able to run before), and whether there was no after-run at all.
data TestDelta = TestDelta {tdPassing :: [Text], tdFailing :: [Text], tdStillRed :: [Text], tdNoTests :: Bool}
  deriving stock (Eq, Show)

-- | A step shown and not yet answered: where (root, the unit's file, its
-- unit), who wrote it, its goal, the situation it was asked (a harder writer
-- is asked the same), and what it writes: each file's new text (absolute
-- paths), the editor's version and text when read (Nothing: not open; a new
-- file's text is Nothing), with the facts the review shows.
data StepHeld = StepHeld
  { hdRoot :: FilePath
  , hdFile :: FilePath
  , hdUnit :: Text
  , hdWriter :: Text
  , hdGoal :: Text
  , hdSituation :: Text
  , hdWrites :: Map FilePath Text
  , hdVersions :: Map FilePath (Maybe Int)
  , hdBases :: Map FilePath (Maybe Text)
  , hdDelta :: TestDelta
  , hdParses :: [(FilePath, Bool)]
  , hdLines :: Int
  , hdCost :: Maybe Double
  , hdSeconds :: Double
  }
  deriving stock (Eq, Show)

stepSystem :: Text
stepSystem =
  T.unlines
    [ "You take one small step on a programmer's code where their cursor is. Read what is there and do the next small thing"
    , "that leaves the code more right than you found it: fix a syntax error, fill or continue a body, make a failing test pass,"
    , "write a missing test; or nothing, when the code is complete and its tests pass."
    , "The code on screen is the person's work in progress, not a rejected attempt: continue their intent, keep what already"
    , "works, change only what the step needs. A red test is the clearest goal, but a step need not turn every test green,"
    , "and a step needs no test to be worth taking."
    , ""
    , "Reply in this format and nothing else (no prose, no markdown fences):"
    , "<goal>the step you take, one sentence</goal>"
    , "then one block per file you change or create, with the file's complete new text:"
    , "<file path=\"path/relative/to/the/project\">"
    , "the complete file"
    , "</file>"
    , "When nothing needs doing: <goal>nothing</goal> and no file blocks."
    ]

situationText :: Situation -> Text
situationText s =
  T.unlines $
    [ "Project files are " <> siLanguage s <> " (" <> siTestNaming s <> ")."
    , ""
    , "The cursor is in `" <> siUnit s <> "` in " <> T.pack (siFile s) <> ", line " <> tshow (siLine s) <> " (the unit is lines " <> tshow (fst (siSpan s)) <> "-" <> tshow (snd (siSpan s)) <> ")."
    , ""
    , "File " <> T.pack (siFile s) <> ":"
    , "```"
    , siModule s
    , "```"
    , ""
    ]
      ++ maybe
        ["There is no test file for this module yet (its tests would go in " <> T.pack (siTestFile s) <> ")."]
        (\t -> ["Its test file " <> T.pack (siTestFile s) <> ":", "```", t, "```"])
        (siTestText s)
      ++ ["", "Tests before the step: " <> siTestsBefore s]
      ++ concat [["", "Related file " <> T.pack p <> ":", "```", t, "```"] | (p, t) <- siRelated s]
      ++ (if T.null (T.strip (siSession s)) then [] else ["", "What the person has been doing (uncommitted changes):", siSession s])

readStep :: Text -> StepReply
readStep t = StepReply goal files (goal == Just "nothing" && null files)
  where
    goal = case T.breakOn "<goal>" t of
      (_, "") -> Nothing
      (_, rest) -> Just (T.strip (fst (T.breakOn "</goal>" (T.drop 6 rest))))
    files =
      [ (T.unpack (T.strip path), ensureNl (unfence (trimNl body)))
      | chunk <- drop 1 (T.splitOn "<file path=\"" t)
      , let (path, rest) = T.breakOn "\">" chunk
      , not (T.null rest)
      , let (body, close) = T.breakOn "</file>" (T.drop 2 rest)
      , not (T.null close)
      ]
    trimNl b = let b' = fromMaybe b (T.stripPrefix "\n" b) in fromMaybe b' (T.stripSuffix "\n" b')
    unfence b = case T.lines b of
      (l : rest) | "```" `T.isPrefixOf` T.strip l, not (null rest), "```" `T.isPrefixOf` T.strip (last rest) -> T.intercalate "\n" (init rest)
      _ -> b
    ensureNl b = if "\n" `T.isSuffixOf` b then b else b <> "\n"

-- | An answered path, relative to the project root, if it may land: inside
-- the project, and not under @.git@ or @.olifant@.
keptPath :: FilePath -> FilePath -> Maybe FilePath
keptPath root path = do
  rel <- if isAbsolute path then (if root' `isPrefixOf` path then Just (makeRelative root path) else Nothing) else Just path
  parts <- normal (splitDirectories rel)
  case parts of
    [] -> Nothing
    (top : _) | top `elem` [".git", ".olifant"] -> Nothing
    _ -> Just (joinPath parts)
  where
    root' = if "/" `isSuffixOf'` root then root else root <> "/"
    isSuffixOf' suf s = reverse suf `isPrefixOf` reverse s
    normal = go []
      where
        go acc [] = Just (reverse acc)
        go acc ("." : xs) = go acc xs
        go [] (".." : _) = Nothing
        go (_ : acc) (".." : xs) = go acc xs
        go acc ("/" : xs) = go acc xs
        go acc (x : xs) = go (x : acc) xs

testDelta :: Maybe TestRun -> Maybe TestRun -> TestDelta
testDelta _ Nothing = TestDelta [] [] [] True
testDelta before (Just after) = TestDelta newlyPassing newlyFailing stillRed False
  where
    passedBefore = maybe [] trPassed before
    failedAfter = map fst (trFailed after)
    newlyPassing = [t | t <- trPassed after, t `notElem` passedBefore]
    newlyFailing = [t | t <- failedAfter, t `elem` passedBefore]
    stillRed = [t | t <- failedAfter, t `notElem` passedBefore]

stepLogLine :: Text -> Text -> Int -> TestDelta -> Text -> Text
stepLogLine writer goal files d choice =
  T.intercalate " · " ["conjure step " <> writer, goal, tshow files <> (if files == 1 then " file" else " files"), tests, choice]
  where
    tests
      | tdNoTests d = "no tests"
      | otherwise = T.intercalate ", " (["+" <> tshow (length (tdPassing d)) <> " tests"] ++ ["-" <> tshow (length (tdFailing d)) <> " failing" | not (null (tdFailing d))])

-- | The session block: per-file diffs (most recently changed first) kept to
-- the language's extensions, without the book, @.olifant@, lockfiles and
-- binary diffs, cut at @cap@ lines with a last line saying what was left out.
sessionFilter :: [Text] -> Int -> [(FilePath, Text)] -> Text
sessionFilter exts cap diffs
  | length ls <= cap = T.unlines ls
  | otherwise = T.unlines (take cap ls ++ ["… (" <> tshow (length ls - cap) <> " more lines)"])
  where
    ls = concatMap (T.lines . snd) (filter kept diffs)
    kept (p, d) =
      any (`T.isSuffixOf` T.pack p) exts
        && not (any (`isPrefixOf` p) ["book/", ".olifant/"])
        && not (".lock" `T.isSuffixOf` T.pack p || "-lock." `T.isInfixOf` T.pack p)
        && not (any ("Binary files " `T.isPrefixOf`) (T.lines d))

tshow :: Show a => a -> Text
tshow = T.pack . show
