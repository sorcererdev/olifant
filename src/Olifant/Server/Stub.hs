-- | Internal to Olifant.Server: a new test's stub (`olifant/stub`) for the
-- function, method or class at a line of a module, at the end of its test
-- file. No model, no block: the test file's edit and where the test's
-- name is.
module Olifant.Server.Stub
  ( StubAnswer (..)
  , stubAt
  , classAt
  , testNameOf
  , belowDecorators
  ) where

import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Char (isAlphaNum, isDigit, isLower, isUpper, toLower)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Block (stubNameFor, uniqueTestName)
import Olifant.Generation.FillPhase (addImports)
import Olifant.Language (Language (..))
import Olifant.Mutate (Mutant (..))
import Olifant.Server.Block (Site (..), appendStepLine, defLineAbove, defName, functionAt, importOf, siteAt, siteTestFile, tshow)
import Olifant.Server.Edits (FileEdit (..), smallestChange)
import Olifant.Server.State (ServerState (..))

-- | The class enclosing LINE (1-based) of a Python module: its name and
-- its `class` line. A blank line is in it only when the next line that is
-- not blank is.
classAt :: Text -> Int -> Maybe (Text, Int)
classAt text line
  | line < 1 || line > length ls = Nothing
  | otherwise = listToMaybe [(n, d) | (d, l) <- reverse (take line ls), Just n <- [className l], encloses d l]
  where
    ls = zip [1 :: Int ..] (T.lines text)
    indentOf = T.length . T.takeWhile (== ' ')
    blank = T.null . T.strip
    next = listToMaybe [l | (i, l) <- ls, i >= line, not (blank l)]
    encloses d dl =
      d == line
        || ( all (\(_, l) -> blank l || indentOf l > indentOf dl) [x | x@(i, _) <- ls, i > d, i <= line]
               && maybe False (\l -> indentOf l > indentOf dl) next
           )
    className l = do
      rest <- T.stripPrefix "class " (T.strip l)
      let n = T.takeWhile (\c -> isAlphaNum c || c == '_') rest
      if T.null n then Nothing else Just n

-- | A unit's test name: `test_` and its name in snake case, the
-- underscores at its ends dropped (`CloseTable` → `test_close_table`).
testNameOf :: Text -> Text
testNameOf n = "test_" <> T.pack (go ' ' (T.unpack (T.dropAround (== '_') n)))
  where
    go prev (c : cs)
      | isUpper c && (isLower prev || isDigit prev) = '_' : toLower c : go c cs
      | otherwise = toLower c : go c cs
    go _ [] = []

-- | A decorator line (1-based) stands for the def or class below it.
belowDecorators :: Text -> Int -> Int
belowDecorators text line = case drop (line - 1) ls of
  l : _ | "@" `T.isPrefixOf` T.strip l -> maybe line fst (listToMaybe [x | x@(_, l') <- drop line (zip [1 ..] ls), isJust (defName l') || "class " `T.isPrefixOf` T.strip l'])
  _ -> line
  where
    ls = T.lines text

-- | A stub's answer: the test file (absolute), the new test's def line
-- (1-based, in the edited file), its name, and the test file's one edit
-- (creating the file when it is new).
data StubAnswer = StubAnswer
  { saFile :: FilePath
  , saLine :: Int
  , saName :: Text
  , saEdit :: FileEdit
  }
  deriving (Eq, Show)

-- | A new test's stub for the function, method or class at LINE (1-based;
-- a decorator's is its def's) of a module: `test_<unit>` after the test
-- file's last line, the unit's import joined to the file's (a new file
-- starts with it), its body `...` and `assert ...`. Nothing is drafted;
-- no block is read or stored. With a survivor (its line is LINE), the test
-- is named from the mutant and the mutant is a comment in its body. Left is
-- one sentence on why there is none.
stubAt :: FilePath -> Int -> Maybe Mutant -> ServerState -> IO (ServerState, Either Text StubAnswer)
stubAt path line0 survivor s0 =
  siteAt path line0 s0 >>= \case
    (s, Left why) -> pure (s, Left why)
    (s, Right site0) -> fmap (s,) . runExceptT $ do
      when (siteOnTest site0) (throwE "on a test: stub writes a new test for a function or class, from its module")
      let lang = siteLang site0
          moduleText = siteModuleText site0
          line = belowDecorators moduleText line0
          above = defLineAbove line moduleText
          site = site0 {siteAtCursor = snd <$> above, siteDefLine = fst <$> above}
      unless (langName lang == "python") (throwE ("stub writes Python tests only, not " <> langName lang <> " yet"))
      (unit, defLine) <-
        lift (functionAt s site line) >>= \case
          Just (n, d, _) -> pure (n, d)
          Nothing -> maybe (throwE ("no function or class at line " <> tshow line0 <> ": put the cursor in one to stub its test")) pure (classAt moduleText line)
      let testFile = siteTestFile site
          testText0 = siteTestText site
          base = fromMaybe "" testText0
          name = maybe (uniqueTestName (testNameOf unit) base) (\m -> stubNameFor m base (langKeywords lang)) survivor
          flat = T.replace "\n" "\9166"
          note = case survivor of
            Just m -> ind <> "# kills: " <> flat (muFrom m) <> " \8594 " <> flat (muTo m) <> " on line " <> tshow (muLine m) <> "\n"
            Nothing -> ""
          ind = langIndent lang
          imported = case importOf lang (siteModuleRel site) moduleText unit (Just defLine) of
            Just i | T.null (T.strip base) -> i
            Just i -> addImports lang [i] base
            Nothing -> base
          kept = T.stripEnd imported
          stub = "def " <> name <> "() -> None:\n" <> note <> ind <> "...\n" <> ind <> "assert ...\n"
          new = if T.null kept then stub else kept <> "\n\n\n" <> stub
          at = if T.null kept then 1 else length (T.splitOn "\n" kept) + 3
          edit = case testText0 of
            Nothing -> FileEdit testFile Nothing True ((0, 0), (0, 0)) new
            Just _ -> let (range, t) = smallestChange base new in FileEdit testFile (fst <$> M.lookup testFile (ssDocs s)) False range t
      lift (appendStepLine (ssPorts s) (siteRoot site) ["stub", "person", T.pack (siteModuleRel site) <> "::" <> unit, "stubbed " <> name <> maybe "" (\m -> " (kills line " <> tshow (muLine m) <> ")") survivor])
      pure (StubAnswer testFile at name edit)
