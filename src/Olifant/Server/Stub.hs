-- | Internal to Olifant.Server: a new test's stub (`olifant/stub`) for the
-- function, method or class at a line of a module, in its test file, as
-- the language's `[test.stub]` writes it. No model: the test file's edit
-- and where the test's name is.
module Olifant.Server.Stub
  ( StubAnswer (..)
  , stubAt
  , stubWith
  , testNameIn
  , testWordsIn
  , namePrefixesIn
  , unitWordsOf
  ) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Char (isDigit, isLower, isUpper, toLower)
import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Anchor (Located (..), RenderedAnchor (..), Spacing (..), insertLines, resolveLayers)
import Olifant.Generation.FillPhase (addImports)
import Olifant.Language (ImportsDef (..), Language (..), ModuleOf (..), StubDef (..), TestDef (..), moduleOf, regexName)
import System.FilePath (takeExtension)
import Olifant.Language.TestUnits (TestUnit (..), anchorName, testUnitsIn)
import Olifant.Mutate (Mutant (..))
import Olifant.Naming (wordsName)
import Olifant.Server.Edits (FileEdit (..), smallestChange)
import Olifant.Server.Site (Site (..), appendStepLine, importOf, siteAt, siteTestFile, tshow)
import Olifant.Server.State (ServerState (..))
import Olifant.Server.Unit (Named (..), unitAround)
import Olifant.App (Ports (..))
import Olifant.Template (Value (..), parseTemplate, render)
import Olifant.TestName (stubNameFor)

-- | A stub's answer: the test file (absolute), the new test's first line
-- (1-based, in the edited file), its name as a plan anchor names it
-- ('anchorName' of its path: `test_x`, `Olifant.Plans.reads_a_step`), and
-- the test file's one edit (creating the file when it is new).
data StubAnswer = StubAnswer
  { saFile :: FilePath
  , saLine :: Int
  , saName :: Text
  , saEdit :: FileEdit
  }
  deriving (Eq, Show)

-- | A new test's stub for the unit at LINE (1-based; a decorator's is its
-- def's) of a module, by the language's `[test.stub]`: its `test` after the
-- test file's last line (`gap` blank lines before it), named by `[test]
-- name` from the unit's words, the unit's import (`[imports] write`) joined
-- to the file's; a new file starts with the stub's `file` head. Nothing is
-- conjured or stored. With a survivor (its line is LINE), the test is named
-- from the mutant and the mutant is a comment in its body. Left is one
-- sentence on why there is none.
stubAt :: FilePath -> Int -> Maybe Mutant -> ServerState -> IO (ServerState, Either Text StubAnswer)
stubAt = stubWith Nothing

-- | 'stubAt' with the test's name given (still made unique against the
-- test file); Nothing names it as 'stubAt' does.
stubWith :: Maybe Text -> FilePath -> Int -> Maybe Mutant -> ServerState -> IO (ServerState, Either Text StubAnswer)
stubWith given path line0 survivor s0 =
  siteAt path line0 s0 >>= \case
    (s, Left why) -> pure (s, Left why)
    (s, Right site) -> fmap (s,) . runExceptT $ do
      when (siteOnTest site) (throwE "on a test: stub writes a new test for a function or class, from its module")
      let lang = siteLang site
          moduleText = siteModuleText site
          td = langTest lang
      sd <- maybe (throwE ("no `[test] stub` for " <> langName lang <> ": add a [test.stub] (or a [test] preset that has one) to stub its tests")) pure (td >>= testStub)
      named <- lift (unitAround (pSyntax (ssPorts s)) lang moduleText line0) >>= either throwE pure
      let qualified = namedName named
          unit = snd (T.breakOnEnd "." qualified)
          testFile = siteTestFile site
          testText0 = siteTestText site
          base = fromMaybe "" testText0
          rules = maybe [] testUnitRules td
          above = td >>= testAbove
          defined = [n | u <- testUnitsIn rules above base, Just n <- [listToMaybe (reverse (tuPath u))]]
          name = case given of
            Just n -> uniqueAmong n defined
            Nothing -> maybe (uniqueAmong (testNameIn lang (unitWordsOf unit)) defined) (\m -> stubNameFor m base (langKeywords lang)) survivor
          flat = T.replace "\n" "\9166"
          ind = langIndent lang
          note = case survivor of
            Just m -> ind <> langComment lang <> " kills: " <> flat (muFrom m) <> " \8594 " <> flat (muTo m) <> " on line " <> tshow (muLine m) <> "\n"
            Nothing -> ""
          modu = moduleOf (fromMaybe (ModuleOf [] [] ".") (langImportsDef lang >>= imModuleOf)) (siteModuleRel site)
          imported = importOf lang (siteModuleRel site) qualified
          env extra = M.fromList ([("name", VText name), ("unit", VText unit), ("module", VText modu), ("indent", VText ind), ("note", VText note)] ++ extra)
          rendered what src = either (const (throwE ("the language's [test.stub] " <> what <> " does not render"))) pure (parseTemplate src >>= render (env [("import", VText (fromMaybe "" imported))]))
      test <- rendered "test" (sdTest sd)
      (new, at) <-
        if T.null (T.strip base)
          then do
            -- a new file: the stub's head, then the test
            hd <- maybe (pure "") (rendered "file") (sdFile sd)
            let hd' = if T.null (T.strip hd) then "" else hd
            pure (hd' <> test, length (T.lines hd') + 1)
          else do
            -- the tests in another file (`mod tests;`): refused, naming it
            case sdElsewhere sd >>= \re -> listToMaybe [n | l <- T.lines base, Just n <- [regexName re l]] of
              Just other -> throwE ("tests live in another file: " <> other <> T.pack (takeExtension (siteTestRel site)))
              Nothing -> pure ()
            let joined = case imported of
                  Just i -> addImports lang [i] base
                  Nothing -> base
                kept = T.stripEnd joined
                gap = T.replicate (sdGap sd + 1) "\n"
                atEnd t = (kept <> gap <> t, length (T.splitOn "\n" kept) + sdGap sd + 1)
            placed <- case sdAt sd of
              Nothing -> pure Nothing
              Just phrase -> Just <$> lift (resolveLayers (pSyntax (ssPorts s)) (Just lang) [PhraseAnchor phrase] joined)
            case (placed, sdAt sd, sdOpen sd) of
              -- at its target (`end of mod tests`), after the gap
              (Just (Right loc), _, _) ->
                let (first, t) = insertLines loc {locSpacing = NoSpacing} (T.replicate (sdGap sd) "\n" <> test) joined
                 in pure (t, first + sdGap sd)
              -- no target: opened at the file's end, the test inside
              (_, Just _, Just o) -> do
                opened <- either (const (throwE "the language's [test.stub] open does not render")) pure (parseTemplate o >>= render (env [("test", VText test)]))
                let (new, at0) = atEnd opened
                    inside = length (T.lines (fst (T.breakOn test opened)))
                pure (new, at0 + inside)
              _ -> pure (atEnd test)
      let edit = case testText0 of
            Nothing -> FileEdit testFile Nothing True ((0, 0), (0, 0)) new
            Just _ -> let (range, t) = smallestChange base new in FileEdit testFile (fst <$> M.lookup testFile (ssDocs s)) False range t
          -- its name as an anchor names it: the path of the test at its line
          anchored = maybe name (anchorName . tuPath) (listToMaybe [u | u <- reverse (testUnitsIn rules above new), fst (tuLines u) <= at, at <= snd (tuLines u)])
      lift (appendStepLine (ssPorts s) (siteRoot site) ["stub", "person", T.pack (siteModuleRel site) <> "::" <> unit, "stubbed " <> name <> maybe "" (\m -> " (kills line " <> tshow (muLine m) <> ")") survivor])
      pure (StubAnswer testFile at anchored edit)

-- | A new test's name in LANG from WORDS, by its `[test] name` template
-- over @{= words =}@ (a name of those words: `test_{= words|snake =}`);
-- the words joined by @_@ without one. No words are the word `step`.
testNameIn :: Language -> [Text] -> Text
testNameIn lang ws0 = fromMaybe (render' "{= words|snake =}") (render' <$> (langTest lang >>= testNaming))
  where
    ws = if null ws0 then ["step"] else ws0
    render' src = either (const (T.intercalate "_" ws)) id (parseTemplate src >>= render (M.singleton "words" (VName (wordsName ws))))

-- | A test NAME's words in LANG: past the literal text its `[test] name`
-- template puts before and after the words (pytest's @test_@), each @_@ a
-- space.
testWordsIn :: Language -> Text -> Text
testWordsIn lang name = T.replace "_" " " (fromMaybe core (T.stripSuffix after core))
  where
    marker = "olifantwords"
    (before, after) = case T.breakOn marker (testNameIn lang [marker]) of
      (b, a) | not (T.null a) -> (b, T.drop (T.length marker) a)
      _ -> ("", "")
    core = fromMaybe name (T.stripPrefix before name)

-- | What a test's name in LANG drops to be short beside the module STEM:
-- the name its `[test] name` template gives the stem's words (pytest's
-- @test_cancel_booking@), then the template's text before the words
-- (@test_@); 'Olifant.Server.ConjureNext.shortName' takes them.
namePrefixesIn :: Language -> Text -> [Text]
namePrefixesIn lang stem = [testNameIn lang (unitWordsOf stem), before]
  where
    marker = "olifantwords"
    before = fst (T.breakOn marker (testNameIn lang [marker]))

-- | A unit's name as words: the underscores at its ends dropped, split at
-- each underscore and before a capital after a lowercase letter or a digit,
-- lowercased (`CloseTable` is `close table`, `V2Table` `v2 table`).
unitWordsOf :: Text -> [Text]
unitWordsOf n = T.splitOn "_" (T.pack (go ' ' (T.unpack (T.dropAround (== '_') n))))
  where
    go prev (c : cs)
      | isUpper c && (isLower prev || isDigit prev) = '_' : toLower c : go c cs
      | otherwise = toLower c : go c cs
    go _ [] = []

-- | NAME, or, NAMES holding it already, the first of NAME_2, NAME_3, … they
-- do not hold.
uniqueAmong :: Text -> [Text] -> Text
uniqueAmong base names = fromMaybe base (find (`notElem` names) (base : [base <> "_" <> T.pack (show i) | i <- [2 :: Int ..]]))
