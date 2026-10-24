-- | Internal to Olifant.Server: a file's tests, run whole over the editor's
-- text (`olifant/tests/run`). A test file runs itself; a module runs the
-- test file its language's `tests_for` names. No model: the run
-- and how long it took. And a file's other side (`olifant/tests/file`): a
-- module's test file, or a test file's module, for the editor to jump to.
module Olifant.Server.Tests
  ( TestsAnswer (..)
  , TestsScope (..)
  , testsRun
  , testAt
  , testLineOf
  , testsJson
  , TestsFile (..)
  , testsFile
  , testsFileJson
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad (guard, when)
import Data.Char (isAlphaNum)
import Data.Set qualified as S
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Aeson (Value, object, (.=))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import Olifant.App (Project (..))
import Olifant.Catalog (catalogLanguages)
import Olifant.Language (Failure (..), Language (..), TestDef (..), TestRun (..), languageFor, testsForFile)
import Olifant.Server.Site (appendStepLine, defName, runOver, tshow)
import Olifant.Server.State (ServerState (..), projectFor)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (makeRelative, takeExtension, takeFileName, (</>))

-- | What a run covers: the test at a line (1-based; the whole file when the
-- line is in no test, or in a module), a file's tests, or the project's.
data TestsScope = ScopeAt Int | ScopeFile | ScopeAll
  deriving (Eq, Show)

-- | A run of tests: the test file that ran (absolute; the project's root
-- for all), what it said, and its wall time in seconds.
data TestsAnswer = TestsAnswer
  { taFile :: FilePath
  , taScope :: TestsScope
  , taTest :: Maybe Text  -- ^ the one test that ran, when one did
  , taRoot :: FilePath
  , taRun :: TestRun
  , taSeconds :: Double
  }
  deriving (Eq, Show)

-- | Run the tests of PATH over the project's files as the editor has them:
-- the test at a line, the file's (a module: its test file; a test file:
-- itself), or all the project's. A runner that cannot start answers a
-- broken run, with why; Left is one sentence on why nothing ran.
testsRun :: TestsScope -> FilePath -> ServerState -> IO (ServerState, Either Text TestsAnswer)
testsRun scope path s0 =
  placeOf ": nothing to run" path s0 >>= \case
    (s, Left why) -> pure (s, Left why)
    (s, Right p) -> fmap (s,) . runExceptT $ do
      let rel = plRel p
          ws = plWorkspace p
          mapped = testsForFile (plTest p) rel
      (ids, file, one) <- case scope of
        ScopeAll -> do
          paths <- lift (listFiles ws ".")
          let known = S.fromList paths
              mapped' = S.fromList [t | m <- paths, let t = testsForFile (plTest p) m, t /= m, t `S.member` known]
              tests = S.toAscList (mapped' <> S.fromList [m | m <- paths, testShaped (plTest p) m])
          when (null tests) (throwE "no test files in this project: nothing to run")
          pure (map T.pack tests, plRoot p, Nothing)
        _ -> do
          moduleTests <- lift (isJust <$> readText ws mapped)
          testRel <- case () of
            _
              | plIsTest p -> pure rel
              | moduleTests -> pure mapped
              -- a test file whose module is gone: named as test files are
              | testShaped (plTest p) rel -> pure rel
              | otherwise -> throwE ("no test file for this module yet: " <> T.pack mapped)
          here <- case scope of
            ScopeAt n | testRel == rel -> lift (readText ws rel) <&> \t -> t >>= (`testAt` n)
            _ -> pure Nothing
          let one' = (\name -> T.pack testRel <> "::" <> name) <$> here
          pure (maybe [T.pack testRel] pure one', normalisePath (plRoot p </> testRel), one')
      start <- lift getMonotonicTime
      run0 <- lift (either (TestRun [] [] . Just) id <$> runOver s ws (plLanguage p) ids)
      end <- lift getMonotonicTime
      let isOwn tid t = t == tid || (tid <> "[") `T.isPrefixOf` t
          run = case one of
            Just tid
              | Nothing <- trBroken run0
              , not (any (isOwn tid) (trPassed run0 ++ map fst (trFailed run0))) ->
                  run0 {trBroken = Just (snd (T.breakOnEnd "::" tid) <> " did not run: is it still in its test file?")}
            _ -> run0
      let ran = case (scope, one) of
            (ScopeAll, _) -> "all"
            (_, Just tid) -> tid
            _ -> T.intercalate " " ids
      lift (appendStepLine (ssPorts s) (plRoot p) ["test", "person", ran, summary run])
      pure (TestsAnswer file scope one (plRoot p) run (end - start))

-- | A run in a few words, for the step log: `2 passed, 1 failed: test_x`,
-- or `broken: <its first line>`.
summary :: TestRun -> Text
summary run = case trBroken run of
  Just why -> "broken: " <> T.strip (T.takeWhile (/= '\n') why)
  Nothing ->
    tshow (length (trPassed run)) <> " passed, " <> tshow (length (trFailed run)) <> " failed"
      <> (if null (trFailed run) then "" else ": " <> T.intercalate ", " [snd (T.breakOnEnd "::" t) | (t, _) <- trFailed run])

-- | The other side of a file: a module's test file (by `tests_for`), or the
-- module a test file tests (absolute), whether it is the test file, and
-- whether it exists yet.
data TestsFile = TestsFile
  { tfFile :: FilePath
  , tfIsTest :: Bool
  , tfExists :: Bool
  }
  deriving (Eq, Show)

-- | The other side of PATH: a module answers its test file, which may not
-- exist yet; a test file answers the module it tests. Left is one sentence
-- on why there is none.
testsFile :: FilePath -> ServerState -> IO (ServerState, Either Text TestsFile)
testsFile path s0 =
  placeOf ": no test file to jump to" path s0 >>= \case
    (s, Left why) -> pure (s, Left why)
    (s, Right p) -> fmap (s,) . runExceptT $ do
      let rel = plRel p
          absolute = normalisePath . (plRoot p </>)
          mapped = testsForFile (plTest p) rel
      -- inline tests: the file is its own test file
      when (mapped == rel) (throwE (T.pack rel <> " is its own test file (" <> langName (plLanguage p) <> "'s tests_for): no other file to jump to"))
      if plIsTest p || testShaped (plTest p) rel
        then case plTested p of
          Just moduleRel -> pure (TestsFile (absolute moduleRel) False True)
          Nothing -> throwE ("no module's test file is " <> T.pack rel <> ": its module cannot be named")
        else TestsFile (absolute mapped) True . isJust <$> lift (readText (plWorkspace p) mapped)

-- | The answer as the editor reads it: `file`, `kind` (`test` or
-- `module`: what `file` is), `exists`.
testsFileJson :: TestsFile -> Value
testsFileJson f =
  object
    [ "file" .= tfFile f
    , "kind" .= (if tfIsTest f then "test" else "module" :: Text)
    , "exists" .= tfExists f
    ]

-- | Where a file stands toward its tests: its project, language, test
-- table, path in the project, the module it is the test file of (if any)
-- and whether it is a test file.
data Place = Place
  { plRoot :: FilePath
  , plWorkspace :: Workspace IO
  , plLanguage :: Language
  , plTest :: TestDef
  , plRel :: FilePath
  , plTested :: Maybe FilePath
  , plIsTest :: Bool
  }

-- | The place of PATH, or one sentence on why it has no tests (ending in
-- the caller's tail: what there is then nothing of).
placeOf :: Text -> FilePath -> ServerState -> IO (ServerState, Either Text Place)
placeOf tail' path s0 =
  projectFor path s0 >>= \case
    (s, Nothing) -> pure (s, Left (T.pack path <> " is in no Olifant project"))
    (s, Just (root, project)) -> fmap (s,) . runExceptT $ do
      let rel = makeRelative root path
          ws = prWorkspace project
      lang <- maybe (throwE ("no language knows " <> T.pack rel)) pure (languageFor (catalogLanguages (prCatalog project)) rel)
      td <- maybe (throwE (langName lang <> " has no [test] table" <> tail')) pure (langTest lang)
      when (T.null (T.strip (testsFor td))) (throwE (langName lang <> "'s tests_for names no test file" <> tail'))
      paths <- lift (listFiles ws ".")
      let tested = listToMaybe [m | m <- paths, m /= rel, testsForFile td m == rel]
      pure (Place root ws lang td rel tested (testsForFile td rel == rel || isJust tested))

-- | Whether a file is named as `tests_for` names test files: its name ends
-- as the pattern does after the module's name (`__test.py`), and that
-- ending says more than the extension does.
testShaped :: TestDef -> FilePath -> Bool
testShaped td rel =
  case T.breakOn "{= stem =}" (testsFor td) of
    (_, rest) | Just ending <- T.stripPrefix "{= stem =}" rest ->
      not (T.null ending) && not ("{=" `T.isInfixOf` ending) && ending /= T.pack (takeExtension rel) && ending `T.isSuffixOf` T.pack (takeFileName rel)
    _ -> False

-- | The answer as the editor reads it: `file`, `passed` (ids), `failed`
-- (id, line or null, message, output), `broken` (why, or null), `seconds`.
testsJson :: TestsAnswer -> Value
testsJson a =
  object
    [ "file" .= taFile a
    , "passed" .= trPassed (taRun a)
    , "scope" .= (case (taTest a, taScope a) of (Just _, _) -> "test"; (_, ScopeAll) -> "all"; _ -> "file" :: Text)
    , "test" .= taTest a
    , "failed" .= [object ["id" .= tid, "file" .= normalisePath (taRoot a </> T.unpack (T.takeWhile (/= ':') tid)), "line" .= fLine f, "message" .= fMessage f, "output" .= fOutput f] | (tid, f) <- trFailed (taRun a)]
    , "broken" .= trBroken (taRun a)
    , "seconds" .= taSeconds a
    ]

-- | The first line (1-based) of the test NAME (as 'testAt' names it) in
-- a Python test file: its def's, or its first decorator's.
testLineOf :: Text -> Text -> Maybe Int
testLineOf text name = listToMaybe [i | i <- [1 .. length (T.lines text)], testAt text i == Just name]

-- | The test enclosing LINE (1-based) of a Python test file, as pytest
-- names it after `path::`: `test_x`, or `TestX::test_y` for a method of a
-- test class. A decorator line is its def's. Nothing outside any test.
testAt :: Text -> Int -> Maybe Text
testAt text line
  | line < 1 || line > length ls = Nothing
  | otherwise = listToMaybe [qualified d dl | (d, dl) <- candidates, encloses d dl]
  where
    ls = zip [1 :: Int ..] (T.lines text)
    indentOf = T.length . T.takeWhile (== ' ')
    blank = T.null . T.strip
    -- a decorator line stands for the first def below it
    target = case drop (line - 1) ls of
      (_, l) : rest | "@" `T.isPrefixOf` T.strip l -> maybe line fst (listToMaybe [x | x@(_, l') <- rest, isJust (defName l')])
      _ -> line
    candidates = [(i, l) | (i, l) <- reverse (take target ls), Just n <- [defName l], "test" `T.isPrefixOf` n]
    -- every line from the def's to the target is blank or deeper than the def
    encloses d dl = all (\(_, l) -> blank l || indentOf l > indentOf dl) [x | x@(i, _) <- ls, i > d, i <= target]
    qualified d dl = maybe name (\c -> c <> "::" <> name) owner
      where
        name = fromMaybe "" (defName dl)
        -- an indented def: the first less-indented line above, when it is a class
        owner = do
          guard (indentOf dl > 0)
          (_, l) <- listToMaybe [x | x@(i, l) <- reverse ls, i < d, not (blank l), indentOf l < indentOf dl]
          className l
    className l = do
      rest <- T.stripPrefix "class " (T.strip l)
      let n = T.takeWhile (\c -> isAlphaNum c || c == '_') rest
      if T.null n then Nothing else Just n
