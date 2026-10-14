-- | Internal to Olifant.Server: a file's tests, run whole over the editor's
-- text (`olifant/tests/run`). A test file runs itself; a module runs the
-- test file its language's `tests_for` names. No model, no block: the run
-- and how long it took. And a file's other side (`olifant/tests/file`): a
-- module's test file, or a test file's module, for the editor to jump to.
module Olifant.Server.Tests
  ( TestsAnswer (..)
  , testsRun
  , testsJson
  , TestsFile (..)
  , testsFile
  , testsFileJson
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad (when)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Aeson (Value, object, (.=))
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import Olifant.App (Project (..))
import Olifant.Catalog (catalogLanguages)
import Olifant.Language (Failure (..), Language (..), TestDef (..), TestRun (..), languageFor, testsForFile)
import Olifant.Server.Block (runOver)
import Olifant.Server.State (ServerState (..), projectFor)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (makeRelative, takeExtension, takeFileName, (</>))

-- | A run of a file's tests: the test file that ran (absolute), what it
-- said, and its wall time in seconds.
data TestsAnswer = TestsAnswer
  { taFile :: FilePath
  , taRun :: TestRun
  , taSeconds :: Double
  }
  deriving (Eq, Show)

-- | Run the tests of PATH (a module: its test file; a test file: itself)
-- over the project's files as the editor has them, as one id: the file. A
-- runner that cannot start answers a broken run, with why; Left is one
-- sentence on why nothing ran.
testsRun :: FilePath -> ServerState -> IO (ServerState, Either Text TestsAnswer)
testsRun path s0 =
  placeOf ": nothing to run" path s0 >>= \case
    (s, Left why) -> pure (s, Left why)
    (s, Right p) -> fmap (s,) . runExceptT $ do
      let rel = plRel p
          ws = plWorkspace p
          mapped = testsForFile (plTest p) rel
      moduleTests <- lift (isJust <$> readText ws mapped)
      testRel <- case () of
        _
          | plIsTest p -> pure rel
          | moduleTests -> pure mapped
          -- a test file whose module is gone: named as test files are
          | testShaped (plTest p) rel -> pure rel
          | otherwise -> throwE ("no test file for this module yet: " <> T.pack mapped)
      start <- lift getMonotonicTime
      run <- lift (either (TestRun [] [] . Just) id <$> runOver s ws (plLanguage p) [T.pack testRel])
      end <- lift getMonotonicTime
      pure (TestsAnswer (normalisePath (plRoot p </> testRel)) run (end - start))

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
    , "failed" .= [object ["id" .= tid, "line" .= fLine f, "message" .= fMessage f, "output" .= fOutput f] | (tid, f) <- trFailed (taRun a)]
    , "broken" .= trBroken (taRun a)
    , "seconds" .= taSeconds a
    ]
