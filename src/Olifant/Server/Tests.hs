-- | Internal to Olifant.Server: a file's tests, run whole over the editor's
-- text (`olifant/tests/run`). A test file runs itself; a module runs the
-- test file its language's `tests_for` names. No model, no block: the run
-- and how long it took.
module Olifant.Server.Tests
  ( TestsAnswer (..)
  , testsRun
  , testsJson
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad (when)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Aeson (Value, object, (.=))
import Data.Maybe (isJust)
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
  projectFor path s0 >>= \case
    (s, Nothing) -> pure (s, Left (T.pack path <> " is in no Olifant project"))
    (s, Just (root, project)) -> fmap (s,) . runExceptT $ do
      let rel = makeRelative root path
          ws = prWorkspace project
      lang <- maybe (throwE ("no language knows " <> T.pack rel)) pure (languageFor (catalogLanguages (prCatalog project)) rel)
      td <- maybe (throwE (langName lang <> " has no [test] table: nothing to run")) pure (langTest lang)
      when (T.null (T.strip (testsFor td))) (throwE (langName lang <> "'s tests_for names no test file: nothing to run"))
      paths <- lift (listFiles ws ".")
      let isTest = testsForFile td rel == rel || any (\m -> m /= rel && testsForFile td m == rel) paths
          mapped = testsForFile td rel
      moduleTests <- lift (isJust <$> readText ws mapped)
      testRel <- case () of
        _
          | isTest -> pure rel
          | moduleTests -> pure mapped
          -- a test file whose module is gone: named as test files are
          | testShaped td rel -> pure rel
          | otherwise -> throwE ("no test file for this module yet: " <> T.pack mapped)
      start <- lift getMonotonicTime
      run <- lift (either (TestRun [] [] . Just) id <$> runOver s ws lang [T.pack testRel])
      end <- lift getMonotonicTime
      pure (TestsAnswer (normalisePath (root </> testRel)) run (end - start))

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
