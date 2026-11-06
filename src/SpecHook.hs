-- | Every spec runs in parallel (`hspec -j`, the cores by default): most
-- wait on compilers and models, not the CPU. A spec that measures time is
-- marked `sequential` where it is.
--
-- Every example also fails after 'exampleLimit' seconds instead of hanging
-- the suite (hspec has no timeout of its own): the slowest example takes
-- about 17 s on a quiet machine, so the limit leaves room for a loaded one.
--
-- Grammars load from the vendored copies (@library/grammars/@) into the
-- suite's own cache: no spec fetches, and the olifant processes specs start
-- find them through @OLIFANT_GRAMMAR_SOURCES@. The shipped three are built
-- before the first example, so a spec never races a build.
module SpecHook (hook, failAfter, exampleLimit, grammarSetup) where

import Control.Monad (forM_)
import Olifant.Language (langGrammar, shippedLanguageNow)
import Olifant.Language.Grammar (GrammarSettings (..), defaultSettings, loadGrammar, setGrammarSettings)
import System.Directory (getTemporaryDirectory, makeAbsolute)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Hspec (Spec, around_, beforeAll_, expectationFailure, parallel)

hook :: Spec -> Spec
hook = parallel . beforeAll_ grammarSetup . around_ (failAfter exampleLimit)

-- | The suite's grammar settings, and the shipped grammars built once.
grammarSetup :: IO ()
grammarSetup = do
  tmp <- getTemporaryDirectory
  local <- makeAbsolute "library/grammars"
  setEnv "OLIFANT_GRAMMAR_SOURCES" local
  -- no spec reaches the network: nvim's repository is one that is not there
  setGrammarSettings (defaultSettings (tmp </> "olifant-test-cache") [local]) {gsTextobjects = ("file:///nonexistent/nvim-treesitter-textobjects", "0000000000000000000000000000000000000000")}
  forM_ ["python", "rust", "haskell"] $ \g ->
    loadGrammar (langGrammar (shippedLanguageNow g)) >>= either (\f -> fail ("the " <> show g <> " grammar does not load: " <> show f)) (const (pure ()))

exampleLimit :: Int
exampleLimit = 180

-- | ACT, failed when it runs past LIMIT seconds.
failAfter :: Int -> IO () -> IO ()
failAfter limit act =
  timeout (limit * 1000000) act >>= \case
    Just () -> pure ()
    Nothing -> expectationFailure ("timed out after " <> show limit <> " s (SpecHook)")
