-- | Internal to Olifant.Server: `olifant/mutate`, the survivors of the
-- unit at a line. The unit's lines are mutated against the module's test
-- file: nothing is stored or gated.
module Olifant.Server.Mutate
  ( MutateAnswer (..)
  , mutateAt
  ) where

import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import Olifant.App (Ports (..), Project (..))
import Olifant.Language (Language (..), TestRun (..), describeSyntaxFailure, runTestsIn)
import Olifant.Generation.FillPhase (projectFiles)
import Olifant.Mutate (Mutant (..), Verdict (..), capMutants, judge, judgeTimeout, mutantsOf)
import Olifant.Mutate.Python (pythonOperators)
import Olifant.Process (cancelled)
import Olifant.Server.Site (Site (..), appendStepLine, siteAt, tshow)
import Olifant.Server.Order (orderSurvivors)
import Olifant.Server.State (ServerState (..), deciderFor)
import Olifant.Server.Unit (Unit (..), unitAt)
import Olifant.Workspace (normalisePath)
import System.FilePath ((</>))

-- | A mutate's answer: the module (absolute), the unit, its lines (1-based),
-- how many mutants were killed, the survivors in the decision model's order
-- (as far as it is known), and the summary.
data MutateAnswer = MutateAnswer
  { maFile :: FilePath
  , maUnit :: Text
  , maLines :: (Int, Int)
  , maKilled :: Int
  , maSurvivors :: [Mutant]
  , maSummary :: Text
  }
  deriving stock (Eq, Show)

-- | The unit at LINE (1-based) of PATH mutated against its module's test
-- file. Left is one sentence on why there is no answer.
mutateAt :: FilePath -> Int -> ServerState -> IO (ServerState, Either Text MutateAnswer)
mutateAt path line s0 =
  siteAt path line s0 >>= \case
    (s, Left why) -> pure (s, Left why)
    (s, Right site) -> fmap (s,) . runExceptT $ do
      let lang = siteLang site
          project = siteProject site
          ws = prWorkspace project
          rel = siteModuleRel site
          file = normalisePath (siteRoot site </> rel)
          moduleText = siteModuleText site
          testRel = siteTestRel site
      when (siteOnTest site) (throwE "on a test: mutate a function or class, from its module")
      unless (langName lang == "python") (throwE ("no mutation operators for " <> langName lang))
      unit <- lift (unitAt (pSyntax (ssPorts s)) lang moduleText line) >>= either (\why -> throwE (why <> ": put the cursor in one to mutate it")) pure
      testText <- maybe (throwE ("no test file for this module yet: " <> T.pack testRel <> "; stub one first (:Durandal stub)")) pure (siteTestText site)
      let (l1, l2) = unitLines unit
          scope = S.fromList [l1 .. l2]
          stopIfCancelled = lift (cancelled (ssCancel s)) >>= \stopped -> when stopped (throwE "cancelled")
          done killed survivors summary = do
            lift (appendStepLine (ssPorts s) (siteRoot site) ["mutate", "person", T.pack rel <> "::" <> unitName unit, summary])
            pure (MutateAnswer file (unitName unit) (l1, l2) killed survivors summary)
      mutants <- lift (mutantsOf (pSyntax (ssPorts s)) lang pythonOperators moduleText scope) >>= either (throwE . describeSyntaxFailure) pure
      case mutants of
        [] -> done 0 [] ("nothing to mutate in " <> unitName unit)
        ms -> do
          (_, files) <- lift (projectFiles ws M.empty lang [testRel])
          t0 <- lift getMonotonicTime
          baseline <- lift (runTestsIn (pTester (ssPorts s) (ssCancel s)) lang [T.pack testRel] files)
          t1 <- lift getMonotonicTime
          stopIfCancelled
          base <- either (\why -> throwE ("could not run: " <> why)) pure baseline
          mapM_ (\why -> throwE ("could not run: " <> why)) (trBroken base)
          case trFailed base of
            (t, _) : _ -> throwE ("could not judge: " <> snd (T.breakOnEnd "::" t) <> " fails before mutation")
            [] -> pure ()
          verdicts <- lift (judge (pTesterWithin (ssPorts s) (judgeTimeout (t1 - t0)) (ssCancel s)) 4 lang rel files (T.pack testRel) (trPassed base) (capMutants 20 ms))
          stopIfCancelled
          vs <- either (\why -> throwE ("could not judge: " <> why)) pure verdicts
          let surviving = [m | (m, Survived) <- vs]
              killed = length vs - length surviving
              n = length surviving
              summary
                | n == 0 = "no survivors \183 " <> tshow killed <> "/" <> tshow (length vs) <> " killed"
                | otherwise = tshow n <> (if n == 1 then " survivor" else " survivors") <> " \183 " <> tshow killed <> "/" <> tshow (length vs) <> " killed"
          ordered <- lift (orderSurvivors s (deciderFor s project) file moduleText testText (pure ()) surviving)
          done killed ordered summary
