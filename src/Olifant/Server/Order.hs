-- | Internal to Olifant.Server: the decision model orders a mutate's survivors. Each
-- is asked about twice, in the background through the decisions the server
-- keeps: is it equivalent to the original (dimmed, and last), and how much
-- does it matter (high first). What is not known yet leaves a survivor
-- undimmed and in the middle; with no decider they stay in file order.
module Olifant.Server.Order (orderSurvivors) where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Decide (Answer (..), Decider, noul, score)
import Olifant.Language (Language (..))
import Olifant.Mutate (Mutant (..))
import Olifant.Server.State (ServerState, decision)
import Text.Regex.TDFA (Regex, makeRegexM, matchTest)

-- | The survivors of the module FILE (in LANG) in the decision model's order, given the module's
-- text and its test file's; ON_DONE runs as each answer lands (the caller orders again).
orderSurvivors :: ServerState -> Maybe (Decider IO) -> Language -> FilePath -> Text -> Text -> IO () -> [Mutant] -> IO [Mutant]
orderSurvivors _ Nothing _ _ _ _ _ ms = pure ms
orderSurvivors s (Just decider) lang file moduleText testText onDone ms = do
  judged <- mapM weigh ms
  pure (map fst (sortOn (\(m, matters) -> (muDim m, Down matters, muLine m)) judged))
  where
    -- the test file's assertion lines, by the language's `[test] assertion`
    asserts = T.intercalate "\n" [T.strip l | l <- T.lines testText, any (`matchesLine` l) (langAssertion lang)]
    matchesLine re l = maybe False (`matchTest` T.unpack l) (makeRegexM (T.unpack re) :: Maybe Regex)
    lineOf n text = maybe "" T.strip (lookup n (zip [1 ..] (T.lines text)))
    weigh m = do
      let mutated = lineOf (muLine m) (muText m)
          context = [("original", lineOf (muLine m) moduleText), ("mutant", mutated), ("tests", asserts)]
          -- each mutant its own: two of one operator may share a line
          key kind = (file, T.intercalate " " [kind, muOperator m, T.pack (show (muLine m)), muFrom m, muTo m, mutated])
          ask kind q = decision s decider (key kind) mutated q onDone
      equivalent <- ask "mutant-equivalent" (noul "mutant-equivalent" "The mutant behaves the same as the original under the tests shown" context)
      matters <- ask "mutant-matters" (score "mutant-matters" "How much a surviving mutant on this line would matter" ["cosmetic: a message or a log line", "logic a test could miss", "a boundary or a branch the tests must pin"] context)
      let weight = case matters of
            Just (Level n _ _) -> n + 1
            _ -> 2 :: Int
      pure (m {muDim = case equivalent of Just (Yes _) -> True; _ -> False}, weight)
