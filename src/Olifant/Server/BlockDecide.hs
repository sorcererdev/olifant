-- | Internal to Olifant.Server: the decision model orders a judged block's survivors. Each
-- is asked about twice, in the background through the decisions the server
-- keeps: is it equivalent to the original (dimmed, and last), and how much
-- does it matter (high first). What is not known yet leaves a survivor
-- undimmed and in the middle; with no decider they stay in file order.
module Olifant.Server.BlockDecide (orderSurvivors) where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Decide (Answer (..), Decider, Question (..))
import Olifant.Mutate (Mutant (..))
import Olifant.Server.State (ServerState, decision)

-- | The survivors of the module FILE in the decision model's order, given the module's text and its
-- test file's; ON_DONE runs as each answer lands (the caller orders again).
orderSurvivors :: ServerState -> Maybe (Decider IO) -> FilePath -> Text -> Text -> IO () -> [Mutant] -> IO [Mutant]
orderSurvivors _ Nothing _ _ _ _ ms = pure ms
orderSurvivors s (Just decider) file moduleText testText onDone ms = do
  judged <- mapM weigh ms
  pure (map fst (sortOn (\(m, matters) -> (muDim m, Down matters, muLine m)) judged))
  where
    asserts = T.intercalate "\n" [T.strip l | l <- T.lines testText, "assert" `T.isPrefixOf` T.strip l]
    lineOf n text = maybe "" T.strip (lookup n (zip [1 ..] (T.lines text)))
    weigh m = do
      let mutated = lineOf (muLine m) (muText m)
          context = [("original", lineOf (muLine m) moduleText), ("mutant", mutated), ("tests", asserts)]
          -- each mutant its own: two of one operator may share a line
          key kind = (file, T.intercalate " " [kind, muOperator m, T.pack (show (muLine m)), muFrom m, muTo m, mutated])
          ask kind options = decision s decider (key kind) mutated (Question kind options [] context) onDone
      equivalent <- ask "mutant-equivalent" ["equivalent", "changes-behaviour"]
      matters <- ask "mutant-matters" ["low", "medium", "high"]
      let weight = case matters of
            Just (Chosen "high" _) -> 3
            Just (Chosen "low" _) -> 1
            _ -> 2 :: Int
      pure (m {muDim = case equivalent of Just (Chosen "equivalent" _) -> True; _ -> False}, weight)
