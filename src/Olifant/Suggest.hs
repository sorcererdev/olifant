-- | "Did you mean": the nearest candidate to a mistyped word.
module Olifant.Suggest (closest) where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T

-- | The candidate nearest to @x@ by edit distance, if it is plausibly a typo.
closest :: Text -> [Text] -> Maybe Text
closest _ [] = Nothing
closest x cs =
  let (best, d) = minimumBy (comparing snd) [(c, distance (T.toLower x) (T.toLower c)) | c <- cs]
   in if d <= T.length best `div` 3 + 1 then Just best else Nothing

distance :: Text -> Text -> Int
distance a b = last (foldl step [0 .. T.length a] (T.unpack b))
  where
    as = T.unpack a
    step prev@(p : ps) c = scanl compute (p + 1) (zip3 as prev ps)
      where
        compute left (ac, diag, up) = minimum [up + 1, left + 1, diag + if ac == c then 0 else 1]
    step [] _ = []
