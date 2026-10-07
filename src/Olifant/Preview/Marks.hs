-- | Internal to Olifant.Preview: test the marks through that unit's public module.
--
-- A preview plans a generator before every input is set. A missing name is
-- planned as a mark (@zqmark_command@), whatever case or number the
-- templates render it in, and every rendering comes back as a placeholder
-- (@⟨command⟩@, @⟨Command⟩@, @⟨commands⟩@).
module Olifant.Preview.Marks (markValue, unmark) where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Naming (Case (..), Name, parseName, pluralize, render, singularize)

-- | The value a missing name input is planned with: words no real code has.
markValue :: Text -> Text
markValue name = "zqmark_" <> name

-- | Every rendering of each missing input's mark, as a placeholder in the
-- same family of case: @⟨Command⟩@ for upper-first, @⟨COMMAND⟩@ for constant,
-- @⟨command⟩@ otherwise; plural and singular forms too. Longest first, so
-- a shorter rendering never splits a longer one.
unmark :: [Text] -> Text -> Text
unmark names text = foldl (\t (from, to) -> T.replace from to t) text (sortOn (Down . T.length . fst) pairs)
  where
    pairs =
      [ (render c (form m), "⟨" <> render (shown c) (form n) <> "⟩")
      | name <- names
      , Right m <- [parseName (markValue name)]
      , Right n <- [parseName name]
      , form <- [id, pluralize, singularize] :: [Name -> Name]
      , c <- [minBound .. maxBound]
      ]
    shown = \case
      Pascal -> Pascal
      Title -> Pascal
      Sentence -> Pascal
      Constant -> Constant
      _ -> Snake
