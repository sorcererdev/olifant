-- | Convention recognition (spec: convention-recognition-design.md): when
-- someone hand-writes what a project's convention produces — especially
-- someone who does not know it — which generator it is, how sure, and with
-- which inputs. Place is the strongest signal; shape (token patterns,
-- weighted by rarity) decides how loud; same-shaped conventions are left to
-- a choice, or a model (the spike, 2026-10-06).
module Olifant.Recognize
  ( module Olifant.Recognize.Shape
  , module Olifant.Recognize.Weights
  , module Olifant.Recognize.Paths
  , module Olifant.Recognize.Index
  , module Olifant.Recognize.Inputs
  , module Olifant.Recognize.Types
  , module Olifant.Recognize.Ask
  , recognizeIn
  ) where

import Olifant.Recognize.Ask
import Olifant.Recognize.Types
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust, listToMaybe)
import Data.Ord (Down (..))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generator (Generator, GeneratorWith (..))
import Olifant.Recognize.Index
import Olifant.Recognize.Inputs
import Olifant.Recognize.Paths
import Olifant.Recognize.Shape
import Olifant.Recognize.Weights
import System.FilePath (takeDirectory, takeFileName)

data Scored = Scored {scConvention :: Convention, scShape :: Double, scPlace :: Double, scLike :: Text}

total :: Scored -> Double
total s = min 1 (scShape s + scPlace s)

-- | What a change is, if anything: the best convention for it, banded by
-- place — at its place (its output path, or beside its siblings) and total
-- ≥ 0.5: an offer; off its place with shape ≥ 0.55: a hint (off its place,
-- a value object and a model are the same shape); else nothing. A
-- runner-up of another generator within 0.1 (its shape ≥ 0.4) makes it a
-- choice.
recognizeIn :: Index -> [Generator] -> Change -> Maybe Recognition
recognizeIn ix gens (Change path text)
  | length (filter (not . T.null . T.strip) (T.lines text)) < 3 = Nothing
  | length (filter nameShingle (S.toList mine)) < 2 = Nothing
  | otherwise = do
      -- the most likely convention that qualifies (the place may favour one
      -- whose shape does not fit, while another's shape does)
      (best, band) <- listToMaybe [(s, b) | s <- byGenerator, Just b <- [bandOf s]]
      let rest = filter ((/= cvGenerator (scConvention best)) . cvGenerator . scConvention) byGenerator
      g <- M.lookup (cvGenerator (scConvention best)) generators
      let c = scConvention best
          others = [s | s <- rest, cvGenerator (scConvention s) /= cvGenerator c]
          choice = listToMaybe [cvGenerator (scConvention s) | s <- others, total best - total s <= 0.1, scShape s >= 0.4]
          rival = listToMaybe [(cvGenerator (scConvention s), scShape s) | s <- sortOn (Down . scShape) others, scShape s > scShape best]
      pure
        Recognition
          { rcGenerator = cvGenerator c
          , rcName = cvName c
          , rcBand = band
          , rcScore = total best
          , rcShape = scShape best
          , rcPlace = scPlace best
          , rcLike = scLike best
          , rcInputs = validInputs g (inputsFor g c path text)
          , rcOr = choice
          , rcRival = rival
          , rcReasons = reasons best
          }
  where
    mine = shingles (shapes (ixKeywords ix) text) `S.difference` wBoilerplate (ixWeights ix)
    generators = M.fromList [(genId g, g) | g <- gens]
    scored = map score (ixConventions ix)
    -- each generator's best file (a command's module, not its test, for a module)
    byGenerator = sortOn (\s -> Down (total s, scShape s)) (M.elems (M.fromListWith better [(cvGenerator (scConvention s), s) | s <- scored]))
    better a b = if (total a, scShape a) >= (total b, scShape b) then a else b
    -- a convention about a struct (a class, a fn) is not what code declaring
    -- none is, wherever it sits: an enum in a service's src/ is no command
    score c
      | Just g <- M.lookup (cvGenerator c) generators
      , kinds <- primaryKinds g c
      , not (null kinds)
      , not (declaresAny kinds text) =
          Scored c 0 0 "the template"
      | otherwise =
      let refs = [(name, containment (ixWeights ix) mine r) | (name, r) <- cvReferences c, T.unpack name /= path]
          (like, shape) = maybe ("the template", 0) id (listToMaybe (sortOn (Down . snd) refs))
          place
            | isJust (invertPath (cvPattern c) path) = 0.25
            | takeDirectory path `S.member` M.findWithDefault S.empty (cvGenerator c) (ixSiblingDirs ix) = 0.15
            | otherwise = 0
       in Scored c shape place like
    bandOf s
      | scPlace s > 0 && total s >= 0.5 = Just Offer
      | scPlace s == 0 && scShape s >= 0.55 = Just Hint
      | otherwise = Nothing
    reasons s =
      let c = scConvention s
          siblings = length [() | (n, _) <- cvReferences c, n /= "template"]
          placed = if scPlace s > 0 then ["beside " <> T.pack (show siblings) <> " " <> T.toLower (cvName c) <> (if siblings == 1 then "" else "s") | siblings > 0] else []
          liked = if scLike s == "template" then "the template" else T.pack (takeFileName (T.unpack (scLike s)))
       in placed <> [T.pack (show (round (scShape s * 100) :: Int)) <> "% shaped like " <> liked]
