-- | Internal to Olifant.Recognize: what a project's conventions look like,
-- for one language — each recognisable generator's files (where they go,
-- and what they look like: the template with sample values, and the
-- project's own instances of it, its siblings), and the project's weights.
module Olifant.Recognize.Index
  ( Convention (..)
  , Index (..)
  , buildIndex
  , sampleText
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Char (isUpper)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generator (EditWith (..), Generator, GeneratorId, GeneratorWith (..), generatorName)
import Olifant.Glob (matchesGlob)
import Olifant.Recognize.Paths (PathPattern (..), pathPattern)
import Olifant.Recognize.Shape (Keywords, Shingle, shapes, shingles)
import Olifant.Recognize.Weights (Weights, weightsOf)
import Olifant.Template.Syntax (Expr (..), Segment (..), Template (..))
import System.FilePath (takeDirectory)

-- | One file a generator makes: where (its path pattern), what (its
-- template), and what it looks like ("template" — the template rendered
-- with sample values — and each sibling's path), as patterns.
data Convention = Convention
  { cvGenerator :: GeneratorId
  , cvName :: Text
  , cvPattern :: PathPattern
  , cvTemplate :: Template
  , cvReferences :: [(Text, Set Shingle)]
  }
  deriving stock (Eq, Show)

data Index = Index
  { ixKeywords :: Keywords
  , ixWeights :: Weights
  , ixConventions :: [Convention]
  , -- | the folders each generator's siblings are in
    ixSiblingDirs :: Map GeneratorId (Set FilePath)
  }
  deriving stock (Eq, Show)

-- | The index of one language's generators over that language's files of
-- the project (paths relative to its root). A starter generator (it makes a
-- whole project) and one with `recognize = false` are left out, and so is a
-- file whose template is empty or whose path cannot be inverted.
buildIndex :: Keywords -> [Generator] -> [(FilePath, Text)] -> Index
buildIndex kw gens files =
  Index
    { ixKeywords = kw
    , ixWeights = weightsOf (M.elems patterns)
    , ixConventions = conventions
    , ixSiblingDirs = M.fromListWith S.union [(cvGenerator c, S.fromList (map (takeDirectory . T.unpack . fst) (siblingsOf c))) | c <- conventions]
    }
  where
    patterns = M.fromList [(p, shingles (shapes kw t)) | (p, t) <- files]
    conventions =
      [ Convention (genId g) (generatorName g) pat src (("template", shingles (shapes kw (sampleText src))) : siblings)
      | g <- gens
      , genRecognize g && not (genStarter g)
      , Create {editTarget = target, editSource = src} <- genEdits g
      , not (T.null (T.strip (templateSource src)))
      , Just pat <- [pathPattern (genDerived g) target]
      , let siblings = [(T.pack p, s) | (p, s) <- M.toList patterns, matchesGlob (ppGlob pat) p]
      ]
    siblingsOf c = filter ((/= "template") . fst) (cvReferences c)

-- | A template rendered for its shape: names as @SampleThing@ (an
-- expression whose last filter is pascal, or a variable starting upper
-- case) or @sample_thing@, holes as @hole_value@, a condition's first
-- branch, a loop's body once.
sampleText :: Template -> Text
sampleText = T.concat . map segment . templateSegments
  where
    segment = \case
      Lit t -> t
      Out e -> if pascal e then "SampleThing" else "sample_thing"
      Hole {} -> "hole_value"
      If ((_, body) : _) _ -> T.concat (map segment body)
      If [] els -> T.concat (map segment els)
      For _ _ body -> T.concat (map segment body)
    pascal = \case
      Filter _ f -> f == "pascal"
      Var v -> maybe False (isUpper . fst) (T.uncons v)
      Field e _ -> pascal e
      _ -> False
