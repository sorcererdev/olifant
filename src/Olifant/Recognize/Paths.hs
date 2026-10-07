-- | Internal to Olifant.Recognize: a generator's output path as a glob (to
-- find the project's own instances of the convention) and inverted (a path
-- someone created gives back the inputs that would have rendered it).
module Olifant.Recognize.Paths
  ( PathPattern (..)
  , PathPart (..)
  , pathPattern
  , invertPath
  ) where

import Data.Char (isAlphaNum)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Naming (Case (Snake), caseFromText, parseName, render)
import Olifant.Template.Syntax (Expr (..), Segment (..), Template (..))

-- | The glob of the files a path makes, and its parts: literal text, and
-- inputs with the case they are rendered in.
data PathPattern = PathPattern {ppGlob :: Text, ppParts :: [PathPart]}
  deriving stock (Eq, Show)

data PathPart = PLit Text | PVar Text (Maybe Case)
  deriving stock (Eq, Show)

-- | A create path with its derived names followed down to inputs; nothing
-- for a path it cannot invert (a condition, a loop, a filter that is not a
-- case, a derived value too deep).
pathPattern :: [(Text, Template)] -> Template -> Maybe PathPattern
pathPattern derived t = do
  parts <- merge <$> expand (0 :: Int) (templateSegments t)
  pure (PathPattern (T.concat [either id (const "*") (asLit p) | p <- parts]) parts)
  where
    defs = M.fromList derived
    asLit = \case
      PLit l -> Left l
      v -> Right v
    expand depth = fmap concat . traverse (segment depth)
    segment depth = \case
      Lit l -> Just [PLit l]
      Out e -> do
        (v, fs) <- chain e
        out depth v fs
      _ -> Nothing
    out depth v fs
      | depth > 5 = Nothing
      | Just d <- M.lookup v defs = case (fs, templateSegments d) of
          ([], segs) -> expand (depth + 1) segs
          (_, [Out e']) -> chain e' >>= \(v', fs') -> out (depth + 1) v' (fs' <> fs)
          _ -> Nothing
      | otherwise = (\c -> [PVar v c]) <$> caseOf fs
    chain = \case
      Var v -> Just (v, [])
      Filter e f -> (\(v, fs) -> (v, fs <> [f])) <$> chain e
      _ -> Nothing
    caseOf fs = case traverse caseFromText fs of
      Nothing -> Nothing
      Just cs -> Just (if null cs then Nothing else Just (last cs))
    merge = \case
      PLit a : PLit b : rest -> merge (PLit (a <> b) : rest)
      p : rest -> p : merge rest
      [] -> []

-- | The inputs a path gives back, in snake case (each value within one path
-- segment); nothing when it does not match or a repeated input disagrees.
invertPath :: PathPattern -> FilePath -> Maybe [(Text, Text)]
invertPath pat = go (ppParts pat) [] . T.pack
  where
    go [] acc rest = if T.null rest then Just (reverse acc) else Nothing
    go (PLit l : parts) acc rest = T.stripPrefix l rest >>= go parts acc
    go (PVar v c : parts) acc rest =
      listToMaybe
        [ r
        | (value, after) <- splits rest
        , Just decoded <- [decode c value]
        , maybe True (== decoded) (lookup v acc)
        , Just r <- [go parts (if v `elem` map fst acc then acc else (v, decoded) : acc) after]
        ]
    -- shortest first, never across a path segment
    splits s =
      let word = T.takeWhile (\x -> isAlphaNum x || x == '_' || x == '-') s
       in [(T.take n s, T.drop n s) | n <- [1 .. T.length word]]
    decode Nothing value = Just value
    decode (Just _) value = either (const Nothing) (Just . render Snake) (parseName value)
