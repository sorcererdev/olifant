module Olifant.Template.Exemplar
  ( Binding (..)
  , ExemplarError (..)
  , describeExemplarError
  , exemplar
  ) where

import Control.Monad (foldM)
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper, isDigit)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Naming (Case (Snake), Name, allCases, caseName, nameWords, pluralize, render)
import Olifant.Template.Syntax

-- | An input whose example value appears in the exemplar file.
data Binding = Binding {bindingInput :: Text, bindingExample :: Name}
  deriving stock (Eq, Show)

data ExemplarError
  = ExampleNeedsTwoWords Text Name
  | AmbiguousForm Text
  deriving stock (Eq, Show)

describeExemplarError :: ExemplarError -> Text
describeExemplarError = \case
  ExampleNeedsTwoWords input n ->
    "bind `" <> input <> "`: example `" <> render Snake n
      <> "` needs at least two words so that every case form is distinct"
  AmbiguousForm t ->
    "exemplar form `" <> t <> "` comes from more than one binding; choose more distinctive example names"

-- | Turn a real file into a template by replacing each binding's example
-- (in every case form, singular and plural) with the bound input.
exemplar :: [Binding] -> Text -> Either ExemplarError Template
exemplar bindings src = do
  mapM_ checkWords bindings
  forms <- foldM addForm M.empty (concatMap formsOf bindings)
  pure (Template src (toSegments (sortOn (negate . T.length . fst) (M.toList forms)) src))
  where
    checkWords (Binding input ex)
      | length (nameWords ex) >= 2 = Right ()
      | otherwise = Left (ExampleNeedsTwoWords input ex)
    addForm m (t, e) = case M.lookup t m of
      Just e' | e' /= e -> Left (AmbiguousForm t)
      _ -> Right (M.insert t e m)

formsOf :: Binding -> [(Text, Expr)]
formsOf (Binding input ex) =
  [ (render c n, Filter base (caseName c))
  | (n, base) <- [(ex, Var input), (pluralize ex, Filter (Var input) "plural")]
  , c <- allCases
  ]

-- | Scan left to right, preferring the longest form at each position.
toSegments :: [(Text, Expr)] -> Text -> [Segment]
toSegments forms = go Nothing []
  where
    go prev lit rest = case T.uncons rest of
      Nothing -> flush lit []
      Just (c, rest') -> case matchAt prev rest of
        Just (form, e) -> flush lit (Out e : go (Just (T.last form)) [] (T.drop (T.length form) rest))
        Nothing -> go (Just c) (c : lit) rest'
    matchAt prev rest =
      listToMaybe
        [ (form, e)
        | (form, e) <- forms
        , form `T.isPrefixOf` rest
        , boundaryBefore prev form
        , boundaryAfter form (T.drop (T.length form) rest)
        ]
    flush [] segs = segs
    flush lit segs = Lit (T.pack (reverse lit)) : segs

-- | A form starts a word after a non-alphanumeric character, or at a camel
-- hump (lowercase/digit followed by an uppercase-initial form).
boundaryBefore :: Maybe Char -> Text -> Bool
boundaryBefore Nothing _ = True
boundaryBefore (Just p) form =
  not (isAlphaNum p) || (isAsciiUpper (T.head form) && (isAsciiLower p || isDigit p))

-- | A form ends a word before a non-alphanumeric character, or before an
-- uppercase letter (the next hump) unless the form itself is all caps.
boundaryAfter :: Text -> Text -> Bool
boundaryAfter form next = case T.uncons next of
  Nothing -> True
  Just (n, _) -> not (isAlphaNum n) || (isAsciiUpper n && T.any isAsciiLower form)
