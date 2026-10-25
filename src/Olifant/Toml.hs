-- | TOML parsed into a neutral tree, plus small decoders with readable errors.
-- This is the only module that knows the toml-parser API.
module Olifant.Toml
  ( Tree (..)
  , Table
  , parseToml
  , keyOrder
  , required
  , optional
  , inKey
  , text
  , int
  , number
  , bool
  , list
  , table
  , onlyKeys
  , onlyKeysWith
  , describeTree
  ) where

import Control.Monad (zipWithM)
import Data.Bifunctor (first)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Suggest (closest)
import Toml qualified

data Tree = TText Text | TInt Integer | TDouble Double | TBool Bool | TList [Tree] | TTable Table | TOther Text
  deriving stock (Eq, Show)

type Table = Map Text Tree

parseToml :: Text -> Either Text Table
parseToml src = case Toml.parse src of
  Left err -> Left (T.pack err)
  Right tbl -> Right (fromTable tbl)
  where
    fromTable :: Toml.Table' a -> Table
    fromTable (Toml.MkTable m) = fmap (fromValue . snd) m
    fromValue :: Toml.Value' a -> Tree
    fromValue = \case
      Toml.Text' _ t -> TText t
      Toml.Integer' _ i -> TInt i
      Toml.Double' _ d -> TDouble d
      Toml.Bool' _ b -> TBool b
      Toml.List' _ xs -> TList (map fromValue xs)
      Toml.Table' _ t -> TTable (fromTable t)
      other -> TOther (T.pack (Toml.valueType other))

-- | The keys of the table at @path@ in the order they appear in the source
-- ('Table' is a map, so it forgets). Empty if the path is not a table.
keyOrder :: Text -> [Text] -> [Text]
keyOrder src path = case Toml.parse src of
  Left _ -> []
  Right tbl -> go path tbl
  where
    go [] (Toml.MkTable m) = map fst (sortOn (Toml.posIndex . fst . snd) (M.toList m))
    go (k : ks) (Toml.MkTable m) = case M.lookup k m of
      Just (_, Toml.Table' _ inner) -> go ks inner
      _ -> []

inKey :: Text -> Either Text a -> Either Text a
inKey k = first (\e -> "`" <> k <> "`: " <> e)

required :: Text -> (Tree -> Either Text a) -> Table -> Either Text a
required k f t = maybe (Left ("missing `" <> k <> "`")) (inKey k . f) (M.lookup k t)

optional :: Text -> (Tree -> Either Text a) -> Table -> Either Text (Maybe a)
optional k f t = traverse (inKey k . f) (M.lookup k t)

text :: Tree -> Either Text Text
text = \case
  TText t -> Right t
  other -> Left ("expected a string, got " <> describeTree other)

int :: Tree -> Either Text Integer
int = \case
  TInt i -> Right i
  other -> Left ("expected an integer, got " <> describeTree other)

-- | A float, or an integer read as one (`input = 2` means 2.0).
number :: Tree -> Either Text Double
number = \case
  TDouble d -> Right d
  TInt i -> Right (fromInteger i)
  other -> Left ("expected a number, got " <> describeTree other)

bool :: Tree -> Either Text Bool
bool = \case
  TBool b -> Right b
  other -> Left ("expected true or false, got " <> describeTree other)

list :: (Tree -> Either Text a) -> Tree -> Either Text [a]
list f = \case
  TList xs -> zipWithM (\i x -> first (\e -> "[" <> T.pack (show i) <> "] " <> e) (f x)) [1 :: Int ..] xs
  other -> Left ("expected an array, got " <> describeTree other)

table :: Tree -> Either Text Table
table = \case
  TTable t -> Right t
  other -> Left ("expected a table, got " <> describeTree other)

onlyKeys :: [Text] -> Table -> Either Text ()
onlyKeys = onlyKeysWith []

-- | Like 'onlyKeys', with advice for keys people (and models) commonly write
-- by mistake; any other unknown key gets the nearest allowed one, if close.
onlyKeysWith :: [(Text, Text)] -> [Text] -> Table -> Either Text ()
onlyKeysWith advice allowed t = case filter (`notElem` allowed) (M.keys t) of
  [] -> Right ()
  unknown ->
    Left ("unknown key(s) " <> T.intercalate ", " (map explain unknown) <> "; expected one of " <> T.intercalate ", " (map tick allowed))
  where
    tick k = "`" <> k <> "`"
    explain k = case lookup k advice of
      Just why -> tick k <> ": " <> why
      Nothing -> tick k <> maybe "" (\c -> ": did you mean " <> tick c <> "?") (closest k allowed)

describeTree :: Tree -> Text
describeTree = \case
  TText _ -> "a string"
  TInt _ -> "an integer"
  TDouble _ -> "a float"
  TBool _ -> "a boolean"
  TList _ -> "an array"
  TTable _ -> "a table"
  TOther o -> "a " <> o
