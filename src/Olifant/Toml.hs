-- | TOML parsed into a neutral tree, plus small decoders with readable errors.
-- This is the only module that knows the toml-parser API.
module Olifant.Toml
  ( Tree (..)
  , Table
  , parseToml
  , required
  , optional
  , inKey
  , text
  , int
  , bool
  , list
  , table
  , onlyKeys
  , describeTree
  ) where

import Control.Monad (zipWithM)
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Toml qualified

data Tree = TText Text | TInt Integer | TBool Bool | TList [Tree] | TTable Table | TOther Text
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
      Toml.Bool' _ b -> TBool b
      Toml.List' _ xs -> TList (map fromValue xs)
      Toml.Table' _ t -> TTable (fromTable t)
      other -> TOther (T.pack (Toml.valueType other))

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
onlyKeys allowed t = case filter (`notElem` allowed) (M.keys t) of
  [] -> Right ()
  unknown ->
    Left ("unknown key(s) " <> T.intercalate ", " (map tick unknown) <> "; expected one of " <> T.intercalate ", " (map tick allowed))
  where
    tick k = "`" <> k <> "`"

describeTree :: Tree -> Text
describeTree = \case
  TText _ -> "a string"
  TInt _ -> "an integer"
  TBool _ -> "a boolean"
  TList _ -> "an array"
  TTable _ -> "a table"
  TOther o -> "a " <> o
