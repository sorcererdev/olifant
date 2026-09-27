module Olifant.Template.Value
  ( Value (..)
  , Env
  , truthy
  , valuesEqual
  , describeValue
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Olifant.Naming (Case (Snake), Name, render)

data Value
  = VText Text
  | VName Name
  | VBool Bool
  | VInt Integer
  | VTime UTCTime
  | VList [Value]
  | VRecord (Map Text Value)
  | VNull
  deriving stock (Eq, Show)

type Env = Map Text Value

truthy :: Value -> Bool
truthy = \case
  VBool b -> b
  VText t -> not (T.null t)
  VList xs -> not (null xs)
  VInt i -> i /= 0
  VNull -> False
  _ -> True

-- | Equality as templates see it: a name equals text spelling it in snake case.
valuesEqual :: Value -> Value -> Bool
valuesEqual (VName a) (VText t) = render Snake a == t
valuesEqual (VText t) (VName a) = render Snake a == t
valuesEqual a b = a == b

describeValue :: Value -> Text
describeValue = \case
  VText _ -> "text"
  VName _ -> "a name"
  VBool _ -> "a bool"
  VInt _ -> "an integer"
  VTime _ -> "a time"
  VList _ -> "a list"
  VRecord _ -> "a record"
  VNull -> "nothing"
