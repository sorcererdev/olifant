module Olifant.Template.Syntax
  ( Template (..)
  , Segment (..)
  , Expr (..)
  , Op (..)
  ) where

import Data.Text (Text)

-- | A parsed template; 'templateSource' keeps the original text for display.
data Template = Template {templateSource :: Text, templateSegments :: [Segment]}
  deriving stock (Eq, Show)

data Segment
  = Lit Text
  | Out Expr
  | -- | (condition, body) branches, then the else body
    If [(Expr, [Segment])] [Segment]
  | For Text Expr [Segment]
  deriving stock (Eq, Show)

data Expr
  = Var Text
  | Field Expr Text
  | StrLit Text
  | IntLit Integer
  | BoolLit Bool
  | Not Expr
  | BinOp Op Expr Expr
  | Filter Expr Text
  deriving stock (Eq, Show)

data Op = Equal | NotEqual | And | Or
  deriving stock (Eq, Show)
