module Olifant.Template.Syntax
  ( Template (..)
  , Segment (..)
  , Expr (..)
  , Op (..)
  , freeVariables
  ) where

import Data.Set (Set)
import Data.Set qualified as S
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
  | -- | a typed hole (spec §6.1): name, kind, instruction
    Hole Text Text [Segment]
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

-- | Variables a template reads from its environment (not @for@-bound names or @loop@).
freeVariables :: Template -> Set Text
freeVariables = foldMap (segmentVars S.empty) . templateSegments
  where
    segmentVars bound = \case
      Lit _ -> S.empty
      Out e -> exprVars bound e
      Hole _ _ body -> foldMap (segmentVars bound) body
      If branches els ->
        foldMap (\(c, body) -> exprVars bound c <> foldMap (segmentVars bound) body) branches
          <> foldMap (segmentVars bound) els
      For v e body -> exprVars bound e <> foldMap (segmentVars (S.insert "loop" (S.insert v bound))) body
    exprVars bound = \case
      Var x
        | x `S.member` bound -> S.empty
        | otherwise -> S.singleton x
      Field e _ -> exprVars bound e
      Not e -> exprVars bound e
      BinOp _ a b -> exprVars bound a <> exprVars bound b
      Filter e _ -> exprVars bound e
      _ -> S.empty
