-- | Tree-sitter captures as plain data ('PNode', 'Cap'), read by
-- 'Olifant.Language.TreeSitter' and by the generic anchor rule.
module Olifant.Language.Captures
  ( PNode (..)
  , Cap (..)
  ) where

import Data.Text (Text)

-- | A node, as the rule needs it: its type, the (line, column) of its start
-- and end (0-based, columns in characters, as 'Olifant.Language.Syntax.Match'
-- counts them), its byte range in the UTF-8 text, its text, and whether it is
-- named and whether it is extra (a comment).
data PNode = PNode {pType :: Text, pStart :: (Int, Int), pEnd :: (Int, Int), pBytes :: (Int, Int), pText :: Text, pNamed :: Bool, pExtra :: Bool}
  deriving stock (Eq, Ord, Show)

-- | A capture: its name, the query file and pattern, the match it is in,
-- its node, the node's parent and top-level ancestor, and the fields the
-- rule reads (`body`, `parameters`, `arguments`) with their named,
-- non-extra children.
data Cap = Cap
  { capName :: Text
  , capFile :: Text
  , capPattern :: Int
  , capMatch :: Int
  , capNode :: PNode
  , capParent :: Maybe PNode
  , capTop :: Maybe PNode
  , capFields :: [(Text, PNode, [PNode])]
  }
  deriving stock (Eq, Show)
