-- | Tree-sitter captures as plain data ('PNode', 'Cap'), read by
-- 'Olifant.Language.TreeSitter', and the generic anchor rule over them
-- (design 4b §A.3, as spike A amended it): /`end of <kind> NAME` is the
-- `.inner` range of the `@definition` named NAME; the insertion point is
-- after its last named child, at that child's column; an empty body is
-- opened./ Here that is each container (the inner nodes' parent, or the
-- definition's `body` when there are none) with its items (the named,
-- non-extra inner nodes), which the existing placement
-- ('Olifant.Anchor.Resolve') turns into a site.
module Olifant.Language.Captures
  ( PNode (..)
  , Cap (..)
  , anchorFromCaptures
  , definedNames
  , oneContainer
  , genericPhrase
  , genericPhrases
  , renamedPhrases
  , phraseKind
  , functionCaps
  , classCaps
  ) where

import Data.ByteString qualified as BS
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Olifant.Language.Syntax (Match (..))

-- | A node, as the rule needs it: its type, the (line, column) of its start
-- and end (0-based, columns in characters, as 'Olifant.Language.Syntax.Match'
-- counts them), its byte range in the UTF-8 text, its text, and whether it is
-- named and whether it is extra (a comment).
data PNode = PNode {pType :: Text, pStart :: (Int, Int), pEnd :: (Int, Int), pBytes :: (Int, Int), pText :: Text, pNamed :: Bool, pExtra :: Bool}
  deriving stock (Eq, Ord, Show)

-- | A capture: its name, the query file and pattern, the match it is in,
-- its node, the node's parent and top-level ancestor, and the fields the
-- rule reads (a definition's `body` and `parameters`, a call's
-- `arguments`) with their named, non-extra children (none for a `body`:
-- its items are the inner captures).
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

-- | The phrases the engine ships, with an example of each (for `where`,
-- doctor and "did you mean").
genericPhrases :: [(Text, Text)]
genericPhrases =
  [ ("function NAME", "end of function main")
  , ("class NAME", "end of class BillingService")
  , ("KIND NAME", "end of struct BillingService")
  , ("parameters in FUNCTION", "end of parameters in new")
  , ("call NAME", "end of call BillingService::new")
  , ("call NAME in FUNCTION", "end of call BillingService::new in main")
  , ("imports", "end of imports")
  ]

-- | Phrases a declared target used to answer that a generic phrase now
-- does, under another name (4b m2: Rust's `uses`): "did you mean" offers
-- the new one, however far apart the spellings are.
renamedPhrases :: [([Text], Text)]
renamedPhrases = [(["uses"], "end of imports")]

-- | Does a phrase's target take the generic rule's shape? (`KIND NAME` for
-- any kind word; the rest by their words.) The shape only: whether a kind
-- word names a node type is the grammar's to say ('phraseKind').
genericPhrase :: [Text] -> Bool
genericPhrase = \case
  ["imports"] -> True
  ["parameters", "in", _] -> True
  ["call", _] -> True
  ["call", _, "in", _] -> True
  [_, _] -> True
  _ -> False

-- | The kind word of a `KIND NAME` phrase other than `function` and
-- `class`, which must name a node type of the grammar (or a prefix of one,
-- before `_`): 'Nothing' for the other shapes.
phraseKind :: [Text] -> Maybe Text
phraseKind = \case
  ["call", _] -> Nothing
  [kind, _] | kind `notElem` ["function", "class"] -> Just kind
  _ -> Nothing

-- | The engine's table: which captures a phrase word names. `function`: a
-- function or method definition; `class`: any class-like one (a class,
-- interface, module, type, and an inherent impl from olifant's impls.scm).
-- Any other word narrows to the definitions whose node type is that word or
-- starts with it and `_` (`struct` → struct_item, `mod` → mod_item).
functionCaps, classCaps :: [Text]
functionCaps = ["definition.function", "definition.method"]
classCaps = ["definition.class", "definition.interface", "definition.module", "definition.type", "definition.impl"]

data Def = Def {defCap :: Cap, defName :: Text}

-- | Definitions: a definition capture with the @name of its match.
definitions :: [Cap] -> [Def]
definitions caps =
  [ Def c (pText (capNode n))
  | c <- caps
  , capName c `elem` functionCaps ++ classCaps
  , Just n <- [M.lookup (capMatch c) names]
  ]
  where
    names = M.fromList [(capMatch n, n) | n <- caps, capName n == "name"]

-- | The names of the definitions a phrase word takes (`function`, `class`,
-- or a kind word), one per definition in the file's order (a struct and its
-- impl: the name twice): what doctor tries the phrase on.
definedNames :: Text -> [Cap] -> [Text]
definedNames kind caps = map snd (nubOn fst [(pBytes (capNode (defCap d)), defName d) | d <- sortOn (pBytes . capNode . defCap) (definitions caps), wanted (defCap d)])
  where
    nubOn f = foldr (\x acc -> x : filter ((/= f x) . f) acc) []
    wanted c = case kind of
      "function" -> capName c `elem` functionCaps
      "class" -> capName c `elem` classCaps
      w -> pType (capNode c) == w || (w <> "_") `T.isPrefixOf` pType (capNode c)

-- | The generic rule. 'Nothing': not a generic phrase. Else every container
-- the phrase names, in the file's order, each with its items ('Right []':
-- no such definition). @root@ is the file's root node.
anchorFromCaptures :: PNode -> [Cap] -> [Text] -> Maybe (Either Text [(Match, [Match])])
anchorFromCaptures root caps = \case
  -- after the last top-level import (imports.scm, olifant's own)
  ["imports"] ->
    -- top-level import statements only (a `use` inside `mod tests` is not one)
    let tops = nub [capNode c | c <- caps, capName c == "import", capTop c == Just (capNode c)]
     in Just (Right [(toMatch root, map toMatch (sortOn pBytes tops)) | not (null tops)])
  ["parameters", "in", f] -> Just (Right (mapMaybe (\d -> field "parameters" (defCap d) "parameter.inner") (defsOf functionCaps f)))
  ["call", name] -> Just (Right (mapMaybe (\c -> field "arguments" c "parameter.inner") [c | c <- calls, callName c == name]))
  ["call", name, "in", f] ->
    let fs = map (capNode . defCap) (defsOf functionCaps f)
     in Just (Right (mapMaybe (\c -> field "arguments" c "parameter.inner") [c | c <- calls, callName c == name, any (`encloses` capNode c) fs]))
  [kind, name]
    | kind == "function" -> Just (Right (mapMaybe body (defsOf functionCaps name)))
    | kind == "class" -> Just (Right (mapMaybe body (defsOf classCaps name)))
    | otherwise -> Just (Right (mapMaybe body [d | d <- defsOf (functionCaps ++ classCaps) name, kindOf kind (capNode (defCap d))]))
  _ -> Nothing
  where
    defs = definitions caps
    defNodes = nub (map (capNode . defCap) defs)
    -- one definition node may carry several captures (method and function)
    defsOf names n = sortOn (pBytes . capNode . defCap) (dedupe [d | d <- defs, defName d == n, capName (defCap d) `elem` names])
    dedupe ds = [d | (i, d) <- zip [0 :: Int ..] ds, capNode (defCap d) `notElem` [capNode (defCap e) | e <- take i ds]]
    kindOf w n = pType n == w || (w <> "_") `T.isPrefixOf` pType n
    -- the `.inner` captures whose nearest strictly enclosing definition is
    -- d, named and not extra; none: the definition's `body`, empty
    body d =
      let dn = capNode (defCap d)
          inners = [c | c <- caps, capName c `elem` ["function.inner", "class.inner"], dn `encloses` capNode c, nearest (capNode c) == Just dn]
          items = nub [capNode c | c <- inners, pNamed (capNode c), not (pExtra (capNode c))]
       in case (items, nub [p | c <- inners, Just p <- [capParent c]]) of
            (_ : _, [p]) -> Just (toMatch p, map groupMatch (decorated (sortOn pBytes items)))
            _ -> fmap (\b -> (toMatch b, [])) (fieldOf "body" (defCap d))
    -- An item that is no definition, ending on the line before a definition
    -- (or on its line), decorates it (an attribute, a decorator): the two
    -- are one item, from the first's start to the definition's end, so
    -- spacing is measured between whole members and `start of` goes before
    -- the decoration.
    decorated = foldr glue []
      where
        glue x ((f, l) : rest)
          | x `notElem` defNodes, l `elem` defNodes, fst (pStart f) <= fst (pEnd x) + 1 = (x, l) : rest
        glue x rest = (x, x) : rest
    groupMatch (f, l)
      | f == l = toMatch f
      | otherwise = let (a, z) = (fst (pBytes f), snd (pBytes l)) in Match (pStart f) (pEnd l) (TE.decodeUtf8Lenient (BS.take (z - a) (BS.drop a source))) (a, z)
    source = TE.encodeUtf8 (pText root)
    -- a node's field (a parameter or argument list) and, as items, the
    -- captures of @name whose parent it is
    field fname c name = case fieldOf fname c of
      Nothing -> Nothing
      Just fnode ->
        let items = nub [capNode i | i <- caps, capName i == name, capParent i == Just fnode, pNamed (capNode i), not (pExtra (capNode i))]
         in Just (toMatch fnode, map toMatch (sortOn pBytes items))
    fieldOf fname c = listToMaybe [n | (f, n, _) <- capFields c, f == fname]
    calls = sortOn (pBytes . capNode) [c | c <- caps, capName c == "call.outer"]
    -- a call's name: its text up to its argument list
    callName c = case fieldOf "arguments" c of
      Just args -> T.strip (TE.decodeUtf8Lenient (BS.take (fst (pBytes args) - fst (pBytes (capNode c))) (TE.encodeUtf8 (pText (capNode c)))))
      Nothing -> ""
    -- nearest strictly enclosing definition node of a node
    nearest n = case sortOn size [dn | dn <- defNodes, dn /= n, dn `encloses` n] of
      d : _ -> Just d
      [] -> Nothing
    size p = snd (pBytes p) - fst (pBytes p)
    encloses a b = fst (pBytes a) <= fst (pBytes b) && snd (pBytes b) <= snd (pBytes a)

toMatch :: PNode -> Match
toMatch p = Match (pStart p) (pEnd p) (pText p) (pBytes p)

-- | The one container a phrase must name (an `end of` site, an `items of`
-- query): 'Right Nothing' for none, 'Left' when it names several.
oneContainer :: [(Match, [Match])] -> Either Text (Maybe (Match, [Match]))
oneContainer = \case
  [] -> Right Nothing
  [c] -> Right (Just c)
  cs -> Left ("matches " <> T.pack (show (length cs)) <> " places; it must match once")
