-- | Inspect's screen: is a changed unit obviously unfinished? Neutral code facts
-- (a parse error, a TODO or FIXME word, an empty body) and Jev's
-- "stops early". No language holds a placeholder list (ruling at the 1a
-- checkpoint): a body of `pass` is Jev's to judge.
module Olifant.Inspect.Screen
  ( Facts (..)
  , unitFacts
  , stopsEarly
  , unfinished
  , screenCut
  ) where

import Data.Char (isAlphaNum)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Decide (Question (..), Shape (..), noul)
import Olifant.Language (Language)
import Olifant.Language.Captures (DefSpan (..), PNode (..))
import Olifant.Language.TreeSitter (bodyEmpty, definitionsOf, errorLines, loadableGrammar)
import Olifant.Server.Unit (Named (..))

data Facts = Facts {fErrors :: Bool, fTodo :: Bool, fEmpty :: Bool}
  deriving stock (Eq, Show)

-- | A unit's facts in the file's text (1-based, inclusive lines).
unitFacts :: Language -> Text -> Named -> Facts
unitFacts lang text n = Facts errors todo empty
  where
    (a, z) = namedLines n
    unitText = T.unlines (take (z - a + 1) (drop (a - 1) (T.lines text)))
    grammar = loadableGrammar lang
    errors = maybe False (any (\l -> l >= a && l <= z) . (`errorLines` text)) grammar
    todo = any (`elem` ["TODO", "FIXME"]) (T.split (\c -> not (isAlphaNum c || c == '_')) unitText)
    -- the definition on the unit's def line, and its body node
    def = listToMaybe [d | Just ds <- [definitionsOf lang text], d <- ds, fst (pStart (dsNode d)) + 1 == namedDef n]
    empty = case (grammar, def >>= dsBody) of
      (Just g, Just body) -> bodyEmpty g text (pBytes body) == Just True
      _ -> False

-- | P(stops early) at or past this is unfinished (the 1a rule).
screenCut :: Double
screenCut = 0.80

unfinished :: Facts -> Double -> Bool
unfinished (Facts e t m) p = e || t || m || p >= screenCut

-- | Jev's question, worded as 1a measured it: language, file, the unit's
-- text, the enclosing class or module line (empty when none), and the facts.
stopsEarly :: Text -> FilePath -> Text -> Text -> Facts -> Question
stopsEarly lang file unitText header (Facts e t m) =
  (noul "stops-early" "Does the body of this function stop before doing what its name, signature and documentation promise?" ctx)
    { qShape = Noul (Just ("the body ends before it produces or returns what the function is for", "the body does what the function is for, however briefly"))
    , qUnsure = False
    }
  where
    yes b = if b then "yes" else "no"
    ctx =
      [ ("language", lang)
      , ("file", T.pack file)
      , ("function text", unitText)
      , ("enclosing class or module", header)
      , ("code facts", "syntax errors: " <> yes e <> "; TODO or FIXME: " <> yes t <> "; placeholder body: " <> yes m)
      ]
