-- | `.olifant/vet.md`, the steering file a project writes for vet: all of it
-- goes to the finder; each heading under `## Rules` is a rule a finding can
-- cite (its text is what Jev reads for that finding); `## A nitpick is` and
-- `## Worth saying is` replace the two sides of Jev's nitpick question.
module Olifant.Vet.Steering
  ( Steering (..)
  , steering
  , noSteering
  , ruleText
  , rulesSection
  , steeringPath
  ) where

import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T

data Steering = Steering
  { stText :: Text
  , -- | (name, text) in file order; a duplicate name keeps both, the first wins in 'ruleText'
    stRules :: [(Text, Text)]
  , stNitpick :: Maybe Text
  , stWorth :: Maybe Text
  }
  deriving stock (Eq, Show)

steeringPath :: FilePath
steeringPath = ".olifant/vet.md"

noSteering :: Steering
noSteering = Steering "" [] Nothing Nothing

-- | A heading line's level and title (`### A` is (3, "A")).
heading :: Text -> Maybe (Int, Text)
heading l =
  let (hashes, rest) = T.span (== '#') l
   in if not (T.null hashes) && (T.null rest || isSpace (T.head rest)) then Just (T.length hashes, T.strip rest) else Nothing

-- | The file's sections: each heading with the lines under it, up to the next heading of any level.
sections :: Text -> [((Int, Text), [Text])]
sections src = go (T.lines src)
  where
    go [] = []
    go (l : ls) = case heading l of
      Just h -> let (body, rest) = break ((/= Nothing) . heading) ls in (h, body) : go rest
      Nothing -> go ls

body :: [Text] -> Text
body = T.strip . T.unlines

steering :: Text -> Steering
steering src = Steering src rules (section "A nitpick is") (section "Worth saying is")
  where
    ss = sections src
    rules = [(name, body b) | ((lvl, name), b) <- inRules, lvl > 2]
    inRules = takeWhile ((> 2) . fst . fst) (drop 1 (dropWhile (not . isRules . fst) ss))
    isRules (lvl, name) = lvl == 2 && name == "Rules"
    section name = case [body b | ((2, n), b) <- ss, n == name] of
      t : _ | not (T.null t) -> Just t
      _ -> Nothing

ruleText :: Steering -> Text -> Maybe Text
ruleText st name = lookup name (stRules st)

-- | `## Rules` and everything under it, up to the next `##` or `#` heading; empty without one.
rulesSection :: Steering -> Text
rulesSection st =
  let ls = T.lines (stText st)
      from = dropWhile ((/= Just (2, "Rules")) . heading) ls
      ends l = maybe False ((<= 2) . fst) (heading l)
   in case from of
        [] -> ""
        h : rest -> T.stripEnd (T.unlines (h : takeWhile (not . ends) rest))
