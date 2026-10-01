-- | `at` phrases (spec §5.2): `<position> <target>`, parsed by a small closed
-- grammar so generators read like sentences but resolve deterministically.
module Olifant.Anchor.Phrase
  ( Position (..)
  , Which (..)
  , Target (..)
  , Phrase (..)
  , parsePhrase
  , showPhrase
  , positionWords
  , closest
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Suggest (closest)

data Position = StartOf | EndOf | Before | After
  deriving stock (Eq, Show, Enum, Bounded)

data Which = FirstLine | LastLine
  deriving stock (Eq, Show)

-- | 'NamedTarget' words are matched against the file's language adapter.
data Target = MarkerTarget Text | LineTarget Which Text | NamedTarget [Text]
  deriving stock (Eq, Show)

data Phrase = Phrase Position Target
  deriving stock (Eq, Show)

positionWords :: Position -> Text
positionWords = \case
  StartOf -> "start of"
  EndOf -> "end of"
  Before -> "before"
  After -> "after"

parsePhrase :: Text -> Either Text Phrase
parsePhrase raw = case [(p, rest) | p <- [minBound .. maxBound], Just rest <- [T.stripPrefix (positionWords p <> " ") phrase]] of
  ((p, rest) : _) -> Phrase p <$> target (T.strip rest)
  []
    | phrase `elem` map positionWords [minBound .. maxBound] -> Left ("`" <> phrase <> "` needs a target, e.g. \"end of type Msg\"")
    | otherwise -> Left ("`" <> phrase <> "`: a phrase starts with start of, end of, before or after (e.g. \"end of type Msg\")" <> suggestion)
  where
    phrase = T.strip raw
    suggestion = maybe "" (\s -> "; did you mean `" <> s <> "`?") (closest (T.unwords (take 2 (T.words phrase))) (map positionWords [minBound .. maxBound]))
    target t
      | T.null t = Left ("`" <> phrase <> "` needs a target, e.g. \"end of type Msg\"")
      | Just m <- T.stripPrefix "marker" t = if T.null (T.strip m) then Left "`marker` needs the marker text, e.g. \"before marker -- olifant:pages\"" else Right (MarkerTarget (T.strip m))
      | Just re <- T.stripPrefix "first line matching " t = line FirstLine re
      | Just re <- T.stripPrefix "last line matching " t = line LastLine re
      | otherwise = Right (NamedTarget (T.words t))
    line w re = if T.null (T.strip re) then Left "`line matching` needs a regular expression" else Right (LineTarget w (T.strip re))

showPhrase :: Phrase -> Text
showPhrase (Phrase p t) = positionWords p <> " " <> case t of
  MarkerTarget m -> "marker " <> m
  LineTarget FirstLine re -> "first line matching " <> re
  LineTarget LastLine re -> "last line matching " <> re
  NamedTarget ws -> T.unwords ws

