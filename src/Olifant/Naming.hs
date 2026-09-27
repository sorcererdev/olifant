-- | Identifiers as lists of lowercase words, rendered in any case convention.
module Olifant.Naming
  ( Name
  , nameWords
  , parseName
  , Case (..)
  , allCases
  , caseName
  , caseFromText
  , render
  , pluralize
  , singularize
  ) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toUpper)
import Data.Text (Text)
import Data.Text qualified as T

-- | A non-empty list of lowercase ASCII words, e.g. ["blog", "post"].
newtype Name = Name [Text]
  deriving stock (Eq, Ord, Show)

nameWords :: Name -> [Text]
nameWords (Name ws) = ws

data Case = Snake | Kebab | Pascal | Camel | Constant | Title | LowerWords
  deriving stock (Eq, Show, Enum, Bounded)

allCases :: [Case]
allCases = [minBound .. maxBound]

caseName :: Case -> Text
caseName = \case
  Snake -> "snake"
  Kebab -> "kebab"
  Pascal -> "pascal"
  Camel -> "camel"
  Constant -> "constant"
  Title -> "title"
  LowerWords -> "lower_words"

caseFromText :: Text -> Maybe Case
caseFromText t = lookup t [(caseName c, c) | c <- allCases]

-- | Parse an identifier written in any supported case.
parseName :: Text -> Either Text Name
parseName raw
  | T.null trimmed = Left "a name cannot be empty"
  | T.any (not . allowed) trimmed =
      Left ("a name may only contain letters, digits, '_', '-' and spaces: `" <> raw <> "`")
  | otherwise = case concatMap splitHumps (T.split isSeparator trimmed) of
      [] -> Left ("a name needs at least one word: `" <> raw <> "`")
      ws@(w : _)
        | isDigit (T.head w) -> Left ("a name cannot start with a digit: `" <> raw <> "`")
        | otherwise -> Right (Name (map T.toLower ws))
  where
    trimmed = T.strip raw
    allowed c = isAsciiLower c || isAsciiUpper c || isDigit c || isSeparator c
    isSeparator c = c == '_' || c == '-' || c == ' '

-- | Split one chunk at case changes: "blogPost" -> ["blog", "Post"],
-- "HTTPServer" -> ["HTTP", "Server"]. All-caps chunks ("BLOG") stay whole.
splitHumps :: Text -> [Text]
splitHumps chunk
  | T.null chunk = []
  | not (T.any isAsciiLower chunk) = [chunk]
  | otherwise = filter (not . T.null) (go (T.unpack chunk) [])
  where
    go [] acc = [T.pack (reverse acc)]
    go (c : rest) acc
      | startsWord c rest acc = T.pack (reverse acc) : go rest [c]
      | otherwise = go rest (c : acc)
    startsWord c rest acc = case acc of
      [] -> False
      (prev : _) ->
        isAsciiUpper c
          && (isAsciiLower prev || isDigit prev || (isAsciiUpper prev && nextIsLower rest))
    nextIsLower (n : _) = isAsciiLower n
    nextIsLower [] = False

render :: Case -> Name -> Text
render c (Name ws) = case c of
  Snake -> T.intercalate "_" ws
  Kebab -> T.intercalate "-" ws
  Pascal -> T.concat (map capitalise ws)
  Camel -> case ws of
    [] -> ""
    (w : rest) -> w <> T.concat (map capitalise rest)
  Constant -> T.toUpper (T.intercalate "_" ws)
  Title -> T.unwords (map capitalise ws)
  LowerWords -> T.unwords ws

capitalise :: Text -> Text
capitalise w = case T.uncons w of
  Nothing -> w
  Just (h, t) -> T.cons (toUpper h) t

pluralize :: Name -> Name
pluralize = onLastWord pluralWord

singularize :: Name -> Name
singularize = onLastWord singularWord

onLastWord :: (Text -> Text) -> Name -> Name
onLastWord f (Name ws) = case reverse ws of
  [] -> Name ws
  (w : rest) -> Name (reverse (f w : rest))

irregulars :: [(Text, Text)]
irregulars =
  [ ("person", "people"), ("man", "men"), ("woman", "women"), ("child", "children")
  , ("tooth", "teeth"), ("foot", "feet"), ("mouse", "mice"), ("goose", "geese"), ("ox", "oxen")
  , ("leaf", "leaves"), ("knife", "knives"), ("life", "lives"), ("wife", "wives"), ("half", "halves")
  , ("wolf", "wolves"), ("shelf", "shelves"), ("criterion", "criteria"), ("datum", "data")
  , ("index", "indices"), ("matrix", "matrices"), ("vertex", "vertices"), ("quiz", "quizzes")
  , ("status", "statuses"), ("bus", "buses"), ("gas", "gases"), ("alias", "aliases")
  , ("analysis", "analyses"), ("crisis", "crises"), ("thesis", "theses") ]

uncountables :: [Text]
uncountables =
  [ "equipment", "information", "rice", "money", "species", "series", "fish", "sheep"
  , "news", "metadata", "feedback", "software", "staff" ]

pluralWord :: Text -> Text
pluralWord w
  | w `elem` uncountables = w
  | Just p <- lookup w irregulars = p
  | w `elem` map snd irregulars = w
  | otherwise = regularPlural (singularWord w)
  where
    regularPlural s
      | any (`T.isSuffixOf` s) ["s", "x", "z", "ch", "sh"] = s <> "es"
      | "y" `T.isSuffixOf` s && not (vowelBeforeLast s) = T.dropEnd 1 s <> "ies"
      | otherwise = s <> "s"
    vowelBeforeLast s = T.length s >= 2 && T.index s (T.length s - 2) `elem` ("aeiou" :: String)

singularWord :: Text -> Text
singularWord w
  | w `elem` uncountables = w
  | Just s <- lookup w [(p, s') | (s', p) <- irregulars] = s
  | w `elem` map fst irregulars = w
  | "ies" `T.isSuffixOf` w && T.length w > 3 = T.dropEnd 3 w <> "y"
  | any (`T.isSuffixOf` w) ["sses", "xes", "zzes", "ches", "shes"] = T.dropEnd 2 w
  | any (`T.isSuffixOf` w) ["ss", "us", "is"] = w
  | "s" `T.isSuffixOf` w = T.dropEnd 1 w
  | otherwise = w
