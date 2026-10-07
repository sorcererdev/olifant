-- | Internal to Olifant.Recognize: the inputs hand-written code gives — its
-- path (inverted), its declarations' names aligned with the template's
-- (`class CloseTable` in `class {= Command =}`), and what the project's own
-- instances beside it share (`app`, `service`).
module Olifant.Recognize.Inputs
  ( inputsFor
  , validInputs
  ) where

import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generator (Choices (..), Generator, GeneratorWith (..), InputSpec (..), InputType (..), Scalar (..))
import Olifant.Naming (Case (Snake), parseName, render)
import Olifant.Recognize.Index (Convention (..))
import Olifant.Recognize.Paths (PathPart (..), PathPattern (..), invertPath, pathPattern)
import Olifant.Template.Syntax (Segment (..), Template (..))
import System.FilePath (takeDirectory, (</>))

-- | The inputs a file gives, first found first: the path at the
-- convention's place; else the declarations' names; then what the
-- convention's siblings in or under the file's folder all share.
inputsFor :: Generator -> Convention -> FilePath -> Text -> [(Text, Text)]
inputsFor g c path text = foldl' (\acc (k, v) -> if k `elem` map fst acc then acc else acc <> [(k, v)]) [] (fromPath <> fromNames <> fromSiblings)
  where
    fromPath = concat (invertPath (cvPattern c) path)
    fromNames = if null fromPath then mapMaybe named (declarations g c) else []
    named (keyword, input, suffix) = do
      name <- listToMaybe [n | n <- declared keyword text, suffix `T.isSuffixOf` n, T.length n > T.length suffix]
      either (const Nothing) (\nm -> Just (input, render Snake nm)) (parseName (T.dropEnd (T.length suffix) name))
    fromSiblings =
      let here = takeDirectory path
          beside = [vs | (p, _) <- cvReferences c, p /= "template", let sp = T.unpack p, (here </> "") `isPrefixOf` sp || takeDirectory sp == here, Just vs <- [invertPath (cvPattern c) sp]]
       in case beside of
            [] -> []
            first : rest -> [(k, v) | (k, v) <- first, all (\vs -> lookup k vs == Just v) rest]

-- | The template's declarations of a name: `class {= X =}Suffix` (or
-- struct, trait, enum, def, fn) — the keyword, the input it names, the
-- literal suffix after it.
declarations :: Generator -> Convention -> [(Text, Text, Text)]
declarations g c = go (templateSegments (cvTemplate c))
  where
    go = \case
      Lit l : o@(Out _) : rest
        | Just kw <- keywordBefore l
        , Just pat <- pathPattern (genDerived g) (Template "" [o])
        , [PVar input _] <- ppParts pat ->
            (kw, input, suffixOf rest) : go rest
      _ : rest -> go rest
      [] -> []
    keywordBefore l =
      let lastLine = T.takeWhileEnd (/= '\n') l
          ws = T.words lastLine
       in case reverse ws of
            kw : _ | kw `elem` ["class", "struct", "trait", "enum", "def", "fn", "type"], T.isSuffixOf " " lastLine -> Just kw
            _ -> Nothing
    suffixOf = \case
      Lit l : _ -> T.takeWhile (\x -> isAlphaNum x || x == '_') l
      _ -> ""

-- | The names a text declares with a keyword, in order.
declared :: Text -> Text -> [Text]
declared keyword = mapMaybe decl . T.lines
  where
    decl line = do
      rest <- T.stripPrefix keyword (T.dropWhile isSpace (dropVisibility (T.dropWhile isSpace line)))
      let name = T.takeWhile (\x -> isAlphaNum x || x == '_') (T.dropWhile isSpace rest)
      if T.null name || not (maybe False (isSpace . fst) (T.uncons rest)) then Nothing else Just name
    dropVisibility l = maybe l (T.dropWhile isSpace) (T.stripPrefix "pub" l >>= \r -> if maybe True (isSpace . fst) (T.uncons r) then Just r else Nothing)

-- | Only the inputs the generator declares, with values that fit: a name for
-- an ident, one of the choices for an enum whose choices are known.
validInputs :: Generator -> [(Text, Text)] -> [(Text, Text)]
validInputs g = filter fits
  where
    fits (k, v) = case [i | i <- genInputs g, inputName i == k] of
      i : _ -> case inputType i of
        ScalarInput IdentS -> either (const False) (const True) (parseName v)
        ScalarInput (EnumS choices) -> v `elem` choices
        ScalarInput (EnumOf _ _ (Resolved choices _)) -> v `elem` choices
        ScalarInput BoolS -> v `elem` ["true", "false"]
        _ -> True
      [] -> False
