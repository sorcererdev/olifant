-- | Language knowledge lives in packs as data (spec §12): adapters, the
-- ast-grep rules behind `at` targets, and the ports that run tools.
module Olifant.Language
  ( module Olifant.Language.Adapter
  , module Olifant.Language.Rule
  , module Olifant.Language.Syntax
  , module Olifant.Language.Check
  , module Olifant.Language.Diagnostics
  , module Olifant.Language.Support
  , module Olifant.Language.Test
  , genericPhrase
  , genericPhrases
  , renamedPhrases
  , oneContainer
  , linkedGrammars
  , capturesOf
  , grammarLabel
  , queriesFor
  , queriesOf
  , definedName
  , definedNamesIn
  , signatureHead
  , functionMatches
  , hasNoFunctionQueries
  , querySet
  , grammarLinked
  , hasUnits
  , genericNames
  , genericFor
  , linkedGrammarOf
  , treeSitterAnchors
  , shippedLanguage
  , shippedLanguageNow
  ) where

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Language.Presets (shippedPresets)
import Olifant.Toml (parseToml)
import System.FilePath (takeFileName, (</>))

import Olifant.Language.Adapter
import Olifant.Language.Check
import Olifant.Language.Diagnostics
import Olifant.Language.Support
import Olifant.Language.Rule
import Olifant.Language.Syntax
import Olifant.Language.Test
import Olifant.Language.Captures (definedNames, genericPhrase, genericPhrases, oneContainer, phraseKind, renamedPhrases)
import Olifant.Language.TreeSitter (capturesOf, definedName, definedNamesIn, functionMatches, hasNoFunctionQueries, grammarLabel, grammarNodeTypes, linkedGrammars, queriesFor, queriesOf, querySet, signatureHead, treeSitterAnchors)

-- | Is the language's grammar linked in, so the generic anchor phrases
-- work on it?
grammarLinked :: Language -> Bool
grammarLinked = isJust . linkedGrammarOf

-- | Does the language have units: `[unit] kinds`, a units provider, or a
-- linked grammar (whose queries' definitions are its units)?
hasUnits :: Language -> Bool
hasUnits l = hasUnitRule l || grammarLinked l

-- | The names a generic phrase's word (`function`, `class`, a kind word)
-- can take in a text: its definitions of that kind, in order. None when the
-- grammar is not linked.
genericNames :: Language -> Text -> Text -> [Text]
genericNames l kind txt = case linkedGrammarOf l of
  Just g -> either (const []) (definedNames kind . snd) (capturesOf g (querySet l) txt)
  Nothing -> []

-- | Does a language answer a phrase by the generic rule: its grammar is
-- linked, the phrase has a generic shape, and its kind word (`struct`) names
-- a node type of the grammar?
genericFor :: Language -> [Text] -> Bool
genericFor l ws = case linkedGrammarOf l of
  Just g -> genericPhrase ws && maybe True (\k -> any (\t -> t == k || (k <> "_") `T.isPrefixOf` t) (grammarNodeTypes g)) (phraseKind ws)
  Nothing -> False

-- | The language's grammar, when it is linked in.
linkedGrammarOf :: Language -> Maybe Text
linkedGrammarOf l = case langGrammar l of
  Grammar g _ | g `elem` linkedGrammars -> Just g
  _ -> Nothing

-- | The shipped language of that name alone, for specs (fails on a file
-- that does not parse).
shippedLanguageNow :: Text -> Language
shippedLanguageNow name = either (error . T.unpack) id (languageFromLayers shippedPresets name (maybe [] pure (shippedLayer name)))

-- | The shipped language of that name under these packs' files (origin,
-- pack root: @root/languages/<name>.toml@), as a project would have it; for
-- specs. Fails on a file that does not parse.
shippedLanguage :: Text -> [(Text, FilePath)] -> IO Language
shippedLanguage name packs = do
  layers <- traverse (\(origin, r) -> either (error . T.unpack) (Layer origin (Just r)) . parseToml <$> TIO.readFile (r </> "languages" </> T.unpack name <> ".toml")) packs
  either (error . T.unpack) pure (languageFromLayers shippedPresets name (maybe [] pure (shippedLayer name) ++ layers))
