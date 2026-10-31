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
  , oneContainer
  , linkedGrammars
  , grammarLinked
  , treeSitterAnchors
  , shippedLanguage
  , shippedLanguageNow
  ) where

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
import Olifant.Language.Captures (genericPhrase, genericPhrases, oneContainer)
import Olifant.Language.TreeSitter (linkedGrammars, treeSitterAnchors)

-- | Is the language's grammar linked in, so the generic anchor phrases
-- work on it?
grammarLinked :: Language -> Bool
grammarLinked l = case langGrammar l of
  BuiltInGrammar g -> g `elem` linkedGrammars
  GrammarSource _ -> False

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
