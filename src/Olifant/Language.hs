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
  , shippedLanguage
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

-- | The shipped language of that name under these packs' files (origin,
-- pack root: @root/languages/<name>.toml@), as a project would have it; for
-- specs. Fails on a file that does not parse.
shippedLanguage :: Text -> [(Text, FilePath)] -> IO Language
shippedLanguage name packs = do
  layers <- traverse (\(origin, r) -> either (error . T.unpack) (Layer origin (Just r)) . parseToml <$> TIO.readFile (r </> "languages" </> T.unpack name <> ".toml")) packs
  either (error . T.unpack) pure (languageFromLayers shippedPresets name (maybe [] pure (shippedLayer name) ++ layers))
