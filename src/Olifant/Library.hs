{-# LANGUAGE TemplateHaskell #-}

-- | The libraries shipped inside the binary (design §three layers): the
-- languages under @library/languages@ and the toolchain presets under
-- @library/toolchains@, one TOML file each.
-- `embedDir` sees a newly added file only once this module recompiles:
-- touch it after adding one.
module Olifant.Library (shippedLanguages, shippedToolchains) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.FilePath (takeBaseName, takeExtension)

-- | (name, TOML source) of each embedded `.toml` file, sorted by name.
tomlFiles :: [(FilePath, ByteString)] -> [(Text, Text)]
tomlFiles fs = sortOn fst [(T.pack (takeBaseName f), TE.decodeUtf8 b) | (f, b) <- fs, takeExtension f == ".toml"]

-- | The shipped toolchain presets: (preset name, TOML source).
shippedToolchains :: [(Text, Text)]
shippedToolchains = tomlFiles $(embedDir "library/toolchains")

-- | The shipped languages: (language name, TOML source).
shippedLanguages :: [(Text, Text)]
shippedLanguages = tomlFiles $(embedDir "library/languages")
