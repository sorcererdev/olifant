{-# LANGUAGE TemplateHaskell #-}

-- | The libraries shipped inside the binary (design §three layers): the
-- languages under @library/languages@ and the toolchain presets under
-- @library/toolchains@, one TOML file each.
-- `embedDir` sees a newly added file only once this module recompiles:
-- touch it after adding one.
module Olifant.Library (shippedLanguages, shippedToolchains, providersSection, queryExemplar, textobjectsSource) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir, embedFile)
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

-- | User chapter 9's @### Providers…@ section, up to the next heading of its
-- level or above: the protocol @init --language@ shows the model. Renaming
-- that heading breaks 'Olifant.LanguageInitSpec'.
providersSection :: Text
providersSection =
  T.unlines (takeWhile (not . heading) (drop 1 from) `withFirst` from)
  where
    chapter = T.lines (TE.decodeUtf8 $(embedFile "book/user/09-packs-and-language-adapters.md"))
    from = dropWhile (not . ("### Providers" `T.isPrefixOf`)) chapter
    heading l = "### " `T.isPrefixOf` l || "## " `T.isPrefixOf` l
    withFirst rest = \case
      first : _ -> first : rest
      [] -> []

-- | The one query file @init --language@ shows a model that writes a
-- grammar's textobjects (nvim has none for it): a shipped file whose shape
-- C-like grammars share, its node kinds what the model changes.
queryExemplar :: (FilePath, Text)
queryExemplar = ("textobjects.scm", TE.decodeUtf8 $(embedFile "library/queries/rust/textobjects.scm"))

-- | Where the shipped textobjects files were taken from
-- (@library/queries/*/SOURCES@): nvim-treesitter-textobjects' repository
-- and commit. @init --language@ copies a new grammar's file from it.
textobjectsSource :: (Text, Text)
textobjectsSource = ("https://github.com/nvim-treesitter/nvim-treesitter-textobjects", "5c7b0263797dfd1bd6202f2b219f3b53a80b2187")
