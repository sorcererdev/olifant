{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Temporary (language-neutral milestone 2; deleted when the milestone
-- closes): the shipped languages and the thinned pack files are what the
-- pack files were at 4c1fbc1 (copies in testdata/split).
module Olifant.SplitSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Language
import Olifant.Language.Presets (shippedPresets)
import Olifant.Toml (parseToml)
import Test.Hspec

layerOf :: Text -> FilePath -> FilePath -> IO Layer
layerOf origin dir file = either (error . T.unpack) (Layer origin (Just dir)) . parseToml <$> TIO.readFile file

clear :: Language -> String
clear l = show l {langLayers = [], langOrigins = M.empty}

built :: Text -> [Layer] -> String
built name layers = either (error . T.unpack) clear (languageFromLayers shippedPresets name (maybe [] pure (shippedLayer name) ++ layers))

-- (language, pack, the pack's file, whether olifant's own project layer applies)
cases :: [(Text, FilePath, Bool)]
cases = [("python", "packs/python-htmx", False), ("rust", "packs/rust-htmx", False), ("elm", "packs/elm", False), ("haskell", "packs/olifant-dev", True)]

spec :: Spec
spec = describe "the split (temporary golden)" $ do
  let copy name pack = "src/Olifant/testdata/split/" <> drop 6 pack <> "-" <> T.unpack name <> ".toml"
      project = layerOf "project" ".olifant" ".olifant/languages/haskell.toml"
  mapM_
    ( \(name, pack, own) -> it ("thinning " <> T.unpack name <> " removed only what the shipped file says the same") $ do
        own' <- if own then pure <$> project else pure []
        ref <- layerOf ("pack " <> T.pack (drop 6 pack)) pack (copy name pack)
        new <- if own then pure [] else pure <$> layerOf ("pack " <> T.pack (drop 6 pack)) pack (pack <> "/languages/" <> T.unpack name <> ".toml")
        built name (new ++ own') `shouldBe` built name (ref : own')
    )
    cases
