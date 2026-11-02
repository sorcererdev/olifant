{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.Language.PresetsSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Olifant.Language.Presets
import Olifant.Library (shippedToolchains)
import Olifant.Toml (Tree (..))
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Language.Presets" $ do
  it "every shipped toolchain parses" $ do
    fmap (const ()) (presetsFrom "shipped" shippedToolchains) `shouldBe` Right ()
    presetNames shippedPresets "check" `shouldBe` ["cabal-repl", "cargo", "pyright"]
    presetNames shippedPresets "test" `shouldBe` ["cargo-nextest", "cargo-test", "hspec", "pytest"]
  it "the message keys ship with their toolchains" $ do
    let Presets ps = shippedPresets
        has s n k = maybe False (M.member k) (M.lookup (s, n) ps)
    [has s n k | (s, n, k) <- [("check", "pyright", "missing_member"), ("check", "pyright", "undefined"), ("check", "cabal-repl", "undefined"), ("check", "cargo", "undefined"), ("test", "pytest", "undefined"), ("test", "pytest", "failure")]] `shouldBe` replicate 6 True
  it "a toolchain file has one table" $ do
    let Left why = presetsFrom "pack p" [("x", T.unlines ["[check]", "format = 'sarif'", "[test]", "format = 'tap'"])]
    why `shouldSatisfy` T.isInfixOf "toolchains/x.toml"
    why `shouldSatisfy` T.isInfixOf "exactly one of [check] or [test]"
  it "a later preset wins" $ do
    let Right over = presetsFrom "pack p" [("hspec", T.unlines ["[test]", "command = ['x']"])]
        Presets merged = overPresets shippedPresets over
        Just t = M.lookup ("test", "hspec") merged
    M.lookup "command" t `shouldBe` Just (TList [TText "x"])
    M.member "lines" t `shouldBe` True
  it "applyPreset fills a table's missing keys, its own keys winning" $ do
    let Right t = applyPreset shippedPresets "test" (M.fromList [("preset", TText "pytest"), ("tests_for", TText "X")])
    M.lookup "tests_for" t `shouldBe` Just (TText "X")
    M.lookup "format" t `shouldBe` Just (TText "junit-xml")
