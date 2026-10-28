{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.Language.PresetsSpec (spec) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language.Presets
import Olifant.Library (shippedToolchains)
import Olifant.Toml (Table, Tree (..))
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Language.Presets" $ do
  it "every shipped toolchain parses" $ do
    fmap (const ()) (presetsFrom "shipped" shippedToolchains) `shouldBe` Right ()
    presetNames shippedPresets "check" `shouldBe` ["cabal-repl", "cargo", "pyright"]
    presetNames shippedPresets "test" `shouldBe` ["hspec", "pytest"]
  -- temporary golden (milestone 2, Task 9 deletes it): the TOML presets are
  -- the Haskell ones they replace, but for the stated differences
  it "the TOML presets equal the Haskell ones" $ do
    let Presets shipped = shippedPresets
        expected = M.mapWithKey adjust legacy
        adjust (_, name) t =
          let t' = M.delete "install" t
           in case name of
                "cargo" -> M.adjust (\(TList ws) -> TList (filter (`notElem` [TText "--exclude", TText "diesel-*"]) ws)) "command" t'
                "pytest" -> M.insert "tests_for" (TText "{= dir =}/test_{= stem =}.py") t'
                _ -> t'
    M.keys shipped `shouldBe` M.keys legacy
    M.map (M.delete "install") shipped `shouldBe` M.map (M.delete "install") expected
    [k | (k, t) <- M.toList shipped, M.notMember "install" t] `shouldBe` []
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
    M.lookup "format" t `shouldBe` Just (TText "pytest")

uv :: [Text] -> [Tree]
uv rest = map TText (["env", "UV_PROJECT_ENVIRONMENT={= cache =}/venv-{= context =}", "uv", "run", "--quiet", "--frozen"] ++ rest)

pyProject :: [(Text, Tree)]
pyProject = [("context", TList [TText "pyproject.toml", TText "uv.lock"]), ("root", TList [TText "pyproject.toml"])]

-- | A cabal package's context: its own file and the project's.
cabalContext :: (Text, Tree)
cabalContext = ("context", TList (map TText ["*.cabal", "cabal.project", "cabal.project.freeze"]))

texts :: [Text] -> Tree
texts = TList . map TText

-- | hspec's `--format=checks` tree, as `lines` reads it.
hspecLines :: Table
hspecLines =
  M.fromList
    [ ("pass", TText "^[[:space:]]*(.*[^[:space:]]) \\[✔\\]$")
    , ("fail", TText "^[[:space:]]*(.*[^[:space:]]) \\[✘\\]$")
    , ("nest", TBool True)
    , ("failure", TText "^  ([^[:space:]].*):([0-9]+):[0-9]+: *$")
    , ("end", TText "^  To rerun use|^Randomized with seed|^Finished in")
    , ("drop", TText "^  [0-9]+\\) ")
    ]

legacy :: Map (Text, Text) Table
legacy =
  M.fromList
    [ (("check", "pyright"), M.fromList (("command", TList (uv ["pyright", "--outputjson"])) : ("format", TText "pyright-json") : pyProject))
    , ( ("check", "cargo")
      , M.fromList
          [ ("command", TList (map TText ["cargo", "check", "--workspace", "--exclude", "diesel-*", "--message-format=json", "--target-dir", "{= cache =}/target-{= context =}"]))
          , ("context", TList [TText "**/Cargo.toml", TText "Cargo.lock"])
          , ("format", TText "rustc-json")
          , ("exclusive", TBool True)
          , ("root", TList [TText "Cargo.toml"])
          , ("root_pick", TText "outermost")
          ]
      )
    , ( ("check", "cabal-repl")
      , M.fromList
          [ ("command", texts ["cabal", "repl", "-v0", "--builddir={= cache =}/dist-repl-{= context =}", "--repl-options=-fno-code", "--repl-options=-fdiagnostics-as-json", "--repl-options=-Wincomplete-patterns", "--repl-options=-fmax-uncovered-patterns=1000"])
          , cabalContext
          , ("format", TText "ghc-json")
          , ("exclusive", TBool True)
          ]
      )
    , ( ("test", "hspec")
      , M.fromList
          [ ("command", texts ["cabal", "test", "-v0", "--builddir={= cache =}/dist-test-{= context =}", "--test-show-details=direct", "--test-option=--format=checks", "--test-option=--no-color", "{= tests =}"])
          , cabalContext
          , ("format", TText "lines")
          , ("lines", TTable hspecLines)
          , ("unit", texts ["(^|[[:space:]$])(describe|context|it|specify|prop)[[:space:]]+\"([^\"]*)\""])
          , ("file_group", TText "^module[[:space:]]+([[:alnum:]_.]+)Spec")
          , ("select", TTable (M.fromList [("test", TText "--test-option=--match=/{= group =}/{= name =}/"), ("file", TText "--test-option=--match=/{= group =}/")]))
          , ("exclusive", TBool True)
          , ("timeout", TInt 900)
          , ("tests_for", TText "{= dir =}/{= stem =}Spec.hs")
          ]
      )
    , (("test", "pytest"), M.fromList (("command", TList (uv ["pytest", "-q", "-rA", "--tb=short", "-p", "no:cacheprovider", "{= tests =}"])) : ("format", TText "pytest") : pyProject))
    ]

