-- | Internal to Olifant.Language: what a [check] or [test] table means when it
-- names a preset. A preset is TOML defaults Olifant knows (its own cache, the
-- venv, the tool's output format): pack authors write `preset = "pytest"`
-- and never the plumbing.
module Olifant.Language.Presets (applyPreset, presetNames) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Toml (Table, Tree (..))

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

presets :: Map (Text, Text) Table
presets =
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

-- | The presets a section (@"check"@ or @"test"@) knows.
presetNames :: Text -> [Text]
presetNames section = [n | (s, n) <- M.keys presets, s == section]

-- | Fill a [check] or [test] table's missing keys from a named preset; the
-- table's own keys win.
applyPreset :: Text -> Table -> Either Text Table
applyPreset section t = case M.lookup "preset" t of
  Nothing -> Right t
  Just (TText name) -> case M.lookup (section, name) presets of
    Just p -> Right (M.union (M.delete "preset" t) p)
    Nothing -> Left ("`preset`: unknown " <> section <> " preset `" <> name <> "`; expected one of " <> T.intercalate ", " (presetNames section))
  Just _ -> Left "`preset`: expected a string"
