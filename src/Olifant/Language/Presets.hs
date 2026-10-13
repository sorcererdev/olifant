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

presets :: Map (Text, Text) Table
presets =
  M.fromList
    [ (("check", "pyright"), M.fromList (("command", TList (uv ["pyright", "--outputjson"])) : ("format", TText "pyright-json") : pyProject))
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
