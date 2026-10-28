-- | Internal to Olifant.Language: what a [check] or [test] table means when it
-- names a preset. A preset is TOML defaults Olifant knows (its own cache, the
-- venv, the tool's output format): pack authors write `preset = "pytest"`
-- and never the plumbing. The presets ship as `library/toolchains/<name>.toml`
-- (one `[check]` or `[test]` table each); a pack's and the project's
-- `toolchains/` override them.
module Olifant.Language.Presets (Presets (..), presetsFrom, overPresets, shippedPresets, applyPreset, presetNames) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Library (shippedToolchains)
import Olifant.Toml (Table, Tree (..), mergeTables, parseToml)

-- | The presets known, by (section, name).
newtype Presets = Presets (Map (Text, Text) Table)
  deriving stock (Eq, Show)

-- | Toolchain files (name, TOML source) as presets; @origin@ names where they
-- come from in errors. Each file holds exactly one table, `[check]` or `[test]`.
presetsFrom :: Text -> [(Text, Text)] -> Either Text Presets
presetsFrom origin files = Presets . M.fromList <$> traverse one files
  where
    one (name, src) = do
      let path = origin <> ": toolchains/" <> name <> ".toml"
      top <- either (\e -> Left (path <> ": " <> e)) Right (parseToml src)
      case M.toList top of
        [(section, TTable t)] | section `elem` ["check", "test"] -> Right ((section, name), t)
        _ -> Left (path <> ": expected exactly one of [check] or [test]")

-- | Later presets win, merged key by key within a preset ('mergeTables').
overPresets :: Presets -> Presets -> Presets
overPresets (Presets a) (Presets b) = Presets (M.unionWith mergeTables a b)

-- | The presets shipped in the binary.
shippedPresets :: Presets
shippedPresets = either (error . T.unpack) id (presetsFrom "shipped" shippedToolchains)
{-# NOINLINE shippedPresets #-}

-- | The presets a section (@"check"@ or @"test"@) knows.
presetNames :: Presets -> Text -> [Text]
presetNames (Presets ps) section = [n | (s, n) <- M.keys ps, s == section]

-- | Fill a [check] or [test] table's missing keys from a named preset; the
-- table's own keys win.
applyPreset :: Presets -> Text -> Table -> Either Text Table
applyPreset presets@(Presets ps) section t = case M.lookup "preset" t of
  Nothing -> Right t
  Just (TText name) -> case M.lookup (section, name) ps of
    Just p -> Right (M.union (M.delete "preset" t) p)
    Nothing -> Left ("`preset`: unknown " <> section <> " preset `" <> name <> "`; expected one of " <> T.intercalate ", " (presetNames presets section))
  Just _ -> Left "`preset`: expected a string"
