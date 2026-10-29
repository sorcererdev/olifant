-- | What each language can do in Olifant, and which key would enable what it
-- cannot (`olifant doctor`'s capability table, design §milestone 1).
module Olifant.Capabilities
  ( Capability (..)
  , capabilities
  ) where

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language (CheckDef (..), Language (..), TestDef (..), defaultUnitNames, supportsMissing)

-- | A capability, how it is provided (Right) or the key that would enable
-- it (Left), and the dotted key its presence depends on (whose layer
-- doctor names: 'Olifant.Language.originOf'). A capability no key of the
-- language enables carries its own fix text, which doctor shows in place of
-- "add KEY to …".
data Capability = Capability {capName :: Text, capHow :: Either Text Text, capKey :: Text, capFix :: Maybe Text}
  deriving stock (Eq, Show)

-- | Units, diagnostics, missing cases, tests, test units, test selection,
-- mutation, stub.
capabilities :: Language -> [Capability]
capabilities lang =
  [ Capability "units" units "unit.kinds" Nothing
  , Capability "diagnostics" diagnostics "check.format" Nothing
  , Capability "missing cases" missingCases "check.format" Nothing
  , Capability "tests" tests "test.format" Nothing
  , Capability "test units" testUnits "test.unit" Nothing
  , Capability "test selection" selection "test.select" Nothing
  , Capability "mutation" mutation "mutate" Nothing
  , Capability "stub" stub "test.format" (either Just (const Nothing) stub)
  ]
  where
    check = langCheck lang
    test = langTest lang
    pytest = fmap testFormat test == Just "pytest"
    units
      | null (langUnitKinds lang) = Left "[unit] kinds"
      | otherwise = Right ("[unit] kinds (ast-grep)" <> (if hasNames then ", named by [unit] name" else "") <> (if langUnitJoin lang then ", joined" else ""))
    hasNames = langUnitNames lang /= defaultUnitNames
    diagnostics = case check of
      Nothing -> Left "[check]"
      Just c -> Right ("[check] format " <> checkFormat c <> (if null (checkConvert c) then "" else " via convert"))
    missingCases = case check of
      Just c | supportsMissing (checkFormat c) -> Right ("[check] format " <> checkFormat c)
      _ -> Left "a [check] format that reads missing cases (ghc-json, pyright-json, rustc-json, elm-json, olifant-json)"
    tests = case test of
      Nothing -> Left "[test]"
      Just td -> Right ("[test] format " <> testFormat td <> ", tests_for " <> testsFor td)
    testUnits
      | maybe False (not . null . testUnitRules) test = Right ("[test] unit" <> (if maybe False (isJust . testAbove) test then ", above" else ""))
      | otherwise = Left "[test] unit"
    stub
      | pytest = Right "built in (pytest shape)"
      | otherwise = Left "stubs for other test formats come with [test] stub"
    mutation = case langMutate lang of
      [] -> Left "[[mutate]]"
      ops -> Right ("[[mutate]] (" <> T.pack (show (length ops)) <> " operators)")
    selection
      | maybe False (isJust . testSelect) test = Right "[test] select"
      | pytest = Right "pytest ids"
      | otherwise = Left "[test] select (whole files run)"
