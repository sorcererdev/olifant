-- | What each language can do in Olifant, and which key would enable what it
-- cannot (`olifant doctor`'s capability table, design §milestone 1).
module Olifant.Capabilities
  ( Capability (..)
  , capabilities
  ) where

import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language (ConjureDef (..), CheckDef (..), Language (..), Provider (..), StubDef (..), TestDef (..), TypesDef (..), defaultUnitNames, grammarLabel, grammarLinked, linkedGrammarOf, missingFormats, supportsMissing)

-- | A capability, how it is provided (Right) or the key that would enable
-- it (Left), and the dotted key its presence depends on (whose layer
-- doctor names: 'Olifant.Language.originOf'). A capability no key of the
-- language enables carries its own fix text, which doctor shows in place of
-- "add KEY to …".
data Capability = Capability {capName :: Text, capHow :: Either Text Text, capKey :: Text, capFix :: Maybe Text}
  deriving stock (Eq, Show)

-- | Units, diagnostics, missing cases, tests, test units, anchors, holes, test selection,
-- mutation, stub, conjure shapes, the check's and the runner's messages,
-- stub bodies, signature facts.
capabilities :: Language -> [Capability]
capabilities lang =
  [ Capability "units" units "unit.kinds" Nothing
  , Capability "diagnostics" diagnostics "check.format" Nothing
  , Capability "missing cases" missingCases "check.format" Nothing
  , Capability "tests" tests "test.format" Nothing
  , Capability "test units" testUnits "test.unit" Nothing
  , Capability "anchors" anchors "targets" Nothing
  , Capability "holes" holes "holes" Nothing
  , Capability "test selection" selection "test.select" Nothing
  , Capability "mutation" mutation "mutate" Nothing
  , Capability "stub" stub "test.stub" Nothing
  , Capability "conjure shapes" shapes "conjure" Nothing
  , Capability "messages" messages "check" Nothing
  , Capability "test messages" testMessages "test" Nothing
  , Capability "stub bodies" stubBodies "stub_lines" Nothing
  , Capability "signature facts" signatureFacts "types" Nothing
  ]
  where
    check = langCheck lang
    test = langTest lang
    provided table p = Right (table <> " provider " <> T.unwords (provArgv p))
    units
      | Just p <- langUnitProvider lang = provided "[unit]" p
      | not (null (langUnitKinds lang)) = Right ("[unit] kinds (ast-grep)" <> (if hasNames then ", named by [unit] name" else "") <> (if langUnitJoin lang then ", joined" else ""))
      | Just g <- linkedGrammarOf lang = Right ("queries (" <> fromMaybe g (grammarLabel g) <> "): each function and class definition")
      | otherwise = Left "[unit] kinds"
    hasNames = langUnitNames lang /= defaultUnitNames
    diagnostics = case check of
      Nothing -> Left "[check]"
      Just c -> Right ("[check] format " <> checkFormat c <> (if null (checkConvert c) then "" else " via convert"))
    missingCases = case check of
      Just c | supportsMissing (checkFormat c) -> Right ("[check] format " <> checkFormat c)
      _ -> Left ("a [check] format that reads missing cases (" <> T.intercalate ", " missingFormats <> ")")
    tests = case test of
      Nothing -> Left "[test]"
      Just td -> Right ("[test] format " <> testFormat td <> ", tests_for " <> testsFor td)
    testUnits
      | Just p <- test >>= testUnitProvider = provided "[test] unit" p
      | maybe False (not . null . testUnitRules) test = Right ("[test] unit" <> (if maybe False (isJust . testAbove) test then ", above" else ""))
      | otherwise = Left "[test] unit"
    anchors = case (langTargets lang, langAnchorProvider lang, grammarLinked lang) of
      ([], Nothing, False) -> Left "[targets.*]"
      (ts, p, linked) -> Right (T.intercalate ", " (["[targets] " <> T.pack (show (length ts)) <> " targets" | not (null ts)] ++ ["generic phrases (queries)" | linked] ++ ["anchors provider " <> T.unwords (provArgv q) | Just q <- [p]]))
    holes = case langHoles lang of
      [] -> Left "[holes.*]"
      ks -> Right ("[holes] " <> T.intercalate ", " (map fst ks))
    stub = case test >>= testStub of
      Just sd -> Right ("[test] stub" <> maybe "" (" at " <>) (sdAt sd))
      Nothing -> Left "[test] stub"
    shapes = case langConjure lang of
      Just cj | parts@(_ : _) <- [n | (n, True) <- [("port", isJust (cjPort cj)), ("port.add", isJust (cjAddMethod cj)), ("fake", isJust (cjFake cj)), ("constructor", isJust (cjConstructor cj)), ("advice", not (null (cjAdvice cj)))]] ->
        Right ("[conjure] " <> T.intercalate ", " parts <> (if isJust (cjPort cj) && isNothing (cjAddMethod cj) then " (no [conjure.port] add: a method a port lacks is yours to add)" else ""))
      _ -> Left "[conjure] (conjure works at the floor: the test and its fill, no ports)"
    messages = case keysOf "[check]" [("undefined", maybe False (not . null . checkUndefined) check), ("missing_member", maybe False (isJust . checkMissingMember) check)] of
      "" -> Left "[check] undefined, missing_member (no undefined names read from the check)"
      present -> Right present
    testMessages = case keysOf "[test]" [("undefined", maybe False (not . null . testUndefined) test), ("failure", maybe False (isJust . testFailure) test)] of
      "" -> Left "[test] undefined, [test.failure] (the panel shows the test's last assertion)"
      present -> Right present
    stubBodies = case langStubLines lang of
      [] -> Left "stub_lines (a body is a stub only when it is empty, a comment or a hole)"
      ls -> Right ("stub_lines (" <> T.pack (show (length ls)) <> ")")
    signatureFacts = case langTypes lang of
      Nothing -> Left "[types] (no new port method's signature is derived)"
      Just ty -> Right ("[types] " <> T.intercalate ", " ([k | (k, Just _) <- [("string", tyString ty), ("integer", tyInteger ty), ("nothing", tyNothing ty), ("optional", tyOptional ty), ("param", tyParam ty), ("annotation", tyAnnotation ty), ("returns", tyReturns ty), ("assigned", tyAssigned ty), ("record", tyRecord ty), ("method", tyMethod ty)]] ++ ["checks_none" | not (null (tyChecksNone ty))]))
    keysOf table ks = case [k | (k, True) <- ks] of
      [] -> ""
      present -> table <> " " <> T.intercalate ", " present
    mutation = case langMutate lang of
      [] -> Left "[[mutate]]"
      ops -> Right ("[[mutate]] (" <> T.pack (show (length ops)) <> " operators)")
    selection
      | Just p <- test >>= testSelectProvider = provided "[test] select" p
      | maybe False (isJust . testSelect) test = Right "[test] select"
      | otherwise = Left "[test] select (whole files run)"
