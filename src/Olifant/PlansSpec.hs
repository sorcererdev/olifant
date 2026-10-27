{-# LANGUAGE OverloadedStrings #-}
module Olifant.PlansSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Olifant.Plans
import Test.Hspec

sample :: Text
sample = T.unlines
  [ "# Ticket: slug the titles"
  , ""
  , "Make titles safe for URLs."
  , "## Behaviour"
  , "What a slug is:"
  , "- lowercases every letter"
  , "- replaces spaces with dashes @src/slug.py:slugify"
  , "- ? what about unicode"
  , "- empty title gives an empty slug"
  , "## Steps"
  , "Work through these in order."
  , "- [x] write slugify @src/slug.py:slugify ✓ abc1234"
  , "- [ ] handle dashes @src/slug.py:collapse"
  , "  - test: dashes collapse @tests/test_slug.py:test_dashes ○"
  , "  - also @src/slug.py:strip"
  , "  - ✓ ran green"
  ]

spec :: Spec
spec = describe "Olifant.Plans" $ do
  it "classifies every line kind" $
    map lnKind (parsePlan sample) `shouldBe`
      [Heading 1, Prose, Prose, Heading 2, Prose, Claim, Claim, Question, Claim, Heading 2, Prose, Step True, Step False, TestChild, AlsoChild, Evidence]
  it "takes the last anchor on a line and its columns" $ do
    let [l] = parsePlan "- see @docs/x.md then @src/a.py:Foo.bar ✓ abc1234"
    lnAnchor l `shouldBe` Just (Anchor "src/a.py" "Foo.bar" 22 39)
    lnVerdict l `shouldBe` Verified "abc1234"
  it "reads ? not found" $
    lnVerdict (head (parsePlan "- x @src/a.py:Gone ? not found")) `shouldBe` NotFound
  it "sections cover rows and the preamble is a section" $
    sections (parsePlan "intro\n## A\n- a\n## B\n") `shouldBe` [Section "" 0 1 Nothing, Section "A" 1 3 Nothing, Section "B" 3 4 Nothing]
  it "children are the deeper lines right after" $
    map lnRow (childrenOf (parsePlan "- [ ] s\n  - test: t @f:x ○\n  - [ ] child\n- [ ] next") 0) `shouldBe` [1, 2]
  it "withVerdict replaces, appends and strips" $ do
    withVerdict "- x @a.py:F ✓ 1111111" (Verified "2222222") `shouldBe` "- x @a.py:F ✓ 2222222"
    withVerdict "- x @a.py:F" NotFound `shouldBe` "- x @a.py:F ? not found"
    withVerdict "- x @a.py:F ? not found" NoVerdict `shouldBe` "- x @a.py:F"
  it "a hash without a space is prose, and a heading has no anchor" $ do
    map lnKind (parsePlan "#tag\n####### seven\n###") `shouldBe` [Prose, Prose, Heading 3]
  it "sectionOf finds the enclosing section; anchorText renders" $ do
    let ls = parsePlan "intro\n## A\n- a\n## B\n"
    secTitle (sectionOf ls 2) `shouldBe` "A"
    secTitle (sectionOf ls 0) `shouldBe` ""
    anchorText (Anchor "a.py" "F" 0 7) `shouldBe` "@a.py:F"
  it "ignores lines inside a fence" $ map lnKind (parsePlan "## A\n```\n# not a heading\n- [ ] not a step\n```\n- [ ] a step") `shouldBe` [Heading 2, Prose, Prose, Prose, Prose, Step False]
  it "fenced lines carry no anchor or verdict" $ do
    let ls = parsePlan "```\n- x @a.py:F ✓ abc1234\n```"
    map lnAnchor ls `shouldBe` [Nothing, Nothing, Nothing]
    map lnVerdict ls `shouldBe` [NoVerdict, NoVerdict, NoVerdict]
  it "a longer fence holding a shorter one stays open until its own length" $
    map lnKind (parsePlan "````\n```\n- [ ] inside\n````\n- [ ] out") `shouldBe` [Prose, Prose, Prose, Prose, Step False]
  it "a closing line with text after the backticks does not close" $
    map lnKind (parsePlan "```\n``` python\n- [ ] inside\n```\n- [ ] out") `shouldBe` [Prose, Prose, Prose, Prose, Step False]
  it "blank lines inside a fence under a step keep the fence among the step's children" $ do
    let ls = parsePlan "- [ ] s @a.py:F\n  ```python @a.py:F\n  def f():\n  \n\n      pass\n  ```\n- [ ] next"
    map lnRow (childrenOf ls 0) `shouldBe` [1, 2, 3, 4, 5, 6]
    map lnIndent ls `shouldBe` [0, 2, 2, 2, 2, 6, 2, 0]
  it "an info-string anchor is not an anchor" $
    map lnAnchor (parsePlan "  ```python @a.py:F\n  x\n  ```") `shouldBe` [Nothing, Nothing, Nothing]
  it "a trailing dot is not part of the unit" $ do
    let [l] = parsePlan "- the table @src/a.py:Table."
    anUnit <$> lnAnchor l `shouldBe` Just "Table"
    anEnd <$> lnAnchor l `shouldBe` Just 27
  it "a done step carries its fingerprint" $ lnVerdict (head (parsePlan "- [x] done @src/a.py:F ✓ abc1234")) `shouldBe` Verified "abc1234"
  it "a section may name its branch" $ sections (parsePlan "## Close table · branch close-table\n- x\n## B") `shouldBe` [Section "Close table" 0 2 (Just "close-table"), Section "B" 2 3 Nothing]
  it "test children carry a state" $ do
    map testState (parsePlan "  - test: it works @tests/t.py:test_it ○\n  - test: red @tests/t.py:test_red ✗\n  - test: none @tests/t.py:test_n") `shouldBe` [Just TestPending, Just TestRed, Nothing]
    withTestState "  - test: it works @tests/t.py:test_it ○" TestGreen `shouldBe` "  - test: it works @tests/t.py:test_it ●"
    withTestState "  - test: none @tests/t.py:test_n" TestPending `shouldBe` "  - test: none @tests/t.py:test_n ○"
  it "evidence children have a tag" $ map evidenceTag (parsePlan "  - ✗ changed: the port moved\n  - ✗ gone: @a.py:F no longer exists\n  - ✗ test_x red again\n  - ✗ @a.py:F red again\n  - ✓ fine @a.py:F") `shouldBe` [Just "changed", Just "gone", Just "red again", Just "red again", Nothing]
  it "stepDone is true only for a done step" $ map stepDone (parsePlan "- [x] a\n- [ ] b\n- c") `shouldBe` [True, False, False]
