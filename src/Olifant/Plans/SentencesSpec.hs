{-# LANGUAGE OverloadedStrings #-}
module Olifant.Plans.SentencesSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Olifant.Plans
import Olifant.Plans.Sentences
import Test.Hspec

of_ :: [Text] -> [Sentence]
of_ = sentences . parsePlan . T.unlines

texts :: [Text] -> [Text]
texts = map snText . of_

spec :: Spec
spec = describe "Olifant.Plans.Sentences" $ do
  it "keeps a step whole" $ do
    let [s] = of_ ["- [ ] Fix it. It should not shift."]
    snKind s `shouldBe` Step False
    snText s `shouldBe` "Fix it. It should not shift."
    (snRow s, snCol s, snEndRow s, snEndCol s) `shouldBe` (0, 6, 0, 34)

  it "keeps a question whole" $ do
    let [s] = of_ ["- ? Why is it slow? And when?"]
    snKind s `shouldBe` Question
    snText s `shouldBe` "Why is it slow? And when?"

  it "gives no sentence for tests, also lines, evidence, journal, fences, front matter and bare links" $
    texts
      [ "---", "title: A thing. Yes.", "---"
      , "- test: dashes collapse"
      , "- also @src/x.py:F"
      , "- ✓ ran green"
      , "- 25 Oct 21:31 · did a thing. It worked."
      , "```", "The code. Is here.", "```"
      , "<https://x.org/a>"
      , "[x](https://x.org/a)"
      , "https://x.org/a"
      ] `shouldBe` []

  it "makes one sentence of a heading, its branch suffix dropped" $ do
    let [s] = of_ ["## Raw votes · raw-votes"]
    snText s `shouldBe` "Raw votes"
    snKind s `shouldBe` Heading 2
    snHeading s `shouldBe` "Raw votes"

  it "splits a paragraph across rows when the rows join" $ do
    let ss = sentencesWith (== 0) (parsePlan (T.unlines ["There is no AI in the codebase yet. We would have to", "redeploy everything."]))
    map snText ss `shouldBe` ["There is no AI in the codebase yet.", "We would have to redeploy everything."]
    let [a, b] = ss
    (snRow a, snCol a, snEndRow a, snEndCol a) `shouldBe` (0, 0, 0, 35)
    (snRow b, snCol b, snEndRow b, snEndCol b) `shouldBe` (0, 36, 1, 20)
    snKind b `shouldBe` Prose

  it "strips numbered markers" $ do
    let [a] = of_ ["1 - Check that collectors parse input"]
        [b] = of_ ["1. Check that collectors parse input"]
    snText a `shouldBe` "Check that collectors parse input"
    snText b `shouldBe` "Check that collectors parse input"
    snCol a `shouldBe` 4
    snCol b `shouldBe` 3

  it "does not join a numbered line to the line above" $
    texts ["Idea: We can use ai to", "1 - Check that collectors parse", "2 - Diagnose (e.g. The format changed)"]
      `shouldBe` ["Idea: We can use ai to", "Check that collectors parse", "Diagnose (e.g. The format changed)"]

  it "splits a bullet into sentences, the dash stripped" $
    texts ["- The collector retries. It gives up after one."]
      `shouldBe` ["The collector retries.", "It gives up after one."]

  it "does not split on abbreviations, versions, code spans or URLs" $ do
    texts ["Use a helper, e.g. The loader. Then stop."] `shouldBe` ["Use a helper, e.g. The loader.", "Then stop."]
    texts ["That is, i.e. Mostly. Fine."] `shouldBe` ["That is, i.e. Mostly.", "Fine."]
    texts ["Cats vs. Dogs, etc. And more."] `shouldBe` ["Cats vs. Dogs, etc. And more."]
    texts ["Ask Dr. Who first."] `shouldBe` ["Ask Dr. Who first."]
    texts ["htmx 4.0.0 is vendored. It works."] `shouldBe` ["htmx 4.0.0 is vendored.", "It works."]
    texts ["`a. B()` returns. Good."] `shouldBe` ["`a. B()` returns.", "Good."]
    texts ["see https://x.org/a.b. Then go."] `shouldBe` ["see https://x.org/a.b.", "Then go."]
    texts ["Is it slow? Yes! \"Quoted\" too."] `shouldBe` ["Is it slow?", "Yes!", "\"Quoted\" too."]

  it "keeps the anchor and drops it from the text" $ do
    let [s] = of_ ["- slugify lowercases @src/slug.py:slugify ✓ abc1234"]
    snText s `shouldBe` "slugify lowercases"
    fmap anUnit (snAnchor s) `shouldBe` Just "slugify"

  it "keys a sentence by its words, anchor and verdict dropped" $
    sentenceKey "  The  collector   retries @nightvote/x.py:F ✓ ab12" `shouldBe` "The collector retries"

  it "gives the section title, and nothing before any heading" $ do
    let ss = of_ ["Before.", "## Plan · branch p1", "After."]
    map snHeading ss `shouldBe` ["", "Plan", "Plan"]

  it "leads with the enclosing question, the previous sentence, or nothing" $ do
    let ss = of_
          [ "- ? Do we know X?"
          , "  - We store it in the loader."
          , ""
          , "First one. Second one."
          , ""
          , "After a blank."
          ]
    map snLead ss `shouldBe` [Nothing, Just "Do we know X?", Nothing, Just "First one.", Nothing]

  it "leads a bullet with the bullet above it in the same run" $
    map snLead (of_ ["- WebFetcher retries.", "- The collector retries."])
      `shouldBe` [Nothing, Just "WebFetcher retries."]

  describe "a line ends a sentence" $ do
    let five =
          [ "The collector retries 3 times.. On failures it does exponential backoff"
          , "There is no AI in the codebase yet."
          , "every scraper fix means a code change and a deploy"
          , "volunteers cant assignthemselves."
          , "I think raw vote totals from 2018 are stored with D and R columns"
          ]
    it "gives the user's five lines as six sentences, none spanning rows" $ do
      let ss = of_ five
      map snText ss `shouldBe`
        [ "The collector retries 3 times.."
        , "On failures it does exponential backoff"
        , "There is no AI in the codebase yet."
        , "every scraper fix means a code change and a deploy"
        , "volunteers cant assignthemselves."
        , "I think raw vote totals from 2018 are stored with D and R columns"
        ]
      all (\x -> snRow x == snEndRow x) ss `shouldBe` True
      map snRow ss `shouldBe` [0, 0, 1, 2, 3, 4]

    it "joins a hard-wrapped paragraph when the predicate says the rows continue" $ do
      let ls = parsePlan (T.unlines ["The collector retries a failed request three times before it gives up and", "marks the county for hand entry."])
      map snText (sentencesWith (== 0) ls) `shouldBe`
        ["The collector retries a failed request three times before it gives up and marks the county for hand entry."]
      let [s] = sentencesWith (== 0) ls
      (snRow s, snEndRow s) `shouldBe` (0, 1)
      length (sentences ls) `shouldBe` 2

    it "offers as join candidates only rows that do not end a sentence" $
      map (\(r, _, _) -> r) (joinCandidates (parsePlan (T.unlines five))) `shouldBe` [0, 2]

    it "gives the candidate's two rows' texts" $
      joinCandidates (parsePlan (T.unlines ["We would have to", "redeploy everything."]))
        `shouldBe` [(0, "We would have to", "redeploy everything.")]

    it "does not offer a row ending in a closing quote or paren after a stop" $
      joinCandidates (parsePlan (T.unlines ["It said \"done.\"", "next", "", "(see x.)", "next"])) `shouldBe` []

    it "joins rows by their pair key in the stored joins" $ do
      let ls = parsePlan (T.unlines ["We would have to", "redeploy everything."])
          k = pairKey "We would have to" "redeploy everything."
      map snText (sentencesJoined (M.fromList [(k, True)]) ls) `shouldBe` ["We would have to redeploy everything."]
      map snText (sentencesJoined (M.fromList [(k, False)]) ls) `shouldBe` ["We would have to", "redeploy everything."]
