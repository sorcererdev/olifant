module Olifant.NamingSpec (spec) where

import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Olifant.Naming
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

name :: Text -> Name
name t = either (error . T.unpack) id (parseName t)

spec :: Spec
spec = do
  describe "parseName" $ do
    it "reads every supported case as the same words" $
      map (fmap nameWords . parseName) ["blog_post", "blog-post", "BlogPost", "blogPost", "BLOG_POST", "Blog Post", "blog post"]
        `shouldBe` replicate 7 (Right ["blog", "post"])
    it "splits an acronym from the word after it" $
      nameWords (name "HTTPServer") `shouldBe` ["http", "server"]
    it "keeps digits attached to their word" $
      nameWords (name "http2_client") `shouldBe` ["http2", "client"]
    it "rejects empty names, leading digits and punctuation" $ do
      parseName "" `shouldSatisfy` isLeft
      parseName "2fast" `shouldSatisfy` isLeft
      parseName "../etc" `shouldSatisfy` isLeft

  describe "render" $ do
    it "renders every case" $
      map (`render` name "blog_post") allCases
        `shouldBe` ["blog_post", "blog-post", "BlogPost", "blogPost", "BLOG_POST", "Blog Post", "blog post"]
    it "round-trips through every case" $ hedgehog $ do
      ws <- forAll (Gen.list (Range.linear 1 4) (Gen.text (Range.linear 2 8) Gen.lower))
      c <- forAll (Gen.element allCases)
      let n = name (T.intercalate "_" ws)
      parseName (render c n) === Right n

  describe "caseName" $
    it "is the inverse of caseFromText" $
      map (caseFromText . caseName) allCases `shouldBe` map Just allCases

  describe "pluralize / singularize" $ do
    let corpus =
          [ "post", "comment", "user", "category", "company", "box", "class", "match", "wish"
          , "person", "child", "status", "bus", "leaf", "sheep", "series", "day", "key"
          , "address", "case", "house", "invoice", "subscription", "quiz", "gas", "blog_post" ]
        snakeOf f = render Snake . f . name
    it "pluralizes regular, -es, -ies, irregular and uncountable words" $
      map (snakeOf pluralize) ["post", "box", "category", "day", "person", "sheep", "blog_post"]
        `shouldBe` ["posts", "boxes", "categories", "days", "people", "sheep", "blog_posts"]
    it "keeps plural words plural" $
      map (snakeOf pluralize) ["posts", "people", "categories"] `shouldBe` ["posts", "people", "categories"]
    it "singularizes" $
      map (snakeOf singularize) ["posts", "boxes", "categories", "people", "addresses", "statuses", "blog_posts"]
        `shouldBe` ["post", "box", "category", "person", "address", "status", "blog_post"]
    it "singularize undoes pluralize across the corpus" $ hedgehog $ do
      w <- forAll (Gen.element corpus)
      snakeOf (singularize . pluralize) w === w
    it "pluralize is idempotent across the corpus" $ hedgehog $ do
      w <- forAll (Gen.element corpus)
      snakeOf (pluralize . pluralize) w === snakeOf pluralize w
