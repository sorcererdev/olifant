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
