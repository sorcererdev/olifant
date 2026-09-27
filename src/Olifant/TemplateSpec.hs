module Olifant.TemplateSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Olifant.Naming (parseName)
import Olifant.Template
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

nameV :: Text -> Value
nameV = VName . either (error . T.unpack) id . parseName

renderWith :: [(Text, Value)] -> Text -> Either TemplateError Text
renderWith vars src = parseTemplate src >>= render (M.fromList vars)

isSyntaxError :: Either TemplateError a -> Bool
isSyntaxError = \case
  Left (SyntaxError _ _) -> True
  _ -> False

spec :: Spec
spec = do
  describe "literal text" $
    it "passes through unchanged" $ hedgehog $ do
      t <- forAll (Gen.text (Range.linear 0 200) (Gen.filter (`notElem` ("{<" :: String)) Gen.unicode))
      renderWith [] t === Right t

  describe "{{ expressions }}" $ do
    it "prints a name in snake case by default" $
      renderWith [("cmd", nameV "RefundInvoice")] "fn {{ cmd }}()" `shouldBe` Right "fn refund_invoice()"
    it "applies case filters" $
      renderWith [("cmd", nameV "refund_invoice")] "{{ cmd|pascal }}Input {{ cmd | constant }}"
        `shouldBe` Right "RefundInvoiceInput REFUND_INVOICE"
    it "chains inflection and case filters" $
      renderWith [("name", nameV "blog_post")] "{{ name|plural|kebab }}" `shouldBe` Right "blog-posts"
    it "treats text as a name for case filters" $
      renderWith [("table", VText "blog_posts")] "Create{{ table|pascal }}" `shouldBe` Right "CreateBlogPosts"
    it "reads record fields" $
      renderWith [("f", VRecord (M.fromList [("name", nameV "user_id"), ("type", VText "integer")]))] "t.{{ f.type }} :{{ f.name|snake }}"
        `shouldBe` Right "t.integer :user_id"
    it "prints string literals, which is how to emit {{ or <$" $
      renderWith [] "a {{ '{{' }} b {{ \"<$\" }}" `shouldBe` Right "a {{ b <$"
    it "reports undefined variables" $
      renderWith [] "{{ missing }}" `shouldBe` Left (UndefinedVariable "missing")
    it "rejects unknown filters when parsing" $
      parseTemplate "{{ x|shout }}" `shouldSatisfy` isSyntaxError
    it "rejects unclosed tags" $
      parseTemplate "{{ x" `shouldSatisfy` isSyntaxError

  describe "holes" $
    it "are refused until step 3, with the line number" $
      parseTemplate "a\nb <$ body: block | x $>" `shouldBe` Left (HolesNotYetSupported 2)
