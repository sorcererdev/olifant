module Olifant.TemplateSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
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

  describe "{% if %} and {% for %}" $ do
    it "picks the first true branch" $ do
      let src = "{% if kind == \"a\" %}A{% elif kind == \"b\" %}B{% else %}C{% endif %}"
      map (\k -> renderWith [("kind", VText k)] src) ["a", "b", "z"] `shouldBe` map Right ["A", "B", "C"]
    it "compares names with text spelled in snake case" $
      renderWith [("t", nameV "references")] "{% if t == \"references\" %}yes{% endif %}" `shouldBe` Right "yes"
    it "loops with loop.last" $
      renderWith [("xs", VList (map nameV ["a_a", "b_b", "c_c"]))] "{% for x in xs %}{{ x }}{% if not loop.last %}, {% endif %}{% endfor %}"
        `shouldBe` Right "a_a, b_b, c_c"
    it "treats a missing optional list as empty" $
      renderWith [("xs", VNull)] "[{% for x in xs %}{{ x }}{% endfor %}]" `shouldBe` Right "[]"
    it "trims whitespace around {%- and -%} (a Rails migration body)" $ do
      let src = "create_table :posts do |t|\n{%- for f in fields %}\n  t.{{ f.type }} :{{ f.name }}\n{%- endfor %}\n\n  t.timestamps\n"
          field n t = VRecord (M.fromList [("name", nameV n), ("type", VText t)])
      renderWith [("fields", VList [field "title" "string", field "body" "text"])] src
        `shouldBe` Right "create_table :posts do |t|\n  t.string :title\n  t.text :body\n\n  t.timestamps\n"
    it "trims leading whitespace after -%}" $
      renderWith [] "{% if true -%}\n   x{% endif %}" `shouldBe` Right "x"
    it "reports unbalanced tags" $ do
      parseTemplate "{% if true %}x" `shouldSatisfy` isSyntaxError
      parseTemplate "x{% endfor %}" `shouldSatisfy` isSyntaxError

  describe "evaluate" $ do
    it "keeps the type of a single expression" $
      (parseTemplate "{{ name != name|singular }}" >>= evaluate (M.fromList [("name", nameV "blog_posts")]))
        `shouldBe` Right (VBool True)
    it "renders anything else to text" $
      (parseTemplate "x{{ name }}" >>= evaluate (M.fromList [("name", nameV "a_b")])) `shouldBe` Right (VText "xa_b")

  describe "timestamp" $
    it "formats times for migration file names" $ do
      let now = UTCTime (fromGregorian 2026 9 27) (secondsToDiffTime (15 * 3600 + 30 * 60))
      renderWith [("now", VTime now)] "{{ now|timestamp }}" `shouldBe` Right "20260927153000"

  describe "freeVariables" $
    it "lists variables read from outside, not loop-bound ones" $
      fmap freeVariables (parseTemplate "{{ a }}{% for x in xs %}{{ x.b }}{{ loop.last }}{{ c }}{% endfor %}")
        `shouldBe` Right (S.fromList ["a", "xs", "c"])
