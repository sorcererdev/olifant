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

  describe "{= expressions =}" $ do
    it "prints a name in snake case by default" $
      renderWith [("cmd", nameV "RefundInvoice")] "fn {= cmd =}()" `shouldBe` Right "fn refund_invoice()"
    it "applies case filters" $
      renderWith [("cmd", nameV "refund_invoice")] "{= cmd|pascal =}Input {= cmd | constant =}"
        `shouldBe` Right "RefundInvoiceInput REFUND_INVOICE"
    it "chains inflection and case filters" $
      renderWith [("name", nameV "blog_post")] "{= name|plural|kebab =}" `shouldBe` Right "blog-posts"
    it "treats text as a name for case filters" $
      renderWith [("table", VText "blog_posts")] "Create{= table|pascal =}" `shouldBe` Right "CreateBlogPosts"
    it "reads record fields" $
      renderWith [("f", VRecord (M.fromList [("name", nameV "user_id"), ("type", VText "integer")]))] "t.{= f.type =} :{= f.name|snake =}"
        `shouldBe` Right "t.integer :user_id"
    it "passes {{, {% and Haskell's <$> through as text; a string literal emits {=" $
      renderWith [] "a {{ b }} {% c %} x <$> y <$ z {= '{=' =}" `shouldBe` Right "a {{ b }} {% c %} x <$> y <$ z {="
    it "reports undefined variables" $
      renderWith [] "{= missing =}" `shouldBe` Left (UndefinedVariable "missing")
    it "rejects unknown filters when parsing" $
      parseTemplate "{= x|shout =}" `shouldSatisfy` isSyntaxError
    it "rejects unclosed tags" $
      parseTemplate "{= x" `shouldSatisfy` isSyntaxError

  describe "hints for invented syntax (v9)" $ do
    let err src = either describeTemplateError (const "") (parseTemplate src)
    it "suggests the filter a made-up one means" $ do
      err "{= name|capitalize =}" `shouldSatisfy` T.isInfixOf "did you mean `pascal`"
      err "{= name|kebabcase =}" `shouldSatisfy` T.isInfixOf "did you mean `kebab`"
    it "says filters are written after a bar, not called" $ do
      err "{= pascalCase(name) =}" `shouldSatisfy` T.isInfixOf "name|pascal"
      err "{= name:pascal =}" `shouldSatisfy` T.isInfixOf "name|pascal"
  describe "mustache text (v9: a model wrote {{ name|pascal }})" $ do
    let err src = either describeTemplateError (const "") (parseTemplate src)
    it "refuses {{ name }} when the template has no {= tag, saying how Olifant writes it" $ do
      err "| {{ name|pascal }}" `shouldSatisfy` T.isInfixOf "{= name|pascal =}"
      err "{{input.name}}" `shouldSatisfy` T.isInfixOf "{= input.name =}"
    it "accepts literal braces: Rust's {{}}, and text that also has real tags" $ do
      parseTemplate "format!(\"{{}}\", x)" `shouldSatisfy` either (const False) (const True)
      parseTemplate "{{ x }} and {= y =}" `shouldSatisfy` either (const False) (const True)
  describe "holes" $ do
    it "renders a hole as a marker carrying its name, kind and rendered instruction" $
      renderWith [("p", VText "A::B")] "x => {= ? status: expr | the status for {= p =} =},"
        `shouldBe` Right "x => \10214olifant-hole status|expr|the status for A::B\10215,"
    it "keeps a marker on one line: an instruction's newlines become spaces (review I3)" $
      renderWith [("i", VText "reset\nand log")] "{= ? h: expr | {= i =} =}" `shouldBe` Right "\10214olifant-hole h|expr|reset and log\10215"
    it "needs a name and a kind" $
      parseTemplate "{= ? : expr | x =}" `shouldSatisfy` isSyntaxError
    it "counts the instruction's variables as the template's" $
      fmap freeVariables (parseTemplate "{= ? h: expr | for {= p =} =}") `shouldBe` Right (S.fromList ["p"])
    it "escaped escapes backslashes and double quotes" $
      renderWith [("t", VText "a \"b\" \\n")] "{= t|escaped =}" `shouldBe` Right "a \\\"b\\\" \\\\n"
  describe "{= if =} and {= for =}" $ do
    it "picks the first true branch" $ do
      let src = "{= if kind == \"a\" =}A{= elif kind == \"b\" =}B{= else =}C{= end =}"
      map (\k -> renderWith [("kind", VText k)] src) ["a", "b", "z"] `shouldBe` map Right ["A", "B", "C"]
    it "compares names with text spelled in snake case" $
      renderWith [("t", nameV "references")] "{= if t == \"references\" =}yes{= end =}" `shouldBe` Right "yes"
    it "loops with loop.last" $
      renderWith [("xs", VList (map nameV ["a_a", "b_b", "c_c"]))] "{= for x in xs =}{= x =}{= if not loop.last =}, {= end =}{= end =}"
        `shouldBe` Right "a_a, b_b, c_c"
    it "treats a missing optional list as empty" $
      renderWith [("xs", VNull)] "[{= for x in xs =}{= x =}{= end =}]" `shouldBe` Right "[]"
    it "trims whitespace around {=- and -=} (a Rails migration body)" $ do
      let src = "create_table :posts do |t|\n{=- for f in fields =}\n  t.{= f.type =} :{= f.name =}\n{=- end =}\n\n  t.timestamps\n"
          field n t = VRecord (M.fromList [("name", nameV n), ("type", VText t)])
      renderWith [("fields", VList [field "title" "string", field "body" "text"])] src
        `shouldBe` Right "create_table :posts do |t|\n  t.string :title\n  t.text :body\n\n  t.timestamps\n"
    it "trims leading whitespace after -%}" $
      renderWith [] "{= if true -=}\n   x{= end =}" `shouldBe` Right "x"
    it "reports unbalanced tags" $ do
      parseTemplate "{= if true =}x" `shouldSatisfy` isSyntaxError
      parseTemplate "x{= end =}" `shouldSatisfy` isSyntaxError

  describe "evaluate" $ do
    it "keeps the type of a single expression" $
      (parseTemplate "{= name != name|singular =}" >>= evaluate (M.fromList [("name", nameV "blog_posts")]))
        `shouldBe` Right (VBool True)
    it "renders anything else to text" $
      (parseTemplate "x{= name =}" >>= evaluate (M.fromList [("name", nameV "a_b")])) `shouldBe` Right (VText "xa_b")

  describe "timestamp" $
    it "formats times for migration file names" $ do
      let now = UTCTime (fromGregorian 2026 9 27) (secondsToDiffTime (15 * 3600 + 30 * 60))
      renderWith [("now", VTime now)] "{= now|timestamp =}" `shouldBe` Right "20260927153000"

  describe "freeVariables" $
    it "lists variables read from outside, not loop-bound ones" $
      fmap freeVariables (parseTemplate "{= a =}{= for x in xs =}{= x.b =}{= loop.last =}{= c =}{= end =}")
        `shouldBe` Right (S.fromList ["a", "xs", "c"])

  describe "exemplar" $ do
    let refund = either (error . T.unpack) id (parseName "refund_invoice")
        bindings = [Binding "command_name" refund]
        renderExemplar value src = do
          t <- either (Left . describeExemplarError) Right (exemplar bindings src)
          either (Left . describeTemplateError) Right (render (M.fromList [("command_name", value)]) t)
    it "renames every case form, plurals, and names embedded in identifiers" $
      renderExemplar (nameV "cancel_subscription")
        "struct RefundInvoice; RefundInvoiceInput; fn refund_invoice(); REFUND_INVOICE; \"refund-invoice\"; Refund Invoice; refund invoice; refund_invoices; refundInvoiceId; get_refund_invoice"
        `shouldBe` Right
          "struct CancelSubscription; CancelSubscriptionInput; fn cancel_subscription(); CANCEL_SUBSCRIPTION; \"cancel-subscription\"; Cancel Subscription; cancel subscription; cancel_subscriptions; cancelSubscriptionId; get_cancel_subscription"
    it "leaves partial-word matches alone" $
      renderExemplar (nameV "cancel_subscription") "prefund_invoice refund_invoicex RefundInvoicer"
        `shouldBe` Right "prefund_invoice refund_invoicex RefundInvoicer"
    it "rejects single-word examples, whose case forms are ambiguous" $
      exemplar [Binding "command_name" (either (error . T.unpack) id (parseName "refund"))] "x"
        `shouldSatisfy` \case
          Left (ExampleNeedsTwoWords "command_name" _) -> True
          _ -> False
    it "rejects two bindings that produce the same form" $
      exemplar [Binding "a" refund, Binding "b" refund] "x" `shouldSatisfy` \case
        Left (AmbiguousForm _) -> True
        _ -> False
    it "reproduces the exemplar exactly when rendered with its own example (identity)" $ hedgehog $ do
      let forms = ["RefundInvoice", "refund_invoice", "refund-invoice", "refundInvoice", "REFUND_INVOICE", "Refund Invoice", "refund invoice", "refund_invoices", "RefundInvoices"]
      pieces <- forAll (Gen.list (Range.linear 0 30) (Gen.choice [Gen.element forms, Gen.element ["_", "-", " ", "\n", "s", "x", "X", "(", "\"", "p"], Gen.text (Range.linear 0 4) Gen.unicode]))
      let src = T.concat pieces
      renderExemplar (VName refund) src === Right src

  describe "rust_string" $
    it "escapes text for the inside of a Rust format-string literal" $
      renderWith [("t", VText "Rec { .. } \"late\" \\n")] "{= t|rust_string =}" `shouldBe` Right "Rec {{ .. }} \\\"late\\\" \\\\n"
  describe "python_string" $
    it "escapes text for the inside of a double-quoted Python string" $
      renderWith [("t", VText "the \"late\" fee\nC:\\x")] "{= t|python_string =}" `shouldBe` Right "the \\\"late\\\" fee\\nC:\\\\x"
  describe "parens" $
    it "wraps multi-word text in parentheses unless it already is one group" $
      map (\t -> renderWith [("t", VText t)] "{= t|parens =}") ["Int", "List String", "(List String)", "(a) (b)", "  Maybe Int ", "List\tString"]
        `shouldBe` map Right ["Int", "(List String)", "(List String)", "((a) (b))", "(Maybe Int)", "(List\tString)"]
