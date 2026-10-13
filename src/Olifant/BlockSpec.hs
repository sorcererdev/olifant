module Olifant.BlockSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Olifant.Block
import Olifant.Language (Failure (..), parseLanguage)
import Data.Text.IO qualified as TIO
import Olifant.Mutate (Mutant (..))
import Test.Hspec
import Test.Hspec.Hedgehog (forAll, hedgehog, success, (===))

spec :: Spec
spec = describe "Olifant.Block" $ do
  let red = Failure (Just 14) "AssertionError: assert None == Ok(None)" ""
      b0 = begin 1 "src/a.py" Code "def f():\n    raise NotImplementedError()\n" (2, 2) "src/a__test.py::test_f" red
      m = Mutant 2 "boundary" "<" "<=" "" Nothing False
      step' mv = either (error . T.unpack) id . step mv
      genMove :: Gen Move
      genMove =
        Gen.element
          [ StartDraft,
            DraftFailed,
            Drafted "x" "m" (Right ()),
            Drafted "y" "m" (Left red),
            Edited (2, 2) (Right ()),
            Applied (2, 2) (Right ()),
            Applied (2, 2) (Left red),
            Judge [],
            Judge [m],
            Judge [m, m {muOperator = "number"}],
            Rejudge,
            Waive 2 "boundary" "r",
            Waive 2 "number" "r",
            Accept,
            Reject
          ]
  it "(a) begins Red with the failure, draft/reject/test open, accept closed" $ do
    blockState b0 `shouldBe` Red red
    open b0 `shouldSatisfy` \o -> "draft" `elem` o && "reject" `elem` o && "accept" `notElem` o
  it "(b) a green draft is Green; a red one is Red and kept as a draft to edit" $ do
    let g = step' (Drafted "    return Ok(None)" "qwen" (Right ())) (step' StartDraft b0)
    blockState g `shouldBe` Green
    map draftBy (blockDrafts g) `shouldBe` ["qwen"]
    let r = step' (Drafted "    return None" "qwen" (Left red)) (step' StartDraft b0)
    blockState r `shouldBe` Red red
    length (blockDrafts r) `shouldBe` 1
  it "(c) judged clean opens accept; a survivor keeps it closed until waived" $ do
    let g = step' (Drafted "x" "qwen" (Right ())) (step' StartDraft b0)
    open (step' (Judge []) g) `shouldSatisfy` ("accept" `elem`)
    let j = step' (Judge [m]) g
    open j `shouldSatisfy` \o -> "accept" `notElem` o && "kill" `elem` o && "waive" `elem` o
    survivors (step' (Waive 2 "boundary" "inclusive by design") j) `shouldBe` []
    open (step' (Waive 2 "boundary" "inclusive by design") j) `shouldSatisfy` ("accept" `elem`)
  it "(c2) a waiver is for one operator: another on the same line stays a survivor" $ do
    let g = step' (Drafted "x" "qwen" (Right ())) (step' StartDraft b0)
        j = step' (Judge [m, m {muOperator = "number", muFrom = "1", muTo = "2"}]) g
        one = step' (Waive 2 "boundary" "inclusive") j
    map muOperator (survivors one) `shouldBe` ["number"]
    open one `shouldSatisfy` ("accept" `notElem`)
    step (Waive 2 "boundary" "again") one `shouldSatisfy` isLeft
    survivors (step' (Waive 2 "number" "any count") one) `shouldBe` []
  it "(c3) Rejudge takes a judged block back to Green, from Judged only" $ do
    let j = step' (Judge [m]) (step' (Drafted "x" "qwen" (Right ())) b0)
    blockState (step' Rejudge j) `shouldBe` Green
    step Rejudge b0 `shouldSatisfy` isLeft
    open (step' Rejudge j) `shouldBe` open (step' (Drafted "x" "qwen" (Right ())) b0)
  it "(d) accept from anything but a clean Judged is refused, with a reason" $ do
    step Accept b0 `shouldSatisfy` either ("test" `T.isInfixOf`) (const False)
    let j = step' (Judge [m]) (step' (Drafted "x" "qwen" (Right ())) b0)
    step Accept j `shouldSatisfy` either ("survivor" `T.isInfixOf`) (const False)
  it "(e) editing by hand makes a 'you' draft; a hand-written green body is judged too" $ do
    let e = step' (Edited (2, 3) (Right ())) b0
    blockState e `shouldBe` Green
    map draftBy (blockDrafts e) `shouldBe` ["you"]
    blockSpan e `shouldBe` (2, 3)
    map draftBy (blockDrafts (step' (Edited (2, 4) (Right ())) e)) `shouldBe` ["you"]
  it "(e2) an applied draft, tested, lands its outcome and makes no 'you' draft; with no draft it is refused" $ do
    let d = step' (Drafted "x" "qwen" (Right ())) (step' StartDraft b0)
        a = step' (Applied (2, 2) (Left red)) d
    blockState a `shouldBe` Red red
    map draftBy (blockDrafts a) `shouldBe` ["qwen"]
    blockState (step' (Applied (2, 2) (Right ())) a) `shouldBe` Green
    step (Applied (2, 2) (Right ())) b0 `shouldSatisfy` isLeft
  it "(f) a test that cannot run is Broken, and nothing else is open but reject/test (Review Focus 2)" $ do
    let br = step' (CannotRun "ImportError: nope") b0
    blockState br `shouldBe` Broken "ImportError: nope"
    open br `shouldBe` ["test", "reject"]
  it "(g) reject from any pending state; nothing after it" $ do
    blockState (step' Reject b0) `shouldBe` Rejected
    step StartDraft (step' Reject b0) `shouldSatisfy` isLeft
  it "(g2) a draft starting is Drafting (only reject open); a failed one puts the state it began from back" $ do
    let d = step' StartDraft b0
    blockState d `shouldBe` Drafting (Red red)
    open d `shouldBe` ["reject"]
    hud d `shouldSatisfy` ("drafting…" `T.isSuffixOf`)
    step' DraftFailed d `shouldBe` b0
    step Accept d `shouldSatisfy` isLeft
    step StartDraft d `shouldSatisfy` isLeft
    step DraftFailed b0 `shouldSatisfy` isLeft
    -- next on a clean judged block, failed: clean judged again, accept open
    let j = step' (Judge []) (step' (Drafted "x" "qwen" (Right ())) (step' StartDraft b0))
    open (step' DraftFailed (step' StartDraft j)) `shouldSatisfy` ("accept" `elem`)
  it "(h) the HUD says where we are" $ do
    hud b0 `shouldBe` "▸ test_f · red: AssertionError: assert None == Ok(None)"
    hud (step' (Judge [m]) (step' (Drafted "x" "qwen" (Right ())) b0)) `shouldBe` "▸ test_f · draft 1 · 1 survivor"
    hud (step' (Judge []) (step' (Drafted "x" "qwen" (Right ())) b0)) `shouldBe` "▸ test_f · draft 1 · clean ✓ accept"
  it "(i) property: Accepted is reachable only through a clean Judged" $ hedgehog $ do
    moves <- forAll (Gen.list (Range.linear 0 12) genMove)
    let walk (b, lastJudgedClean) mv = case step mv b of
          Left _ -> (b, lastJudgedClean)
          Right b' -> (b', case mv of Judge ms -> all (isJust . muWaived) ms; Waive {} -> null (survivors b'); Rejudge -> False; _ -> lastJudgedClean)
        (end, clean) = foldl walk (b0, False) moves
    if blockState end == Accepted then clean === True else success

  describe "drafts" $ do
    pythonToml <- runIO (TIO.readFile "packs/python-htmx/languages/python.toml")
    let Right py = parseLanguage "python" "packs/python-htmx" pythonToml
        file = T.unlines ["class CloseTable:", "    def handle(self, input: CloseTableInput) -> CloseTableOutput:", "        raise NotImplementedError()"]
        test = T.unlines ["def test_closes_a_table() -> None:", "    close = CloseTable(repo)", "    assert close.handle(CloseTableInput(id=\"1\")) == Ok(None)"]
        red = Failure (Just 3) "AssertionError: assert None == Ok(None)" ""
    it "(a) the draft template puts one block hole at the span with the test's words, assertion and failure" $ do
      let Right (_, instruction) = draftTemplate py file (3, 3) "src/x__test.py::test_closes_a_table" red test
      instruction `shouldSatisfy` ("closes a table" `T.isInfixOf`)
      instruction `shouldSatisfy` ("assert close.handle" `T.isInfixOf`)
      instruction `shouldSatisfy` ("AssertionError" `T.isInfixOf`)
      instruction `shouldSatisfy` ("least code" `T.isInfixOf`)
    it "(b) the kill stub is a new test with the name and assertion as tab stops and the arrangement as a hole" $ do
      let (at, stub) = killStub py test (Mutant 5 "boundary" ">" ">=" "" Nothing False)
      at `shouldBe` 4
      stub `shouldSatisfy` ("def test_${1:" `T.isInfixOf`)
      stub `shouldSatisfy` ("assert ${2:" `T.isInfixOf`)
      stub `shouldSatisfy` ("olifant: arrange" `T.isInfixOf`)
    it "(c) the stubbed test's placeholder becomes a stmt hole at its indentation" $ do
      let (_, stub) = killStub py test (Mutant 5 "boundary" ">" ">=" "" Nothing False)
          stubbed = test <> stub
      testDraftTemplate py stubbed 7 `shouldSatisfy` isRight
      testDraftTemplate py stubbed 1 `shouldSatisfy` isLeft
