module Olifant.BlockSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Olifant.Block
import Olifant.Language (Failure (..), Language (..), parseLanguage)
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
    open j `shouldSatisfy` \o -> "accept" `notElem` o && "stub" `elem` o && "kill" `notElem` o && "waive" `elem` o
    survivors (step' (Waive 2 "boundary" "inclusive by design") j) `shouldBe` []
    open (step' (Waive 2 "boundary" "inclusive by design") j) `shouldSatisfy` ("accept" `elem`)
  it "(c4) another draft is draft, never next: the open moves say draft before and after the first" $ do
    let r = step' (Drafted "y" "qwen" (Left red)) (step' StartDraft b0)
        g = step' (Drafted "x" "qwen" (Right ())) (step' StartDraft b0)
    open r `shouldBe` ["draft", "reject", "test"]
    open (step' (Judge []) g) `shouldBe` ["accept", "draft", "reject"]
    open (step' (Judge [m]) g) `shouldBe` ["stub", "waive", "draft", "reject"]
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
    step Accept j `shouldBe` Left "1 survivor(s): stub a test or waive each, then accept"
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
  it "(h) the HUD says where we are, and the next step" $ do
    hud b0 `shouldBe` "▸ test_f · red: AssertionError: assert None == Ok(None) · draft"
    hud (step' (Drafted "y" "qwen" (Left red)) b0) `shouldBe` "▸ test_f · draft 1 · red: AssertionError: assert None == Ok(None)"
    hud (step' (Judge [m]) (step' (Drafted "x" "qwen" (Right ())) b0)) `shouldBe` "▸ test_f · draft 1 · 1 survivor · stub a test or waive"
    hud (step' (Judge [m, m {muOperator = "number"}]) (step' (Drafted "x" "qwen" (Right ())) b0)) `shouldBe` "▸ test_f · draft 1 · 2 survivors · stub a test or waive each"
    hud (step' (Waive 2 "number" "r") (step' (Judge [m, m {muOperator = "number"}]) (step' (Drafted "x" "qwen" (Right ())) b0))) `shouldBe` "▸ test_f · draft 1 · 1 survivor · stub a test or waive"
    hud (step' (Judge []) (step' (Drafted "x" "qwen" (Right ())) b0)) `shouldBe` "▸ test_f · draft 1 · clean ✓ accept"
  it "(h3) a red circle's cue is honest: the person's edit keeps it, a model's failed draft drops it, a test stub is named first" $ do
    -- the person's own red edit is a draft, but no model has tried: draft is still the cue
    hud (step' (Edited (2, 2) (Left red)) b0) `shouldBe` "▸ test_f · draft 1 · red: AssertionError: assert None == Ok(None) · draft"
    -- the model's draft failed after the person's edit: the model already tried
    hud (step' (Drafted "y" "qwen" (Left red)) (step' (Edited (2, 2) (Left red)) b0)) `shouldBe` "▸ test_f · draft 2 · red: AssertionError: assert None == Ok(None)"
    -- the person edits after the model's failed draft: their text, so the cue is back
    hud (step' (Edited (2, 2) (Left red)) (step' (Drafted "y" "qwen" (Left red)) b0)) `shouldBe` "▸ test_f · draft 2 · red: AssertionError: assert None == Ok(None) · draft"
    -- a new test's stub, not named yet (no edit of the person's): name it first; reject is the way out
    let stub = begin 2 "src/a__test.py" (TestFor Nothing) "" (4, 6) "src/a__test.py" (Failure Nothing "a new test: write its name and assertion" "")
    hud stub `shouldBe` "▸ src/a__test.py · red: a new test: write its name and assertion · name it, then draft · reject drops it"
    -- a kill's stub is named already (the HUD's first field): the draft writes its arrangement and assertion; no failure to show
    let killed = begin 3 "src/a__test.py" (TestFor (Just m)) "" (9, 11) "src/a__test.py::test_boundary_line2" (Failure Nothing "" "")
    hud killed `shouldBe` "▸ test_boundary_line2 · draft · reject drops it"
    -- the person typed in the stub, red: still no model's draft, so the stub's cue stands
    hud (step' (Edited (9, 11) (Left red)) killed) `shouldBe` "▸ test_boundary_line2 · draft 1 · red: AssertionError: assert None == Ok(None) · draft · reject drops it"
    -- a begin's stub once its test is named (tested: the block's test is the new one): the draft next
    hud (step' (Edited (4, 6) (Left red)) stub {blockTestId = "src/a__test.py::test_reopens_again"}) `shouldBe` "▸ test_reopens_again · draft 1 · red: AssertionError: assert None == Ok(None) · draft · reject drops it"
    hud (step' (Edited (4, 6) (Left red)) stub {blockTestId = "src/a__test.py::test_name"}) `shouldSatisfy` ("· name it, then draft · reject drops it" `T.isSuffixOf`)
    -- a model's draft of it failed: the person reads it, no cue
    hud (step' (Drafted "x" "qwen" (Left red)) killed) `shouldBe` "▸ test_boundary_line2 · draft 1 · red: AssertionError: assert None == Ok(None)"
  it "(h4) a new test's stub drafted red at its assertion says what accept does; a kill's test stays clean" $ do
    let stub = begin 2 "src/a__test.py" (TestFor Nothing) "" (4, 6) "src/a__test.py::test_handle" (Failure Nothing "" "")
        ready = step' (Judge []) (step' (Drafted "    table = Table()" "qwen" (Right ())) (step' StartDraft stub))
    hud ready `shouldBe` "▸ test_handle · draft 1 · red ✓ accept · go make it pass"
    open ready `shouldBe` ["accept", "draft", "reject"]
    -- its draft failed: today's red, no cue
    hud (step' (Drafted "    table = Table()" "qwen" (Left red)) (step' StartDraft stub)) `shouldBe` "▸ test_handle · draft 1 · red: AssertionError: assert None == Ok(None)"
    let killed = begin 3 "src/a__test.py" (TestFor (Just m)) "" (9, 11) "src/a__test.py::test_boundary_line2" (Failure Nothing "" "")
    hud (step' (Judge []) (step' (Drafted "x" "qwen" (Right ())) (step' StartDraft killed))) `shouldBe` "▸ test_boundary_line2 · draft 1 · clean ✓ accept"
  it "(h2) kill takes the survivor on the line, or the top-ranked unwaived one when the line has none" $ do
    let g = step' (Drafted "x" "qwen" (Right ())) b0
        first = m {muLine = 5, muOperator = "number", muFrom = "1", muTo = "2"}
        onTwo = m
        third = m {muLine = 7, muOperator = "compare"}
        j = step' (Judge [first, onTwo, third]) g
    killTarget 2 j `shouldBe` Right onTwo
    killTarget 9 j `shouldBe` Right first
    step (Kill 9) j `shouldBe` Right j
    -- a waived survivor is passed over: the next in order is the top
    let w = step' (Waive 5 "number" "any count") j
    killTarget 9 w `shouldBe` Right onTwo
    killTarget 5 w `shouldBe` Right onTwo
    let none = step' (Waive 7 "compare" "r") (step' (Waive 2 "boundary" "r") w)
    killTarget 2 none `shouldBe` Left "no survivor left: accept"
    step (Kill 2) none `shouldBe` Left "no survivor left: accept"
    killTarget 2 g `shouldSatisfy` isLeft
    -- the person's word for the move is stub
    step (Kill 2) b0 `shouldBe` Left "`stub` is not open here"
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
      let (at, stub, name) = killStub py test (Mutant 5 "boundary" ">" ">=" "" Nothing False)
      (at, name) `shouldBe` (4, "test_boundary_line5")
      stub `shouldSatisfy` ("def ${1:test_boundary_line5}() -> None:" `T.isInfixOf`)
      -- a begin's stub is named by the person: its placeholder is test_name
      snd (testStub py test Nothing) `shouldSatisfy` ("def test_${1:name}() -> None:" `T.isInfixOf`)
      stubName `shouldBe` "test_name"
      stub `shouldSatisfy` ("assert ${2:" `T.isInfixOf`)
      stub `shouldSatisfy` ("olifant: arrange" `T.isInfixOf`)
    it "(c) the stubbed test's placeholder becomes a stmt hole at its indentation" $ do
      let (_, stub, _) = killStub py test (Mutant 5 "boundary" ">" ">=" "" Nothing False)
          stubbed = test <> stub
      testDraftTemplate py stubbed 7 Nothing `shouldSatisfy` isRight
      testDraftTemplate py stubbed 1 Nothing `shouldSatisfy` isLeft
    it "(c2) a stub's assertion left `...` is a stmt hole too, worded from the test's name (a kill's: and its mutant)" $ do
      let (_, stub, _) = killStub py test (Mutant 5 "boundary" ">" ">=" "" Nothing False)
          stubbed = test <> T.replace "${2:...}" "..." stub
      T.lines stubbed !! 7 `shouldBe` "    " <> stubAssertion
      let instruction = assertionInstruction "test_reopens_again" Nothing
      instruction `shouldBe` "assert what 'reopens again' means; one assert, the subject is `result`"
      fmap (const ()) (testDraftTemplate py stubbed 7 (Just (8, instruction))) `shouldBe` Right ()
      testDraftTemplate py stubbed 7 (Just (7, instruction)) `shouldSatisfy` isLeft
      assertionInstruction "test_boundary_line5" (Just (Mutant 5 "boundary" ">" ">=" "" Nothing False))
        `shouldBe` "assert what 'boundary line5' means; one assert, the subject is `result`; it passes on the code as it is and fails when line 5's `>` becomes `>=`"
    describe "the kill stub's name, from the mutant" $ do
      let module' = T.unlines ["class Book:", "    def handle(self, input: BookInput) -> BookOutput:", "        table = self.tables.at(input.table_id)", "        if input.party >= table.seats:", "            return TableNotFound(f\"there is no table {input.table_id}\")"]
          boundary = Mutant 4 "boundary" ">" ">=" module' Nothing False
      it "(n1) a string: test_ and the literal's first four words" $ do
        let m = Mutant 5 "string" "f\"there is no table {input.table_id}\"" "\"\"" module' Nothing False
        stubNameFor m test (langKeywords py) `shouldBe` "test_there_is_no_table"
        let (_, stub, _) = killStub py test m
        stub `shouldSatisfy` ("def ${1:test_there_is_no_table}() -> None:" `T.isInfixOf`)
        -- a name is a Python name: ASCII letters, digits and _ only
        stubNameFor m {muFrom = "\"½ price is fine\""} test (langKeywords py) `shouldBe` "test_price_is_fine"
        -- and 60 characters at most, cut between words
        stubNameFor m {muFrom = "\"supercalifragilistic expialidocious_and_more_words_here antidisestablishmentarianism pneumonoultramicroscopic\""} test (langKeywords py)
          `shouldBe` "test_supercalifragilistic_expialidocious_and_more_words_here"
      it "(n2) any other operator: test_, the operator, the mutated line's names (no keywords, self or attribute heads), four at most; a line left with none, what it changed from" $ do
        stubNameFor boundary test (langKeywords py) `shouldBe` "test_boundary_party_seats"
        stubNameFor (Mutant 1 "arith" "+" "-" "total = price * count - tax - fee  # f\"no\" names here" Nothing False) test (langKeywords py) `shouldBe` "test_arith_total_price_count_tax"
        stubNameFor (Mutant 3 "delete-call" "self.tables.at(input.table_id)" "pass" (T.replace "table = self.tables.at(input.table_id)" "pass" module') Nothing False) test (langKeywords py) `shouldBe` "test_delete_call_at_table_id"
      it "(n3) a name the test file has already gets _2, _3, …" $ do
        let taken = test <> T.unlines ["", "", "def test_boundary_party_seats() -> None:", "    assert True", "", "", "def test_boundary_party_seats_2() -> None:", "    assert True"]
        stubNameFor boundary taken (langKeywords py) `shouldBe` "test_boundary_party_seats_3"
        -- an async test has its name too
        stubNameFor boundary (test <> "\n\nasync def test_boundary_party_seats() -> None:\n    assert True\n") (langKeywords py) `shouldBe` "test_boundary_party_seats_2"
      it "(n4) a line of keywords and self only: test_, the operator and the line" $ do
        let selfOnly = T.unlines ["class A:", "    def ok(self) -> bool:", "        return not self"]
        stubNameFor (Mutant 3 "negate" "self" "not self" selfOnly Nothing False) test (langKeywords py) `shouldBe` "test_negate_line3"
        stubNameFor (Mutant 3 "return-none" "return self" "return None" selfOnly Nothing False) test (langKeywords py) `shouldBe` "test_return_none_line3"
