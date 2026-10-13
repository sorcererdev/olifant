module Olifant.BlockSpec (spec) where

import Data.Either (isLeft)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Olifant.Block
import Olifant.Language (Failure (..))
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
            Drafted "x" "m" (Right ()),
            Drafted "y" "m" (Left red),
            Edited (2, 2) (Right ()),
            Judge [],
            Judge [m],
            Waive 2 "r",
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
    survivors (step' (Waive 2 "inclusive by design") j) `shouldBe` []
    open (step' (Waive 2 "inclusive by design") j) `shouldSatisfy` ("accept" `elem`)
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
  it "(f) a test that cannot run is Broken, and nothing else is open but reject/test (Review Focus 2)" $ do
    let br = step' (CannotRun "ImportError: nope") b0
    blockState br `shouldBe` Broken "ImportError: nope"
    open br `shouldBe` ["test", "reject"]
  it "(g) reject from any pending state; nothing after it" $ do
    blockState (step' Reject b0) `shouldBe` Rejected
    step StartDraft (step' Reject b0) `shouldSatisfy` isLeft
  it "(h) the HUD says where we are" $ do
    hud b0 `shouldBe` "▸ test_f · red: AssertionError: assert None == Ok(None)"
    hud (step' (Judge [m]) (step' (Drafted "x" "qwen" (Right ())) b0)) `shouldBe` "▸ test_f · draft 1 · 1 survivor"
    hud (step' (Judge []) (step' (Drafted "x" "qwen" (Right ())) b0)) `shouldBe` "▸ test_f · draft 1 · clean ✓ accept"
  it "(i) property: Accepted is reachable only through a clean Judged" $ hedgehog $ do
    moves <- forAll (Gen.list (Range.linear 0 12) genMove)
    let walk (b, lastJudgedClean) mv = case step mv b of
          Left _ -> (b, lastJudgedClean)
          Right b' -> (b', case mv of Judge ms -> all (isJust . muWaived) ms; Waive _ _ -> null (survivors b'); _ -> lastJudgedClean)
        (end, clean) = foldl walk (b0, False) moves
    if blockState end == Accepted then clean === True else success
