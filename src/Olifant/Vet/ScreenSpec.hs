module Olifant.Vet.ScreenSpec (spec) where

import Data.Maybe (isJust)
import Olifant.Decide (Question (..))
import Olifant.Language (shippedLanguageNow)
import Olifant.Language.TreeSitter (loadableGrammar)
import Olifant.Server.Unit (Named (..))
import Olifant.Vet.Screen
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Vet.Screen" $ do
  let py = shippedLanguageNow "python"
      grammar = isJust (loadableGrammar py)
  it "a docstring-only body is empty" $
    if not grammar then pendingWith "no python grammar" else
      unitFacts py "def f():\n    \"\"\"Does f.\"\"\"\n" (Named "f" (1, 2) 1) `shouldBe` Facts False False True
  it "a comment-only body is empty" $
    if not grammar then pendingWith "no python grammar" else
      unitFacts py "def f():\n    # later\n    \"\"\"Doc.\"\"\"\n" (Named "f" (1, 3) 1) `shouldBe` Facts False False True
  it "pass is no fact at all (no placeholder list)" $
    if not grammar then pendingWith "no python grammar" else
      unitFacts py "def f():\n    pass\n" (Named "f" (1, 2) 1) `shouldBe` Facts False False False
  it "a TODO word is a fact; todo_list is not" $ do
    fTodo (unitFacts py "def f():\n    # TODO: finish\n    return 1\n" (Named "f" (1, 3) 1)) `shouldBe` True
    fTodo (unitFacts py "def f():\n    return todo_list\n" (Named "f" (1, 2) 1)) `shouldBe` False
    fTodo (unitFacts py "def f():\n    return 1  # FIXME\n" (Named "f" (1, 2) 1)) `shouldBe` True
  it "a broken line is an error fact; one outside the unit is not" $
    if not grammar then pendingWith "no python grammar" else do
      fErrors (unitFacts py "def f():\n    return (1,\n\ndef g():\n    return 2\n" (Named "f" (1, 2) 1)) `shouldBe` True
      fErrors (unitFacts py "def f():\n    return 1\n\n\nx = (1,\n" (Named "f" (1, 2) 1)) `shouldBe` False
  it "unfinished: any fact, or stops-early at 0.80" $ do
    unfinished (Facts False False False) 0.79 `shouldBe` False
    unfinished (Facts False False False) 0.80 `shouldBe` True
    unfinished (Facts False True False) 0 `shouldBe` True
  it "the stops-early question is the 1a wording" $ do
    let q = stopsEarly "python" "src/a.py" "def f():\n    pass" "class A:" (Facts False False True)
    qKind q `shouldBe` "stops-early"
    qInstructions q `shouldBe` "Does the body of this function stop before doing what its name, signature and documentation promise?"
    map fst (qContext q) `shouldBe` ["language", "file", "function text", "enclosing class or module", "code facts"]
    lookup "code facts" (qContext q) `shouldBe` Just "syntax errors: no; TODO or FIXME: no; placeholder body: yes"
