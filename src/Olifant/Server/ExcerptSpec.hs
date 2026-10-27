module Olifant.Server.ExcerptSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Plans (Anchor (..), parsePlan)
import Olifant.Server.Edits (editScript, editScriptWithin, unifiedDiff)
import Olifant.Server.Excerpt
import Olifant.Server.LoopTypes (Fact (..))
import Olifant.Server.Unit (Named (..))
import Test.Hspec

-- | N lines "l1" … "lN", a unit over all of them defined on its first.
unitOf :: Int -> (Named, [Text])
unitOf n = (Named "f" (1, n) 1, ["l" <> T.pack (show i) | i <- [1 .. n]])

anchor :: Anchor
anchor = Anchor "a.py" "F" 0 7


spec :: Spec
spec = describe "Olifant.Server.Excerpt" $ do
  describe "clip" $ do
    it "a unit of 20 lines is whole" $ do
      let (u, ls) = unitOf 20
      clip u ls Nothing `shouldBe` ls
    it "a 100-line unit with line 50 changed: the header, a gap, 48–52, a gap" $ do
      let (u, ls) = unitOf 100
          base = [if i == 50 then "old" else l | (i, l) <- zip [1 :: Int ..] ls]
      clip u ls (Just base) `shouldBe` ["l1", "…", "l48", "l49", "l50", "l51", "l52", "…"]
    it "a header runs to the definition's line (decorators above it)" $ do
      let (_, ls) = unitOf 100
          base = [if i == 50 then "old" else l | (i, l) <- zip [1 :: Int ..] ls]
      take 3 (clip (Named "f" (1, 100) 3) ls (Just base)) `shouldBe` ["l1", "l2", "l3"]
    it "with no base, the first 30 lines and a gap" $ do
      let (u, ls) = unitOf 100
      clip u ls Nothing `shouldBe` take 30 ls ++ ["…"]
    it "an unchanged unit shows as with no base" $ do
      let (u, ls) = unitOf 100
      clip u ls (Just ls) `shouldBe` take 30 ls ++ ["…"]
    it "a removed line marks where it was" $ do
      let (u, ls) = unitOf 100
          base = take 49 ls ++ ["gone"] ++ drop 49 ls
      clip u ls (Just base) `shouldBe` ["l1", "…", "l48", "l49", "l50", "l51", "l52", "…"]
    it "is cut at 60 lines, a gap ending it" $ do
      let (u, ls) = unitOf 200
          base = [if even i then "old" else l | (i, l) <- zip [1 :: Int ..] ls]
          c = clip u ls (Just base)
      length c `shouldBe` 60
      last c `shouldBe` "…"
      take 3 c `shouldBe` ["l1", "l2", "l3"]
    it "a 3,000-line unit with one line changed: only the lines around it (the common ends are not diffed)" $ do
      let (u, ls) = unitOf 3000
          base = [if i == 1500 then "old" else l | (i, l) <- zip [1 :: Int ..] ls]
      clip u ls (Just base) `shouldBe` ["l1", "…", "l1498", "l1499", "l1500", "l1501", "l1502", "…"]
    it "past the diff's cap (a 600-line unit wholly rewritten): shown as with no base" $ do
      let (u, ls) = unitOf 600
      clip u ls (Just (map ("old " <>) ls)) `shouldBe` take 30 ls ++ ["…"]
    it "takes the unit's lines out of the file" $
      clip (Named "g" (3, 4) 3) ["a", "b", "c", "d", "e"] Nothing `shouldBe` ["c", "d"]
  describe "renderFence" $ do
    it "is one backtick longer than a run in the code, three at least" $ do
      take 1 (renderFence 2 "python" anchor False ["x = 1"]) `shouldBe` ["  ```python @a.py:F"]
      renderFence 2 "python" anchor False ["\"\"\"", "```", "\"\"\""] `shouldBe` ["  ````python @a.py:F", "  \"\"\"", "  ```", "  \"\"\"", "  ````"]
    it "a blank code line is the indent alone; frozen says done" $
      renderFence 4 "python" anchor True ["def f():", "", "    pass"] `shouldBe` ["    ```python @a.py:F · done", "    def f():", "    ", "        pass", "    ```"]
    it "a fence holding ``` reads back as the same excerpt" $ do
      let body = ["s = \"\"\"", "```", "\"\"\""]
          ls = parsePlan (T.unlines (["- [ ] s @a.py:F"] ++ renderFence 2 "python" anchor False body ++ ["- [ ] next"]))
      map exBody (excerptsOf ls) `shouldBe` [body]
      map exClose (excerptsOf ls) `shouldBe` [5]
  describe "excerptsOf" $ do
    it "reads a live and a frozen excerpt" $ do
      let ls = parsePlan (T.unlines ["- [ ] a @a.py:F", "  ```python @a.py:F", "  x", "  ```", "- [x] b @a.py:G", "  ```python @a.py:G · done", "  y", "  ```"])
      excerptsOf ls `shouldBe`
        [ Excerpt 0 1 3 "python" (Anchor "a.py" "F" 0 7) False ["x"]
        , Excerpt 4 5 7 "python" (Anchor "a.py" "G" 0 7) True ["y"] ]
    it "ignores the user's fence and one at the wrong indent" $ do
      let ls = parsePlan (T.unlines ["- [ ] a @a.py:F", "  ```python", "  x", "  ```", "    ```python @a.py:F", "    x", "    ```", "  ```python @a.py:F and more", "  ```"])
      excerptsOf ls `shouldBe` []
    it "ignores an unclosed fence" $
      excerptsOf (parsePlan "- [ ] a @a.py:F\n  ```python @a.py:F\n  x\n") `shouldBe` []
  describe "excerptFacts" $ do
    let live = ["  ```python @a.py:F", "  x = 1", "  ```"]
        frozen = ["  ```python @a.py:F · done", "  x = 1", "  ```"]
        facts step fence res = excerptFacts (parsePlan (T.unlines ([step] ++ fence ++ ["- [ ] next"]))) (const Nothing) res
    it "a ticked step's live excerpt is frozen with its final code, in one replacement" $
      facts "- [x] a @a.py:F" live (const (Just ("python", ["x = 2"]))) `shouldBe` [ReplaceRows 1 3 ["  ```python @a.py:F · done", "  x = 2", "  ```"]]
    it "a ticked step whose unit does not resolve: its opening line alone is frozen" $
      facts "- [x] a @a.py:F" live (const Nothing) `shouldBe` [Replace 1 "  ```python @a.py:F · done"]
    it "an open step's excerpt with the same code: no fact" $
      facts "- [ ] a @a.py:F" live (const (Just ("python", ["x = 1"]))) `shouldBe` []
    it "trailing spaces stripped by a formatter are no change" $
      facts "- [ ] a @a.py:F" ["  ```python @a.py:F", "  def f():", "", "      pass", "  ```"] (const (Just ("python", ["def f():", "", "    pass"]))) `shouldBe` []
    it "an open step's excerpt with new code: one replacement of its rows" $
      facts "- [ ] a @a.py:F" live (const (Just ("python", ["x = 2", "y = 3"]))) `shouldBe` [ReplaceRows 1 3 ["  ```python @a.py:F", "  x = 2", "  y = 3", "  ```"]]
    it "a frozen excerpt is never written, also when the step is open again" $ do
      facts "- [ ] a @a.py:F" frozen (const (Just ("python", ["x = 2"]))) `shouldBe` []
      facts "- [x] a @a.py:F" frozen (const (Just ("python", ["x = 2"]))) `shouldBe` []
    it "a unit that does not resolve leaves the excerpt" $
      facts "- [ ] a @a.py:F" live (const Nothing) `shouldBe` []
    it "a moved unit: the info string follows" $ do
      let to = Anchor "b.py" "H" 0 7
          ls = parsePlan (T.unlines (["- [ ] a @a.py:F"] ++ live))
          res a = if a == to then Just ("python", ["x = 1"]) else Nothing
      excerptFacts ls (\a -> if anUnit a == "F" then Just to else Nothing) res `shouldBe` [ReplaceRows 1 3 ["  ```python @b.py:H", "  x = 1", "  ```"]]
  describe "editScript" $ do
    it "keeps the common ends and diffs the middle" $
      editScript ["a", "b", "c", "d"] ["a", "B", "c", "d"] `shouldBe` [(' ', "a"), ('-', "b"), ('+', "B"), (' ', "c"), (' ', "d")]
    it "within a cap: Nothing when the middle's table is larger, the script otherwise" $ do
      editScriptWithin (Just 3) ["a", "x", "y", "z"] ["a", "p", "q"] `shouldBe` Nothing
      editScriptWithin (Just 6) ["a", "x", "y", "z"] ["a", "p", "q"] `shouldBe` Just (editScript ["a", "x", "y", "z"] ["a", "p", "q"])
  it "unifiedDiff keeps its output" $
    unifiedDiff "a.py" "a\nb\nc\n" "a\nB\nc\nd\n" `shouldBe` "--- a/a.py\n+++ b/a.py\n@@ -1,3 +1,4 @@\n a\n-b\n+B\n c\n+d\n"
