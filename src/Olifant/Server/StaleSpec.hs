{-# LANGUAGE OverloadedStrings #-}
module Olifant.Server.StaleSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import Control.Monad (void, when)
import Data.IORef (atomicModifyIORef', newIORef)
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (defaultPlans)
import Olifant.Server.Checker (CheckMode (..), CheckReply (..), Citation (..), Confirmed (..), Judgement (..), Print (..), confirm, fences)
import Olifant.Server.CheckerSpec (checkerServer)
import Olifant.Server.Claims (CheckRecord (..), SentenceEntry (..))
import Olifant.Server.Stale
import Olifant.Server.State (ServerState (..), filesChanged)
import Olifant.Server.Triage (Route (..), Triage (..))
import Olifant.Workspace (Workspace (..))
import Test.Hspec

at0 :: UTCTime
at0 = UTCTime (fromGregorian 2026 11 11) 0

foo :: Text
foo = "class Foo:\n    def bar(self):\n        return 1\n"

checkOf :: ServerState -> Project -> Judgement -> [Citation] -> IO CheckRecord
checkOf s proj j cs = do
  c <- confirm s proj (fences defaultPlans) (CheckReply j "w" cs)
  pure (CheckRecord c "sonnet" Nothing 1 at0 CheckClaim)

-- | The file rewritten on disk, as a watched-file change reports it.
rewrite :: ServerState -> FilePath -> Text -> IO ServerState
rewrite s path t = do
  writeText (pFiles (ssPorts s)) path t
  filesChanged [path] s

staleNow :: ServerState -> Project -> CheckRecord -> IO Bool
staleNow s proj c = do
  now <- printsNow s proj (map ptPath (cfPrints (cjConfirmed c)))
  pure (isStale now c)

spec :: Spec
spec = describe "Olifant.Server.Stale" $ do
  it "an unchanged unit is not stale; a changed unit body is" $ do
    (s, proj) <- checkerServer
    c <- checkOf s proj JRight [Citation "src/a.py" (Just "Foo.bar") Nothing]
    staleNow s proj c `shouldReturn` False
    s1 <- rewrite s "/r1/src/a.py" "class Foo:\n    def bar(self):\n        return 2\n"
    staleNow s1 proj c `shouldReturn` True
  it "a gone unit or file is stale" $ do
    (s, proj) <- checkerServer
    c <- checkOf s proj JRight [Citation "src/a.py" (Just "Foo") Nothing]
    -- the unit renamed
    let renamed = c {cjConfirmed = (cjConfirmed c) {cfPrints = [Print "src/a.py" (Just "Gone") "abc1234"]}}
    staleNow s proj renamed `shouldReturn` True
    s1 <- rewrite s "/r1/src/a.py" foo
    staleNow s1 proj c `shouldReturn` False
    removeFile (pFiles (ssPorts s1)) "/r1/src/a.py"
    s2 <- filesChanged ["/r1/src/a.py"] s1
    staleNow s2 proj c `shouldReturn` True
  it "a file-hash print goes stale on any edit to the file" $ do
    (s, proj) <- checkerServer
    c <- checkOf s proj JRight [Citation "notes.txt" Nothing Nothing]
    staleNow s proj c `shouldReturn` False
    s1 <- rewrite s "/r1/notes.txt" "x \n"
    staleNow s1 proj c `shouldReturn` True
  it "a check with no prints, or one that could not tell, is never stale" $ do
    (s, proj) <- checkerServer
    c <- checkOf s proj JRight [Citation "src/a.py" (Just "Foo") Nothing]
    let bare = c {cjConfirmed = (cjConfirmed c) {cfPrints = []}}
        unsure = c {cjConfirmed = (cjConfirmed c) {cfJudgement = JCannotTell}}
    s1 <- rewrite s "/r1/src/a.py" "class Foo:\n    def bar(self):\n        return 3\n"
    staleNow s1 proj bare `shouldReturn` False
    staleNow s1 proj unsure `shouldReturn` False
  it "staleKeys names the stale entries; a file read once is not read again until it changes" $ do
    (s, proj) <- checkerServer
    c <- checkOf s proj JRight [Citation "src/a.py" (Just "Foo") Nothing]
    let entry = SentenceEntry (Triage "claim" 0.9 0.9 "jev") Verify (Just c) Nothing at0 Nothing False
        store = M.fromList [("a", entry), ("b", entry {seCheck = Nothing})]
    now0 <- printsNow s proj ["src/a.py"]
    staleKeys now0 store `shouldBe` S.empty
    -- written behind the cache's back: still the old reading
    writeText (pFiles (ssPorts s)) "/r1/src/a.py" "class Foo:\n    pass\n"
    now1 <- printsNow s proj ["src/a.py"]
    now1 `shouldBe` now0
    s1 <- filesChanged ["/r1/src/a.py"] s
    now2 <- printsNow s1 proj ["src/a.py"]
    staleKeys now2 store `shouldBe` S.fromList ["a"]
  it "a file changed on disk while it is being read is read again: the old print is not kept" $ do
    (s, proj) <- checkerServer
    c <- checkOf s proj JRight [Citation "src/a.py" (Just "Foo.bar") Nothing]
    fired <- newIORef False
    -- the change lands (and 'filesChanged' runs) after the read, before the cache insert
    let ws = prWorkspace proj
        racing = ws {readText = \rel -> do
          t <- readText ws rel
          first <- atomicModifyIORef' fired (\f -> (True, not f))
          when first (void (rewrite s "/r1/src/a.py" "class Foo:\n    def bar(self):\n        return 2\n"))
          pure t}
    staleNow s proj {prWorkspace = racing} c `shouldReturn` False
    staleNow s proj c `shouldReturn` True
