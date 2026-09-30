{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Olifant.QuerySpec (spec) where

import Data.Functor.Identity (Identity (..))
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Olifant.Query
import Olifant.Template (Value (..))
import Olifant.Toml (Tree (..))
import Test.Hspec

paths :: [FilePath]
paths = ["src/Olifant/Anchor/Phrase.hs", "src/Olifant/Anchor.hs", "src/Olifant/AnchorSpec.hs", "src/TestDriver.hs", "app/Main.hs", "dist-newstyle/x/src/Y.hs"]

spec :: Spec
spec = do
  describe "globRecords" $ do
    it "gives each match path, dir, stem, rel and dotted, sorted, skipping build dirs" $
      map (\r -> (M.lookup "path" r, M.lookup "rel" r, M.lookup "dotted" r)) (globRecords "src/**/*.hs" ["**/*Spec.hs", "src/TestDriver.hs"] paths)
        `shouldBe` [ (Just (VText "src/Olifant/Anchor.hs"), Just (VText "Olifant/Anchor"), Just (VText "Olifant.Anchor"))
                   , (Just (VText "src/Olifant/Anchor/Phrase.hs"), Just (VText "Olifant/Anchor/Phrase"), Just (VText "Olifant.Anchor.Phrase"))
                   ]
    it "gives dir and stem" $
      map (\r -> (M.lookup "dir" r, M.lookup "stem" r)) (globRecords "*/*-service" [] ["billing/billing-service"])
        `shouldBe` [(Just (VText "billing"), Just (VText "billing-service"))]
  describe "queryFromTree" $ do
    it "reads a glob with excludes and a command with select and sort" $ do
      queryFromTree (TTable (M.fromList [("glob", TText "src/**/*.hs"), ("exclude", TList [TText "**/*Spec.hs"])])) `shouldBe` Right (GlobQuery "src/**/*.hs" ["**/*Spec.hs"])
      queryFromTree (TTable (M.fromList [("command", TList [TText "cargo", TText "metadata"]), ("select", TText "packages"), ("sort", TText "name")])) `shouldBe` Right (CommandQuery ["cargo", "metadata"] (Just "packages") (Just "name"))
    it "needs exactly one of glob or command" $
      queryFromTree (TTable M.empty) `shouldSatisfy` either ("glob" `T.isInfixOf`) (const False)
  describe "runQuery (commands)" $ do
    let fake out = Commands (\_ -> Identity out)
    it "turns a JSON array of objects into records, sorted by `sort`" $
      runIdentity (runQuery (fake (Right "[{\"name\":\"b\",\"n\":2},{\"name\":\"a\",\"n\":1}]")) [] (CommandQuery ["x"] Nothing (Just "name")))
        `shouldBe` Right [M.fromList [("name", VText "a"), ("n", VInt 1)], M.fromList [("name", VText "b"), ("n", VInt 2)]]
    it "selects an array out of an object" $
      fmap length (runIdentity (runQuery (fake (Right "{\"packages\":[{\"name\":\"a\"}]}")) [] (CommandQuery ["x"] (Just "packages") Nothing))) `shouldBe` Right 1
    it "reports invalid JSON and command failures, naming the command (Review Focus 4)" $ do
      runIdentity (runQuery (fake (Right "not json")) [] (CommandQuery ["x"] Nothing Nothing)) `shouldSatisfy` either ("command x" `T.isInfixOf`) (const False)
      runIdentity (runQuery (fake (Left "boom")) [] (CommandQuery ["x"] Nothing Nothing)) `shouldSatisfy` either ("boom" `T.isInfixOf`) (const False)
  describe "processCommands" $
    it "runs a real command in the given directory" $ do
      r <- runQuery (processCommands ".") [] (CommandQuery ["echo", "[{\"k\":\"v\"}]"] Nothing Nothing)
      r `shouldBe` Right [M.fromList [("k", VText "v")]]
