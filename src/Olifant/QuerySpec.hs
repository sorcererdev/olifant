{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Olifant.QuerySpec (spec) where

import Data.Functor.Identity (Identity (..))
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Olifant.Query
import Olifant.Template (Value (..))
import Olifant.Toml (Tree (..))
import System.Directory (createDirectoryIfMissing, getPermissions, setOwnerExecutable, setPermissions)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

paths :: [FilePath]
paths = ["src/Olifant/Anchor/Phrase.hs", "src/Olifant/Anchor.hs", "src/Olifant/AnchorSpec.hs", "src/TestDriver.hs", "app/Main.hs", "dist-newstyle/x/src/Y.hs"]

spec :: Spec
spec = do
  describe "choiceValue (spec §4.1)" $
    it "reads a name field (a glob record's `name`) as its text" $
      fmap (\r -> choiceValue (FieldValue "name") r) (take 1 (globRecords "src/*.rs" [] ["src/send_email.rs"])) `shouldBe` [Right "send_email"]
  describe "globRecords" $ do
    it "gives each match path, dir, stem, rel and dotted, sorted, skipping build dirs" $
      map (\r -> (M.lookup "path" r, M.lookup "rel" r, M.lookup "dotted" r)) (globRecords "src/**/*.hs" ["**/*Spec.hs", "src/TestDriver.hs"] paths)
        `shouldBe` [ (Just (VText "src/Olifant/Anchor.hs"), Just (VText "Olifant/Anchor"), Just (VText "Olifant.Anchor"))
                   , (Just (VText "src/Olifant/Anchor/Phrase.hs"), Just (VText "Olifant/Anchor/Phrase"), Just (VText "Olifant.Anchor.Phrase"))
                   ]
    it "matches directories as well as files" $
      map (M.lookup "path") (globRecords "*/*-service" [] ["billing/billing-service/Cargo.toml", "billing/billing-service/src/lib.rs"])
        `shouldBe` [Just (VText "billing/billing-service")]
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
    let fake out = noQueries {queryCommand = \_ -> Identity out}
    it "turns a JSON array of objects into records, sorted by `sort`" $
      runIdentity (runQuery (fake (Right "[{\"name\":\"b\",\"n\":2},{\"name\":\"a\",\"n\":1}]")) [] (const (pure Nothing)) (CommandQuery ["x"] Nothing (Just "name")))
        `shouldBe` Right [M.fromList [("name", VText "a"), ("n", VInt 1)], M.fromList [("name", VText "b"), ("n", VInt 2)]]
    it "selects an array out of an object" $
      fmap length (runIdentity (runQuery (fake (Right "{\"packages\":[{\"name\":\"a\"}]}")) [] (const (pure Nothing)) (CommandQuery ["x"] (Just "packages") Nothing))) `shouldBe` Right 1
    it "reports invalid JSON and command failures, naming the command (Review Focus 4)" $ do
      runIdentity (runQuery (fake (Right "not json")) [] (const (pure Nothing)) (CommandQuery ["x"] Nothing Nothing)) `shouldSatisfy` either (("command x" `T.isInfixOf`) . failureWhy) (const False)
      runIdentity (runQuery (fake (Left (failed "boom"))) [] (const (pure Nothing)) (CommandQuery ["x"] Nothing Nothing)) `shouldSatisfy` either (("boom" `T.isInfixOf`) . failureWhy) (const False)
  describe "queryFromTree (ast)" $ do
    it "reads items_of with each fields" $
      queryFromTree (TTable (M.fromList [("files", TText "src/*.rs"), ("items_of", TText "struct Foo"), ("each", TTable (M.fromList [("name", TText "^pub ([a-z_]+):")]))]))
        `shouldBe` Right (AstQueryQ (AstQuery "src/*.rs" (ItemsOf "struct Foo") [("name", "^pub ([a-z_]+):")]))
    it "rejects an invalid each regex, naming the field (Review Focus 2)" $
      queryFromTree (TTable (M.fromList [("files", TText "a"), ("items_of", TText "type X"), ("each", TTable (M.fromList [("bad", TText "(")]))]))
        `shouldSatisfy` either ("bad" `T.isInfixOf`) (const False)
    it "rejects Perl classes such as \\s and \\w, which POSIX reads as literals" $
      queryFromTree (TTable (M.fromList [("files", TText "a"), ("items_of", TText "type X"), ("each", TTable (M.fromList [("f", TText "^\\(\\s*(\\w+)")]))]))
        `shouldSatisfy` either ("each.f" `T.isInfixOf`) (const False)
    it "accepts an escaped backslash before s" $
      queryFromTree (TTable (M.fromList [("files", TText "a"), ("items_of", TText "type X"), ("each", TTable (M.fromList [("f", TText "a\\\\s")]))]))
        `shouldSatisfy` either (const False) (const True)
  describe "eachFields (properties)" $ do
    it "gives every field, whatever the text, and never drops one" $ hedgehog $ do
      t <- forAll (Gen.text (Range.linear 0 40) Gen.unicode)
      M.keys (eachFields [("a", "^([a-z]+)"), ("b", "zzz"), ("c", "[0-9]")] t) === ["a", "b", "c"]
    it "captures a word it can see" $ hedgehog $ do
      w <- forAll (Gen.text (Range.linear 1 8) Gen.lower)
      pre <- forAll (Gen.text (Range.linear 0 8) (Gen.element (" ,;:" :: String)))
      M.lookup "w" (eachFields [("w", "([a-z]+)")] (pre <> w <> " 1")) === Just (VText w)
  describe "eachFields" $
    it "takes the first capture group, the whole match without one, empty without a match" $
      eachFields [("name", "^pub ([a-z_]+):"), ("kw", "pub"), ("none", "zzz")] "pub repo: Repo,"
        `shouldBe` M.fromList [("name", VText "repo"), ("kw", VText "pub"), ("none", VText "")]
  describe "renderQuery" $
    it "renders templates in a query's text" $
      fmap describeQuery (renderQuery (M.fromList [("Cmd", VText "Refund")]) (AstQueryQ (AstQuery "src/{= Cmd|snake =}.rs" (ItemsOf "struct {= Cmd =}") [])))
        `shouldBe` Right "ast src/refund.rs (items of struct Refund)"
  describe "processCommands" $ do
    it "runs a project script given by a relative path (review #5)" $
      withSystemTempDirectory "olifant-cmd" $ \dir -> do
        createDirectoryIfMissing True (dir </> "bin")
        writeFile (dir </> "bin" </> "list") "#!/bin/sh\necho '[{\"name\":\"a\"}]'\n"
        p <- getPermissions (dir </> "bin" </> "list")
        setPermissions (dir </> "bin" </> "list") (setOwnerExecutable True p)
        r <- runQuery (noQueries {queryCommand = processCommand dir}) [] (const (pure Nothing)) (CommandQuery ["bin/list"] Nothing Nothing)
        r `shouldBe` Right [M.fromList [("name", VText "a")]]
        r2 <- runQuery (noQueries {queryCommand = processCommand dir}) [] (const (pure Nothing)) (CommandQuery ["./bin/list"] Nothing Nothing)
        r2 `shouldBe` r
    it "runs a real command in the given directory" $ do
      r <- runQuery (noQueries {queryCommand = processCommand "."}) [] (const (pure Nothing)) (CommandQuery ["echo", "[{\"k\":\"v\"}]"] Nothing Nothing)
      r `shouldBe` Right [M.fromList [("k", VText "v")]]
