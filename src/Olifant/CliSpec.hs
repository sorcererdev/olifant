module Olifant.CliSpec (spec) where

import Data.Aeson (Value (..), decodeStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Olifant.Cli
import Olifant.Workspace (memoryWorkspace)
import System.Exit (ExitCode (..))
import Test.Hspec

project :: [(FilePath, Text)]
project =
  [
    ( "/proj/.olifant/generators/model/generator.toml"
    , T.unlines
        [ "description = \"A model\""
        , "language = \"ruby\""
        , "[inputs.name]"
        , "type = \"ident\""
        , "[[edits]]"
        , "create = \"db/{= now|timestamp =}_create_{= name|plural|snake =}.rb\""
        , "text = \"# {= name|pascal =}\\n\""
        , "[[edits]]"
        , "create = \"app/models/{= name|snake =}.rb\""
        , "text = \"class {= name|pascal =}\\nend\\n\""
        ]
    )
  ]

fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2026 9 27) (secondsToDiffTime 55800)

runWith :: [(FilePath, Text)] -> Bool -> [String] -> IO (ExitCode, Text, Text, Map FilePath Text)
runWith files tty args = do
  (root, snapshot) <- memoryWorkspace files
  out <- newIORef ""
  err <- newIORef ""
  code <-
    runCli
      Runtime
        { rtRoot = root
        , rtProjectDir = "/proj"
        , rtUserConfigDir = Nothing
        , rtHomeDir = Nothing
        , rtNow = pure fixedNow
        , rtStdoutIsTty = tty
        , rtOut = \t -> modifyIORef' out (<> t)
        , rtErr = \t -> modifyIORef' err (<> t)
        }
      args
  (,,,) code <$> readIORef out <*> readIORef err <*> snapshot

jsonAt :: [Text] -> Text -> Maybe Value
jsonAt path out = decodeStrict (TE.encodeUtf8 out) >>= go path
  where
    go [] v = Just v
    go (k : ks) (Object o) = KM.lookup (Key.fromText k) o >>= go ks
    go _ _ = Nothing

spec :: Spec
spec = describe "runCli" $ do
  it "gen applies by default and prints a human summary on a TTY" $ do
    (code, out, _, files) <- runWith project True ["gen", "model", "--set", "name=blog_post"]
    code `shouldBe` ExitSuccess
    M.lookup "/proj/app/models/blog_post.rb" files `shouldBe` Just "class BlogPost\nend\n"
    out `shouldSatisfy` T.isInfixOf "+ app/models/blog_post.rb"
  it "uses the clock for `now`, and --now pins it" $ do
    (_, _, _, files1) <- runWith project True ["gen", "model", "--set", "name=post"]
    M.member "/proj/db/20260927153000_create_posts.rb" files1 `shouldBe` True
    (_, _, _, files2) <- runWith project True ["gen", "model", "--set", "name=post", "--now", "2030-01-02T03:04:05Z"]
    M.member "/proj/db/20300102030405_create_posts.rb" files2 `shouldBe` True
  it "--plan writes nothing and prints the plan as JSON when not on a TTY" $ do
    (code, out, _, files) <- runWith project False ["gen", "model", "--set", "name=post", "--plan"]
    code `shouldBe` ExitSuccess
    files `shouldBe` M.fromList project
    jsonAt ["applied"] out `shouldBe` Just (Bool False)
    jsonAt ["generator"] out `shouldBe` Just (String "model")
  it "lists every input problem as JSON with exit 4" $ do
    (code, out, _, _) <- runWith project False ["gen", "model", "--set", "colour=red"]
    code `shouldBe` ExitFailure 4
    jsonAt ["error", "code"] out `shouldBe` Just (String "invalid_inputs")
    jsonAt ["error", "unknown"] out `shouldBe` Just (Array (pure (String "colour")))
    fmap (T.isInfixOf "--set name=<ident>") (jsonAt ["error", "hint"] out >>= \case String s -> Just s; _ -> Nothing) `shouldBe` Just True
  it "exits 3 for an unknown generator" $ do
    (code, out, _, _) <- runWith project False ["gen", "nope"]
    code `shouldBe` ExitFailure 3
    jsonAt ["error", "code"] out `shouldBe` Just (String "generator_not_found")
  it "exits 5 on a conflict and writes nothing" $ do
    let existing = ("/proj/app/models/post.rb", "mine\n")
    (code, out, _, files) <- runWith (existing : project) False ["gen", "model", "--set", "name=post"]
    code `shouldBe` ExitFailure 5
    M.lookup "/proj/app/models/post.rb" files `shouldBe` Just "mine\n"
    M.size files `shouldBe` 2
    fmap (\case Array xs -> length xs; _ -> 0) (jsonAt ["conflicts"] out) `shouldBe` Just 1
  it "exits 3 with a pack_not_configured hint for pack/generator of an unknown pack" $ do
    (code, out, _, _) <- runWith project False ["gen", "elm/page"]
    code `shouldBe` ExitFailure 3
    jsonAt ["error", "code"] out `shouldBe` Just (String "pack_not_configured")
  it "exits 2 on bad usage" $ do
    (code, _, _, _) <- runWith project False ["gen"]
    code `shouldBe` ExitFailure 2
  it "lists generators as JSON" $ do
    (code, out, _, _) <- runWith project False ["list"]
    code `shouldBe` ExitSuccess
    out `shouldSatisfy` T.isInfixOf "\"id\":\"model\""
  it "shows a generator's inputs" $ do
    (_, out, _, _) <- runWith project False ["show", "model", "--json"]
    out `shouldSatisfy` T.isInfixOf "\"type\":\"ident\""
