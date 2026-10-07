module Olifant.OutputSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Olifant.Actions (Action (..), FileAction (..), Range (..))
import Olifant.Doctor (Check (..), Status (..))
import Olifant.Output
import Olifant.Generator (GeneratorId (..), GeneratorWith (..), parseGenerator)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Output (what editors, agents and the language server read)" $ do
  describe "generatorKind / shortSummary (the generate picker's list)" $ do
    let load name = TIO.readFile ("packs/python-htmx/generators/" <> name <> "/generator.toml") >>= either (fail . T.unpack) pure . parseGenerator (GeneratorId ("python-htmx/" <> T.pack name))
    it "a generator that creates files is create; one that only inserts is extend; one with model holes is fill" $ do
      gens <- traverse load ["command", "add-dependency", "handle-body"]
      map generatorKind gens `shouldBe` ["create", "extend", "fill"]
    it "the summary is `summary`, else the description up to its first : ; or —" $ do
      d <- load "command"
      shortSummary d {genSummary = Nothing, genDescription = "A command of a service: commands/NAME.py (its Input)"} `shouldBe` "A command of a service"
      shortSummary d {genSummary = Nothing, genDescription = "Writes a body — with the model"} `shouldBe` "Writes a body"
      shortSummary d {genSummary = Just "Short"} `shouldBe` "Short"
  it "says a file's actions where each applies, and how an editor may offer it" $ do
    let json = fileActionsJson [] [FileAction (Action "g/wire" "Wire c" [("command", "c")] [] []) (Just (Range (2, 5) (2, 14)))] []
    actionsOf json `shouldBe` [object ["generator" .= ("g/wire" :: Text), "name" .= ("g/wire" :: Text), "label" .= ("Wire c" :: Text), "range" .= object ["start" .= [2 :: Int, 5], "end" .= [2 :: Int, 14]], "sets" .= object ["command" .= ("c" :: Text)], "missing" .= ([] :: [Text]), "suggest" .= False, "snippet" .= False, "trigger" .= False]]
  it "says doctor's checks with their status words and fixes" $
    doctorJson [Check "model" Warn "no [model]" (Just "add [model]")]
      `shouldBe` object ["checks" .= [object ["name" .= ("model" :: Text), "status" .= ("warn" :: Text), "detail" .= ("no [model]" :: Text), "fix" .= Just ("add [model]" :: Text)]]]
  where
    actionsOf = \case
      Object o | Just (Array as) <- KM.lookup (Key.fromText "actions") o -> foldr (:) [] as
      _ -> []
