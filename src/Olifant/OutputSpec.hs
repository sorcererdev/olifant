module Olifant.OutputSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Olifant.Actions (Action (..), FileAction (..), Range (..))
import Olifant.Doctor (Check (..), Status (..))
import Olifant.Output
import Test.Hspec

spec :: Spec
spec = describe "Olifant.Output (what editors, agents and the language server read)" $ do
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
