module Olifant.PreviewSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Generator (GeneratorDef, GeneratorId (..), parseGenerator)
import Olifant.Naming (Case (..), parseName, pluralize, render)
import Olifant.Preview
import Test.Hspec

-- | A generator with these input tables (TOML lines).
gen :: [Text] -> GeneratorDef
gen inputs = either (error . T.unpack) id (parseGenerator (GeneratorId "p/g") (T.unlines (["description = \"d\"", "language = \"x\""] ++ inputs)))

spec :: Spec
spec = describe "Olifant.Preview" $ do
  describe "fieldsFor" $ do
    let d =
          gen
            [ "[inputs.command]", "type = \"ident\""
            , "[inputs.service]", "type = \"enum\"", "choices = [\"booking\", \"notifications\"]"
            , "[inputs.app]", "type = \"enum\"", "choices = [\"bistro\"]"
            , "[inputs.kind]", "type = \"enum\"", "choices = [\"commands\", \"queries\"]", "default = \"commands\""
            , "[inputs.port]", "type = \"enum\"", "choices = [\"a\", \"b\"]"
            ]
    it "(a) the user's value wins, then the path's, then the cursor's, then a lone choice, then a default" $
      [(fName f, fValue f, fSource f) | f <- fieldsFor d [("command", "close_table")] [("service", "booking"), ("command", "from_path")] [("port", "b")]]
        `shouldBe` [ ("command", Just "close_table", Just FromYou)
                   , ("service", Just "booking", Just FromPath)
                   , ("app", Just "bistro", Just OnlyChoice)
                   , ("kind", Just "commands", Just FromDefault)
                   , ("port", Just "b", Just FromHere)
                   ]
    it "(b) a value is checked against its type, with the engine's own reason" $ do
      case fieldsFor d [("command", "3tables")] [] [] of
        f : _ -> do
          fValid f `shouldBe` False
          fWhy f `shouldSatisfy` maybe False ("3tables" `T.isInfixOf`)
        [] -> expectationFailure "no fields"
      let g = fieldsFor d [("port", "c")] [] [] !! 4
      (fValid g, fmap ("a, b" `T.isInfixOf`) (fWhy g)) `shouldBe` (False, Just True)
    it "(b2) a missing value is not valid and says nothing; an enum shows its choices" $ do
      case fieldsFor d [] [] [] of
        command : service : _ -> do
          (fValue command, fValid command, fWhy command) `shouldBe` (Nothing, False, Nothing)
          fChoices service `shouldBe` Just ["booking", "notifications"]
        _ -> expectationFailure "fewer than two fields"
  describe "marks" $ do
    let mark n = either (error . T.unpack) id (parseName (markValue n))
        placeholder c = case c of
          Constant -> "⟨COMMAND⟩"
          Pascal -> "⟨Command⟩"
          Title -> "⟨Command⟩"
          Sentence -> "⟨Command⟩"
          _ -> "⟨command⟩"
    it "(c) a missing name's mark comes back as ⟨name⟩ in every case, and plural (Review Focus 2)" $ do
      forM_ [minBound .. maxBound] $ \c ->
        unmark ["command"] ("x " <> render c (mark "command") <> " y") `shouldBe` ("x " <> placeholder c <> " y")
      unmark ["command"] (render Snake (pluralize (mark "command"))) `shouldBe` "⟨commands⟩"
      unmark ["web_resource"] (render Pascal (mark "web_resource")) `shouldBe` "⟨WebResource⟩"
      unmark ["command"] ("from " <> render Snake (mark "command") <> " import " <> render Pascal (mark "command") <> "Input") `shouldBe` "from ⟨command⟩ import ⟨Command⟩Input"
    it "(d) a real name that contains the mark's words is left alone (Review Focus 2)" $
      unmark ["command"] "class CommandBus:\n    zqmark = 1  # command\n" `shouldBe` "class CommandBus:\n    zqmark = 1  # command\n"
