{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.ModelSpec (spec) where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Olifant.Fill (FillHole (..), FillRequest (..))
import Olifant.Model
import Olifant.Toml (parseToml)
import Test.Hspec

request :: FillRequest
request = FillRequest [FillHole "status" "expr" "the HTTP status" "a.rs" "x => \10214status\10215," (Just ("BAD", "a.rs: BAD is not defined"))] [("failure", "too_late")]

spec :: Spec
spec = describe "Olifant.Model (spec §6.4)" $ do
  it "reads [model]: backends of both kinds, default and retries" $ do
    let Right top = parseToml (T.unlines
          [ "[model]", "default = \"local\"", "retries = 3"
          , "[model.backends.local]", "kind = \"openai-compatible\"", "url = \"http://localhost:11434/v1\"", "model = \"qwen3-coder:30b\""
          , "[model.backends.claude]", "kind = \"anthropic\"", "model = \"claude-haiku-4-5-20251001\"", "api_key_env = \"ANTHROPIC_API_KEY\"" ])
    parseModelConfig top `shouldBe` Right (ModelConfig (Just "local") 3 [("claude", Anthropic "claude-haiku-4-5-20251001" "ANTHROPIC_API_KEY"), ("local", OpenAICompatible "http://localhost:11434/v1" "qwen3-coder:30b")])
  it "refuses an unknown backend kind, naming the kinds" $ do
    let Right top = parseToml "[model.backends.x]\nkind = \"gemini\"\nmodel = \"m\"\n"
    parseModelConfig top `shouldSatisfy` either ("openai-compatible, anthropic" `T.isInfixOf`) (const False)
  it "defaults to no backend and 2 retries without [model]" $
    parseModelConfig M.empty `shouldBe` Right (ModelConfig Nothing 2 [])
  it "asks an openai-compatible endpoint for a JSON object, with the rules and the holes" $ do
    let body = TE.decodeUtf8 (BL.toStrict (encode (requestBody (OpenAICompatible "u" "m") request)))
    mapM_ (\k -> body `shouldSatisfy` T.isInfixOf k) ["\"model\":\"m\"", "\"response_format\":{\"type\":\"json_object\"}", "\"role\":\"system\"", "{\\\"fills\\\": {", "status", "the HTTP status", "a.rs: BAD is not defined", "failure = too_late"]
  it "asks Anthropic with a system prompt and max_tokens" $ do
    let body = TE.decodeUtf8 (BL.toStrict (encode (requestBody (Anthropic "claude-x" "K") request)))
    mapM_ (\k -> body `shouldSatisfy` T.isInfixOf k) ["\"model\":\"claude-x\"", "\"max_tokens\":", "\"system\":", "\"role\":\"user\""]
  it "reads fills from either reply shape, a ```json fence included (Review Focus 2)" $ do
    decodeFills (OpenAICompatible "u" "m") "{\"choices\":[{\"message\":{\"content\":\"{\\\"fills\\\": {\\\"status\\\": \\\"409\\\"}}\"}}]}" `shouldBe` Right (M.fromList [("status", "409")])
    decodeFills (Anthropic "c" "K") "{\"content\":[{\"type\":\"text\",\"text\":\"```json\\n{\\\"fills\\\": {\\\"status\\\": \\\"409\\\"}}\\n```\"}]}" `shouldBe` Right (M.fromList [("status", "409")])
  it "says what was wrong with a reply that is not the agreed JSON" $ do
    decodeFills (OpenAICompatible "u" "m") "{\"choices\":[{\"message\":{\"content\":\"Sure! Here is the code.\"}}]}" `shouldSatisfy` either ("not the JSON" `T.isInfixOf`) (const False)
    decodeFills (OpenAICompatible "u" "m") "{\"error\":{\"message\":\"model not found\"}}" `shouldSatisfy` either ("model not found" `T.isInfixOf`) (const False)
