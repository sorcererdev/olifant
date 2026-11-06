{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.ModelSpec (spec) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Olifant.Fill (FillHole (..), FillRequest (..), Stance (..))
import Olifant.Model
import Olifant.Toml (parseToml)
import Test.Hspec

request :: FillRequest
request = FillRequest [FillHole "status" "expr" "the HTTP status" "a.rs" "x => \10214status\10215," [("BAD", "a.rs: BAD is not defined")] False Nothing] [("failure", "too_late")] [] FillStance

spec :: Spec
spec = describe "Olifant.Model (spec §6.4)" $ do
  it "reads [model]: backends of both kinds, default and retries" $ do
    let Right top = parseToml (T.unlines
          [ "[model]", "default = \"local\"", "retries = 3"
          , "[model.backends.local]", "kind = \"openai-compatible\"", "url = \"http://localhost:11434/v1\"", "model = \"qwen3-coder:30b\""
          , "[model.backends.claude]", "kind = \"anthropic\"", "model = \"claude-haiku-4-5-20251001\"", "api_key_env = \"ANTHROPIC_API_KEY\"" ])
    parseModelConfig top `shouldBe` Right (defaultModelConfig {mcDefault = Just "local", mcRetries = 3, mcBackends = [("claude", Anthropic "claude-haiku-4-5-20251001" "ANTHROPIC_API_KEY"), ("local", OpenAICompatible "http://localhost:11434/v1" "qwen3-coder:30b")], mcLadder = [Rung "local" EscalateAsk], mcEscalate = [("claude", EscalateAsk), ("local", EscalateAsk)]})
  it "refuses an unknown backend kind, naming the kinds" $ do
    let Right top = parseToml "[model.backends.x]\nkind = \"gemini\"\nmodel = \"m\"\n"
    parseModelConfig top `shouldSatisfy` either ("openai-compatible, anthropic" `T.isInfixOf`) (const False)
  it "(a) reads a jev backend: model and key variable default; a stray key is refused" $ do
    let Right top = parseToml "[model.backends.jev]\nkind = \"jev\"\n"
        Right stray = parseToml "[model.backends.jev]\nkind = \"jev\"\nurl = \"http://x\"\n"
    parseModelConfig top `shouldBe` Right (defaultModelConfig {mcBackends = [("jev", Jev "jev-latest" "JEV_API_KEY")], mcEscalate = [("jev", EscalateAsk)]})
    parseModelConfig stray `shouldSatisfy` either ("url" `T.isInfixOf`) (const False)
  it "defaults to no backend and 2 retries without [model]" $
    parseModelConfig M.empty `shouldBe` Right defaultModelConfig
  it "asks an openai-compatible endpoint for tagged blocks, with the rules and the holes" $ do
    let body = TE.decodeUtf8 (BL.toStrict (encode (requestBody (OpenAICompatible "u" "m") request)))
    mapM_ (\k -> body `shouldSatisfy` T.isInfixOf k) ["\"model\":\"m\"", "\"role\":\"system\"", "<fill name=\\\"NAME\\\">", "status", "the HTTP status", "a.rs: BAD is not defined", "failure = too_late"]
  it "(c) a conjure request uses the conjures stance and lists every previous attempt" $ do
    let req = FillRequest [FillHole "body" "block" "i" "a.py" "ctx" [("return 1", "rejected by the person"), ("return 2", "the tests failed: x")] False Nothing] [] [] ConjureStance
        asText = TE.decodeUtf8 . BL.toStrict . encode
    mapM_ (\k -> requestBody (OpenAICompatible "u" "m") req `shouldSatisfy` \v -> k `T.isInfixOf` asText v)
      ["least code that makes this test pass", "return 1", "rejected by the person", "return 2", "attempt 2:", "Previous attempts (do not repeat them):"]
    requestBody (OpenAICompatible "u" "m") request `shouldSatisfy` \v -> not ("least code" `T.isInfixOf` asText v)
  it "a file request uses the file stance: whole files, no markdown" $ do
    let req = request {frStance = FileStance}
        asText = TE.decodeUtf8 . BL.toStrict . encode
    requestBody (OpenAICompatible "u" "m") req `shouldSatisfy` \v -> "configuration files olifant reads" `T.isInfixOf` asText v
    requestBody (OpenAICompatible "u" "m") request `shouldSatisfy` \v -> not ("configuration files olifant reads" `T.isInfixOf` asText v)
  it "a value request uses the value stance: one TOML value per hole, `none` when the sample cannot say" $ do
    let req = request {frStance = ValueStance}
        asText = TE.decodeUtf8 . BL.toStrict . encode
    requestBody (OpenAICompatible "u" "m") req `shouldSatisfy` \v -> all (`T.isInfixOf` asText v) ["one value per hole", "the right side of", "none"]
    requestBody (OpenAICompatible "u" "m") request `shouldSatisfy` \v -> not ("one value per hole" `T.isInfixOf` asText v)
  it "says a fill may begin with imports only where the language moves them, once, and on a retry" $ do
    let withImports = request {frHoles = [h {fhImports = True} | h <- frHoles request]}
        rule = "start the fill with the import line"
    userPrompt request `shouldNotSatisfy` T.isInfixOf rule
    length (T.breakOnAll rule (userPrompt withImports)) `shouldBe` 1
    -- the retry points at it (the previous fill failed)
    userPrompt withImports `shouldSatisfy` T.isInfixOf "If it failed on a name that is not defined, add that name's import line first."
    userPrompt request `shouldNotSatisfy` T.isInfixOf "add that name's import line first"
  it "shows related files (a fix's context, e.g. the port's trait) to the model" $ do
    let withContext = request {frContext = [("src/ports/invoice_repository.rs", "pub trait InvoiceRepository { async fn get(&self, id: Uuid) -> Option<Invoice>; }")]}
        body = TE.decodeUtf8 (BL.toStrict (encode (requestBody (OpenAICompatible "u" "m") withContext)))
    body `shouldSatisfy` T.isInfixOf "Related file src/ports/invoice_repository.rs"
    body `shouldSatisfy` T.isInfixOf "pub trait InvoiceRepository"
  it "asks Anthropic with a system prompt and max_tokens" $ do
    let body = TE.decodeUtf8 (BL.toStrict (encode (requestBody (Anthropic "claude-x" "K") request)))
    mapM_ (\k -> body `shouldSatisfy` T.isInfixOf k) ["\"model\":\"claude-x\"", "\"max_tokens\":", "\"system\":", "\"role\":\"user\""]
  it "reads fills from either reply shape, a ```json fence included (Review Focus 2)" $ do
    decodeFills (OpenAICompatible "u" "m") "{\"choices\":[{\"message\":{\"content\":\"{\\\"fills\\\": {\\\"status\\\": \\\"409\\\"}}\"}}]}" `shouldBe` Right (M.fromList [("status", "409")])
    decodeFills (Anthropic "c" "K") "{\"content\":[{\"type\":\"text\",\"text\":\"```json\\n{\\\"fills\\\": {\\\"status\\\": \\\"409\\\"}}\\n```\"}]}" `shouldBe` Right (M.fromList [("status", "409")])
  it "reads the shapes small models send: numbers, <think>, prose around JSON, fenced values (review I7)" $ do
    let oa content = "{\"choices\":[{\"message\":{\"content\":" <> content <> "}}]}"
    decodeFills (OpenAICompatible "u" "m") (oa "\"{\\\"fills\\\": {\\\"val\\\": 200, \\\"ok\\\": true}}\"") `shouldBe` Right (M.fromList [("ok", "true"), ("val", "200")])
    decodeFills (OpenAICompatible "u" "m") (oa "\"<think>hmm</think>\\n{\\\"fills\\\": {\\\"val\\\": \\\"1\\\"}}\"") `shouldBe` Right (M.fromList [("val", "1")])
    decodeFills (OpenAICompatible "u" "m") (oa "\"Here you go:\\n```json\\n{\\\"fills\\\": {\\\"val\\\": \\\"2\\\"}}\\n```\\nDone.\"") `shouldBe` Right (M.fromList [("val", "2")])
    decodeFills (OpenAICompatible "u" "m") (oa "\"{\\\"fills\\\": {\\\"val\\\": \\\"```rust\\\\n204\\\\n```\\\"}}\"") `shouldBe` Right (M.fromList [("val", "204")])
  it "reads a timeout, default 120 seconds (review I8)" $ do
    let Right top = parseToml "[model]\ntimeout = 30\n"
    fmap mcTimeout (parseModelConfig top) `shouldBe` Right 30
    fmap mcTimeout (parseModelConfig M.empty) `shouldBe` Right 120
  it "asks for tagged blocks, not JSON mode (v12)" $ do
    let body = TE.decodeUtf8 (BL.toStrict (encode (requestBody (OpenAICompatible "u" "m") request)))
    body `shouldSatisfy` T.isInfixOf "<fill name=\\\"status\\\">"
    body `shouldNotSatisfy` T.isInfixOf "response_format"
  it "reads tagged fills: raw code, quotes and braces untouched (v12)" $ do
    let reply content = BL.toStrict (encode (object ["choices" .= [object ["message" .= object ["content" .= (content :: T.Text)]]]]))
    decodeFills (OpenAICompatible "u" "m") (reply "<fill name=\"status\">\nformat!(\"{{\\\"id\\\": {}}}\", id)\n</fill>")
      `shouldBe` Right (M.fromList [("status", "format!(\"{{\\\"id\\\": {}}}\", id)")])
    decodeFills (OpenAICompatible "u" "m") (reply "Sure:\n<fill name=\"a\">1</fill>\n<fill name=\"b\">\nx + 1\n</fill>")
      `shouldBe` Right (M.fromList [("a", "1"), ("b", "x + 1")])
  it "says what was wrong with a reply that is not the agreed JSON" $ do
    decodeFills (OpenAICompatible "u" "m") "{\"choices\":[{\"message\":{\"content\":\"Sure! Here is the code.\"}}]}" `shouldSatisfy` either ("not the JSON" `T.isInfixOf`) (const False)
    decodeFills (OpenAICompatible "u" "m") "{\"error\":{\"message\":\"model not found\"}}" `shouldSatisfy` either ("model not found" `T.isInfixOf`) (const False)
  describe "the ladder (steered conjures)" $ do
    let backends = ["[model.backends.local]", "kind = \"openai-compatible\"", "url = \"http://localhost:11434/v1\"", "model = \"q\""]
        claude extra = ["[model.backends.claude]", "kind = \"anthropic\"", "model = \"c\""] <> extra
        parse ls = parseModelConfig (either (error . T.unpack) id (parseToml (T.unlines ls)))
        failsWith needle ls = parse ls `shouldSatisfy` either (needle `T.isInfixOf`) (const False)
    it "(a) reads the ladder in its order, with rounds, budget and each rung's escalate" $ do
      let Right c = parse (["[model]", "default = \"local\"", "ladder = [\"local\", \"claude\"]", "rounds = 4", "budget = 90"] <> backends <> claude ["escalate = \"auto\""])
      mcLadder c `shouldBe` [Rung "local" EscalateAsk, Rung "claude" EscalateAuto]
      mcEscalate c `shouldBe` [("claude", EscalateAuto), ("local", EscalateAsk)]
      (mcRounds c, mcBudget c) `shouldBe` (4, 90)
    it "(b) defaults: the default backend is the ladder, 6 rounds, 180 seconds" $ do
      let Right c = parse (["[model]", "default = \"local\""] <> backends)
      (mcLadder c, mcRounds c, mcBudget c) `shouldBe` ([Rung "local" EscalateAsk], 6, 180)
      fmap mcLadder (parse ["[model]"]) `shouldBe` Right []
      parseModelConfig M.empty `shouldBe` Right defaultModelConfig
    it "(c) refuses a ladder name with no backend" $
      failsWith "no such backend `gpt`" (["[model]", "ladder = [\"local\", \"gpt\"]"] <> backends)
    it "(d) refuses a jev backend in the ladder" $
      failsWith "answers decisions, not prompts" (["[model]", "ladder = [\"local\", \"jev\"]", "[model.backends.jev]", "kind = \"jev\""] <> backends)
    it "(e) refuses a bad escalate value" $
      failsWith "must be never, ask or auto" (backends <> ["escalate = \"sometimes\""])
    it "(f) refuses rounds or budget below 1" $ do
      failsWith "`rounds` must be at least 1" ["[model]", "rounds = 0"]
      failsWith "`budget` must be at least 1" ["[model]", "budget = 0"]
    it "(g) escalate is an unknown key on a jev backend" $
      failsWith "escalate" ["[model.backends.jev]", "kind = \"jev\"", "escalate = \"auto\""]
