{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.DoctorSpec (spec) where

import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Doctor
import Olifant.Language (Language, parseLanguage)
import Olifant.Model (Backend (..), ModelConfig (..), defaultModelConfig)
import Test.Hspec

rust :: Language
rust = l
  where
    Right l = parseLanguage "rust" "/p" (T.unlines ["files = [\"**/*.rs\"]", "comment = \"//\"", "grammar = \"rust\"", "[check]", "command = [\"cargo\", \"check\"]", "format = \"rustc-json\""])

local :: ModelConfig
local = defaultModelConfig {mcDefault = Just "local", mcBackends = [("local", OpenAICompatible "http://localhost:11434/v1" "qwen3-coder:30b")]}

input :: ModelConfig -> DoctorInput
input m = DoctorInput (Just "/proj") ["rust-htmx"] [rust] [] [] m Nothing

-- Ports: these executables exist; the probe answers; these env vars are set.
ports :: [String] -> Either Text () -> [Text] -> DoctorPorts Identity
ports exes probe envs =
  DoctorPorts
    { dpFind = \e -> Identity (if e `elem` exes then Just ("/bin/" <> e) else Nothing)
    , dpPath = Identity "/usr/bin:/bin"
    , dpProbe = \_ -> Identity probe
    , dpEnvSet = \v -> Identity (v `elem` envs)
    }

statusOf :: Text -> [Check] -> Maybe Status
statusOf name cs = lookup name [(checkName c, checkStatus c) | c <- cs]

spec :: Spec
spec = describe "olifant doctor (spec §4.7)" $ do
  it "(a) all present: every check ok" $ do
    let cs = runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) (input local))
    map checkName cs `shouldBe` ["config", "pack rust-htmx", "language rust", "ast-grep", "check tool cargo (rust)", "model"]
    map checkStatus cs `shouldBe` replicate 6 Ok
  it "(b) a missing check tool is an error naming the PATH and where it usually lives" $ do
    let cs = runIdentity (doctor (ports ["ast-grep"] (Right ()) []) (input local))
        [c] = [c' | c' <- cs, checkName c' == "check tool cargo (rust)"]
    checkStatus c `shouldBe` Bad
    checkDetail c `shouldSatisfy` T.isInfixOf "/usr/bin:/bin"
    checkFix c `shouldSatisfy` maybe False (\f -> "/opt/homebrew/opt/rustup/bin" `T.isInfixOf` f && "~/.cargo/bin" `T.isInfixOf` f)
  it "(c) a model that does not answer is a warning with the reason, not an error (Review Focus 4)" $ do
    let cs = runIdentity (doctor (ports ["ast-grep", "cargo"] (Left "timed out after 3 s") []) (input local))
        [c] = [c' | c' <- cs, checkName c' == "model"]
    checkStatus c `shouldBe` Warn
    checkDetail c `shouldSatisfy` T.isInfixOf "timed out after 3 s"
  it "(d) no [model]: a warning with the snippet to add" $ do
    let cs = runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) (input defaultModelConfig))
        [c] = [c' | c' <- cs, checkName c' == "model"]
    checkStatus c `shouldBe` Warn
    checkFix c `shouldSatisfy` maybe False (\f -> "[model]" `T.isInfixOf` f && "default = \"local\"" `T.isInfixOf` f)
  it "(e) anthropic: never probed; only whether the key's variable is set" $ do
    let claude = defaultModelConfig {mcDefault = Just "claude", mcBackends = [("claude", Anthropic "claude-x" "MY_KEY")]}
        probeFails = ports ["ast-grep", "cargo"] (Left "must not be called") []
    statusOf "model" (runIdentity (doctor probeFails (input claude))) `shouldBe` Just Warn
    statusOf "model" (runIdentity (doctor (ports ["ast-grep", "cargo"] (Left "must not be called") ["MY_KEY"]) (input claude))) `shouldBe` Just Ok
  it "(c) a check run through `env VAR=… uv …` checks uv, with how to install it (Review Focus 3)" $ do
    let Right python = parseLanguage "python" "/p" (T.unlines ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[check]", "command = [\"env\", \"UV_PROJECT_ENVIRONMENT=/c/venv\", \"uv\", \"run\", \"pyright\"]", "format = \"pyright-json\""])
        cs = runIdentity (doctor (ports ["ast-grep", "env"] (Right ()) []) (DoctorInput (Just "/proj") ["python-htmx"] [python] [] [] local Nothing))
        [c] = [c' | c' <- cs, checkName c' == "check tool uv (python)"]
    checkStatus c `shouldBe` Bad
    checkFix c `shouldSatisfy` maybe False ("brew install uv" `T.isInfixOf`)
  it "(f) a language with [test] gets a test runner line: the program found, and where its tests live" $ do
    let Right python = parseLanguage "python" "/p" (T.unlines ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = \"python\"", "[test]", "preset = \"pytest\"", "tests_for = \"{= dir =}/{= stem =}__test.py\""])
        run exes = runIdentity (doctor (ports exes (Right ()) []) (DoctorInput (Just "/proj") ["python-htmx"] [python] [] [] local Nothing))
        [ok] = [c | c <- run ["ast-grep", "uv"], checkName c == "test runner uv (python)"]
        [bad] = [c | c <- run ["ast-grep"], checkName c == "test runner uv (python)"]
    checkStatus ok `shouldBe` Ok
    checkDetail ok `shouldSatisfy` T.isInfixOf "tests_for = {= dir =}/{= stem =}__test.py"
    checkStatus bad `shouldBe` Bad
    checkDetail bad `shouldSatisfy` (\d -> "uv" `T.isInfixOf` d && "/usr/bin:/bin" `T.isInfixOf` d)
  it "(g) a language without [test] gets no test runner line" $
    [checkName c | c <- runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) (input local)), "test runner" `T.isPrefixOf` checkName c] `shouldBe` []
  it "reports a broken language adapter or generator as an error" $ do
    let broken = DoctorInput (Just "/proj") ["p"] [] [("p/languages/x.toml", "bad grammar")] [("p/gen", "unknown key")]
        cs = runIdentity (doctor (ports ["ast-grep"] (Right ()) []) (broken local Nothing))
    statusOf "language p/languages/x.toml" cs `shouldBe` Just Bad
    statusOf "generator p/gen" cs `shouldBe` Just Bad
  it "(e) checks that the recognition model ([recognize] model) is a configured backend" $ do
    let run name = runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) ((input local) {diRecognizeModel = Just name}))
    statusOf "recognize model" (run "nope") `shouldBe` Just Bad
    statusOf "recognize model" (run "local") `shouldBe` Just Ok
    statusOf "recognize model" (runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) (input local))) `shouldBe` Nothing
  it "outside a project: config is an error" $
    statusOf "config" (runIdentity (doctor (ports ["ast-grep"] (Right ()) []) (DoctorInput Nothing [] [] [] [] local Nothing))) `shouldBe` Just Bad
