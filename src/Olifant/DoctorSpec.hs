{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.DoctorSpec (spec) where

import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Doctor
import Olifant.Evidence (Evidence (..), Proof (..))
import Olifant.Language (Grammar (..), Language (..), parseLanguage, shippedLanguage)
import Olifant.Language.Grammar (GrammarFailure (..), GrammarInfo (..))
import Olifant.Catalog (PlansConfig (..), defaultPlans)
import Olifant.Model (Backend (..), ModelConfig (..), defaultModelConfig)
import Test.Hspec

rust :: Language
rust = l
  where
    Right l = parseLanguage "rust" "/p" (T.unlines ["files = [\"**/*.rs\"]", "comment = \"//\"", "grammar = { git = \"https://github.com/tree-sitter/tree-sitter-rust\", rev = \"77a3747266f4d621d0757825e6b11edcbf991ca5\" }", "[check]", "preset = \"cargo\""])

local :: ModelConfig
local = defaultModelConfig {mcDefault = Just "local", mcBackends = [("local", OpenAICompatible "http://localhost:11434/v1" "qwen3-coder:30b")]}

input :: ModelConfig -> DoctorInput
input m = DoctorInput (Just "/proj") ["rust-htmx"] [rust] [] [] m Nothing defaultPlans [] False

-- | The checks but the capability rows (`<lang>: …`) and the plans runner's.
core :: [Check] -> [Check]
core = filter (\c -> not (": " `T.isInfixOf` checkName c) && not ("plans runner" `T.isPrefixOf` checkName c))

-- Ports: these executables exist; the probe answers; these env vars are set.
ports :: [String] -> Either Text () -> [Text] -> DoctorPorts Identity
ports exes probe envs =
  DoctorPorts
    { dpFind = \e -> Identity (if e `elem` exes then Just ("/bin/" <> e) else Nothing)
    , dpPath = Identity "/usr/bin:/bin"
    , dpProbe = \_ -> Identity probe
    , dpEnvSet = \v -> Identity (v `elem` envs)
    , dpRead = \f -> Identity (if f `elem` ["/proj/uv.lock"] then Just "" else Nothing)
    , dpList = \_ -> Identity []
    , dpGrammar = \_ -> Identity (Left NoSource)
    }

statusOf :: Text -> [Check] -> Maybe Status
statusOf name cs = lookup name [(checkName c, checkStatus c) | c <- cs]

spec :: Spec
spec = describe "olifant doctor (spec §4.7)" $ do
  it "(a) all present: every check ok" $ do
    let cs = core (runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) (input local)))
    map checkName cs `shouldBe` ["config", "pack rust-htmx", "language rust", "ast-grep", "check tool cargo (rust)", "model"]
    map checkStatus cs `shouldBe` replicate 6 Ok
  it "(b) a missing check tool is an error naming the PATH and where it usually lives" $ do
    let cs = runIdentity (doctor (ports ["ast-grep"] (Right ()) []) (input local))
        [c] = [c' | c' <- cs, checkName c' == "check tool cargo (rust)"]
    checkStatus c `shouldBe` Bad
    checkDetail c `shouldSatisfy` T.isInfixOf "/usr/bin:/bin"
    checkFix c `shouldSatisfy` maybe False (\f -> "/opt/homebrew/opt/rustup/bin" `T.isInfixOf` f && "~/.cargo/bin" `T.isInfixOf` f)
  it "(b2) a check tool with no install hint gets the generic one" $ do
    let Right bare = parseLanguage "x" "/p" (T.unlines ["files = [\"**/*.x\"]", "comment = \"#\"", "grammar = { git = \"https://github.com/tree-sitter/tree-sitter-rust\", rev = \"77a3747266f4d621d0757825e6b11edcbf991ca5\" }", "[check]", "command = [\"xc\"]", "format = \"sarif\""])
        cs = runIdentity (doctor (ports ["ast-grep"] (Right ()) []) ((input local) {diLanguages = [bare]}))
        [c] = [c' | c' <- cs, checkName c' == "check tool xc (x)"]
    checkFix c `shouldBe` Just "install xc and make sure it is on the PATH your editor starts with"
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
    let Right python = parseLanguage "python" "/p" (T.unlines ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = { git = \"https://github.com/tree-sitter/tree-sitter-python\", rev = \"293fdc02038ee2bf0e2e206711b69c90ac0d413f\" }", "[check]", "preset = \"pyright\""])
        cs = runIdentity (doctor (ports ["ast-grep", "env"] (Right ()) []) (DoctorInput (Just "/proj") ["python-htmx"] [python] [] [] local Nothing defaultPlans [] False))
        [c] = [c' | c' <- cs, checkName c' == "check tool uv (python)"]
    checkStatus c `shouldBe` Bad
    checkFix c `shouldSatisfy` maybe False ("brew install uv" `T.isInfixOf`)
  it "(f) a language with [test] gets a test runner line: the program found, and where its tests live" $ do
    let Right python = parseLanguage "python" "/p" (T.unlines ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = { git = \"https://github.com/tree-sitter/tree-sitter-python\", rev = \"293fdc02038ee2bf0e2e206711b69c90ac0d413f\" }", "[test]", "preset = \"pytest\"", "tests_for = \"{= dir =}/{= stem =}__test.py\""])
        run exes = runIdentity (doctor (ports exes (Right ()) []) (DoctorInput (Just "/proj") ["python-htmx"] [python] [] [] local Nothing defaultPlans [] False))
        [ok] = [c | c <- run ["ast-grep", "uv"], checkName c == "test runner uv (python)"]
        [bad] = [c | c <- run ["ast-grep"], checkName c == "test runner uv (python)"]
    checkStatus ok `shouldBe` Ok
    checkDetail ok `shouldSatisfy` T.isInfixOf "tests_for = {= dir =}/{= stem =}__test.py"
    checkDetail ok `shouldSatisfy` T.isInfixOf "uv.lock found"
    checkStatus bad `shouldBe` Bad
    checkDetail bad `shouldSatisfy` (\d -> "uv" `T.isInfixOf` d && "/usr/bin:/bin" `T.isInfixOf` d)
  it "(f2) the test runner row names the command candidate chosen, and why" $ do
    let Right python = parseLanguage "python" "/p" (T.unlines ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = { git = \"https://github.com/tree-sitter/tree-sitter-python\", rev = \"293fdc02038ee2bf0e2e206711b69c90ac0d413f\" }", "[test]", "preset = \"pytest\"", "tests_for = \"{= dir =}/{= stem =}__test.py\""])
        withFiles fs = (ports ["ast-grep", "pytest", "/proj/.venv/bin/pytest"] (Right ()) []) {dpRead = \f -> Identity (if f `elem` fs then Just "" else Nothing)}
        rows fs = [(checkName c, checkStatus c, checkDetail c) | c <- runIdentity (doctor (withFiles fs) (DoctorInput (Just "/proj") [] [python] [] [] local Nothing defaultPlans [] False)), "test runner" `T.isPrefixOf` checkName c]
    case rows ["/proj/.venv/bin/pytest"] of
      [(name, st, detail)] -> do
        (name, st) `shouldBe` ("test runner .venv/bin/pytest (python)", Ok)
        detail `shouldSatisfy` T.isInfixOf ".venv/bin/pytest found"
      other -> expectationFailure (show other)
    case rows [] of
      [(name, st, detail)] -> do
        (name, st) `shouldBe` ("test runner pytest (python)", Ok)
        detail `shouldSatisfy` T.isInfixOf "no uv.lock, .venv/bin/pytest or venv/bin/pytest"
      other -> expectationFailure (show other)
  it "the hspec runner row says which it found" $ do
    hs <- shippedLanguage "haskell" []
    let withCabal t = (ports ["ast-grep", "cabal", "env"] (Right ()) []) {dpRead = \f -> Identity (if f == "/proj/p.cabal" then Just t else Nothing), dpList = \_ -> Identity ["p.cabal"]}
        row t = [checkDetail c | c <- runIdentity (doctor (withCabal t) (DoctorInput (Just "/proj") [] [hs] [] [] local Nothing defaultPlans [] False)), "test runner" `T.isPrefixOf` checkName c]
    row "build-depends: hspec-junit-formatter" `shouldSatisfy` any (T.isInfixOf "hspec-junit-formatter found in p.cabal; format junit-xml")
    row "build-depends: hspec" `shouldSatisfy` any (T.isInfixOf "no hspec-junit-formatter in *.cabal or package.yaml")
  it "(f3) the check tool row names the [check] command candidate chosen, {= root =}/ dropped" $ do
    let Right python = parseLanguage "python" "/p" (T.unlines ["files = [\"**/*.py\"]", "comment = \"#\"", "grammar = { git = \"https://github.com/tree-sitter/tree-sitter-python\", rev = \"293fdc02038ee2bf0e2e206711b69c90ac0d413f\" }", "[check]", "format = \"sarif\"", "command = [{ when = [\".venv/bin/ruff\"], command = [\"{= root =}/.venv/bin/ruff\", \"check\"] }, { command = [\"ruff\", \"check\"] }]"])
        withFiles fs = (ports ["ast-grep", "ruff", "/proj/.venv/bin/ruff"] (Right ()) []) {dpRead = \f -> Identity (if f `elem` fs then Just "" else Nothing)}
        rows fs = [(checkName c, checkStatus c, checkDetail c) | c <- runIdentity (doctor (withFiles fs) (DoctorInput (Just "/proj") [] [python] [] [] local Nothing defaultPlans [] False)), "check tool" `T.isPrefixOf` checkName c]
    case rows ["/proj/.venv/bin/ruff"] of
      [(name, st, detail)] -> do
        (name, st) `shouldBe` ("check tool .venv/bin/ruff (python)", Ok)
        detail `shouldSatisfy` T.isInfixOf ".venv/bin/ruff found"
      other -> expectationFailure (show other)
    case rows [] of
      [(name, st, _)] -> (name, st) `shouldBe` ("check tool ruff (python)", Ok)
      other -> expectationFailure (show other)
  it "(g) a language without [test] gets no test runner line" $
    [checkName c | c <- runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) (input local)), "test runner" `T.isPrefixOf` checkName c] `shouldBe` []
  it "reports a broken language adapter or generator as an error" $ do
    let broken = DoctorInput (Just "/proj") ["p"] [] [("p/languages/x.toml", "bad grammar")] [("p/gen", "unknown key")]
        cs = runIdentity (doctor (ports ["ast-grep"] (Right ()) []) (broken local Nothing defaultPlans [] False))
    statusOf "language p/languages/x.toml" cs `shouldBe` Just Bad
    statusOf "generator p/gen" cs `shouldBe` Just Bad
  it "(e) checks that the recognition model ([recognize] model) is a configured backend" $ do
    let run name = runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) ((input local) {diRecognizeModel = Just name}))
    statusOf "recognize model" (run "nope") `shouldBe` Just Bad
    statusOf "recognize model" (run "local") `shouldBe` Just Ok
    statusOf "recognize model" (runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) (input local))) `shouldBe` Nothing
  it "outside a project: config is an error" $
    statusOf "config" (runIdentity (doctor (ports ["ast-grep"] (Right ()) []) (DoctorInput Nothing [] [] [] [] local Nothing defaultPlans [] False))) `shouldBe` Just Bad
  it "the plans runner: ok when found, with its timeout and do tools; an error with the fix when not" $ do
    let plans = defaultPlans {pcTimeout = 900, pcDoTools = ["Bash(cabal test*)"]}
        run exes = runIdentity (doctor (ports exes (Right ()) []) ((input local) {diPlans = plans}))
        [ok] = [c | c <- run ["ast-grep", "cargo", "claude"], checkName c == "plans runner claude"]
        [bad] = [c | c <- run ["ast-grep", "cargo"], checkName c == "plans runner claude"]
    checkStatus ok `shouldBe` Ok
    checkDetail ok `shouldSatisfy` (\d -> all (`T.isInfixOf` d) ["/bin/claude", "900 s", "Bash(cabal test*)"])
    checkStatus bad `shouldBe` Bad
    checkFix bad `shouldBe` Just "install Claude Code, or set [plans] runner"
  it "a capability a language lacks is a warning naming the key; one it has is ok" $ do
    let cs = runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) (input local))
        [mutation] = [c | c <- cs, checkName c == "rust: mutation"]
        [diag] = [c | c <- cs, checkName c == "rust: diagnostics"]
    (checkStatus mutation, checkDetail mutation, checkFix mutation) `shouldBe` (Warn, "absent", Just "add [[mutate]] to .olifant/languages/rust.toml")
    (checkStatus diag, checkDetail diag) `shouldBe` (Ok, "[check] format rustc-json · from toolchain cargo")
  it "shows each language's layers and where each capability's key came from" $ do
    hs <- shippedLanguage "haskell" [("project", ".olifant")]
    let cs = runIdentity (doctor (ports ["ast-grep", "cabal"] (Right ()) []) ((input local) {diLanguages = [hs]}))
        row n = head [c | c <- cs, checkName c == n]
    checkDetail (row "language haskell") `shouldBe` "loaded (shipped, project)"
    checkDetail (row "haskell: tests") `shouldSatisfy` T.isSuffixOf " · from toolchain hspec"
    checkDetail (row "haskell: units") `shouldSatisfy` T.isSuffixOf " · from shipped"
    checkFix (row "haskell: mutation") `shouldBe` Just "add [[mutate]] to .olifant/languages/haskell.toml"
    checkDetail (row "haskell: stub") `shouldBe` "[test] stub at end of do in spec · from toolchain hspec"
  it "rust: its tests, test units, selection and stub, from cargo-test and the shipped language" $ do
    rust <- shippedLanguage "rust" []
    let cs = runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) ((input local) {diLanguages = [rust]}))
        row n = head [c | c <- cs, checkName c == n]
    map (checkStatus . row) ["rust: tests", "rust: test units", "rust: test selection", "rust: stub"] `shouldBe` [Ok, Ok, Ok, Ok]
    checkDetail (row "rust: stub") `shouldBe` "[test] stub at end of mod tests · from toolchain cargo-test"
    checkDetail (row "rust: test units") `shouldBe` "[test] unit, above · from shipped"
  it "a queries row: the loaded grammar and its query files (elm from its pack); a bare grammar name is a warning" $ do
    rust <- shippedLanguage "rust" []
    elm <- shippedLanguage "elm" [("pack elm", "packs/elm")]
    let cs = runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) ((input local) {diLanguages = [rust, elm]}))
        row n = head [(checkStatus c, checkDetail c, checkFix c) | c <- cs, checkName c == n]
    row "rust: queries" `shouldBe` (Ok, "tree-sitter-rust @77a3747; impls, imports, tags, textobjects (shipped)", Nothing)
    row "elm: queries" `shouldBe` (Ok, "elm; imports, tags, textobjects, types (shipped)", Nothing)
    let bare = elm {langGrammar = Grammar "elm" Nothing}
        cs' = runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) ((input local) {diLanguages = [bare]}))
    [(checkStatus c, checkDetail c) | c <- cs', checkName c == "elm: queries"] `shouldBe` [(Warn, "no grammar (anchors from [targets.*] only)")]
  it "the grammar row: loaded, no source, no compiler, fetch failed, build failed, ABI, load failed" $ do
    rust <- shippedLanguage "rust" []
    let go = rust {langName = "go", langGrammar = Grammar "go" Nothing}
        answer g
          | grName g == "rust" = Right (GrammarInfo "tree-sitter-rust @77a3747" 15 "/c/grammars/rust-0123abcd.dylib" (Just 0.31))
          | otherwise = Left NoSource
        cs = runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) {dpGrammar = Identity . answer} ((input local) {diLanguages = [rust, go]}))
        row n = head [(checkStatus c, checkDetail c, checkFix c) | c <- cs, checkName c == n]
        names = map checkName cs
    row "rust: grammar" `shouldBe` (Ok, "tree-sitter-rust @77a3747, ABI 15; /c/grammars/rust-0123abcd.dylib; built now in 0.3 s", Nothing)
    -- before the queries row
    length (takeWhile (/= "rust: grammar") names) + 1 `shouldBe` length (takeWhile (/= "rust: queries") names)
    row "go: grammar" `shouldBe` (Warn, "no source (grammar = \"go\" names it only): units, generic anchors and counts need one", Just "add grammar = { git = URL, rev = SHA } (or { source = \"dir\" }) to .olifant/languages/go.toml")
    let failing = runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) {dpGrammar = \_ -> Identity (Left (BuildFailed "x"))} ((input local) {diLanguages = [rust]}))
    [(checkStatus c, checkDetail c) | c <- failing, checkName c == "rust: queries"]
      `shouldBe` [(Warn, "tree-sitter-rust @77a3747; impls, imports, tags, textobjects (shipped) (not read: the grammar is not loaded)")]
    let one f = let c = grammarRow rust (Left f) in (checkStatus c, checkDetail c, checkFix c)
    fst3 (one (NeedsCompiler "cc is not on PATH")) `shouldBe` Bad
    snd3 (one (NeedsCompiler "cc is not on PATH")) `shouldBe` "needs a C compiler to build: cc is not on PATH"
    one (FetchFailed "fetching https://github.com/tree-sitter/tree-sitter-rust @77a3747 failed: Could not resolve host: github.com")
      `shouldBe` (Bad, "fetching https://github.com/tree-sitter/tree-sitter-rust @77a3747 failed: Could not resolve host: github.com", Just "check the network, or put the sources under $OLIFANT_GRAMMAR_SOURCES/rust/")
    one (BuildFailed "parser.c:1:1: error: x") `shouldBe` (Bad, "building failed: parser.c:1:1: error: x", Nothing)
    one (AbiMismatch 16 (13, 15)) `shouldBe` (Bad, "ABI 16; this olifant reads 13–15", Just "pin a rev generated with tree-sitter ≤ 0.27, or update olifant")
    one (LoadFailed "dlopen: no such file") `shouldBe` (Bad, "loading failed: dlopen: no such file", Nothing)
  it "a queries row names a project's query file and its origin" $ do
    rust <- shippedLanguage "rust" []
    let own = rust {langQueries = [("imports.scm", "(extern_crate_declaration) @import\n", "project")]}
        cs = runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) ((input local) {diLanguages = [own]}))
    [checkDetail c | c <- cs, checkName c == "rust: queries"] `shouldBe` ["tree-sitter-rust @77a3747; imports (project), impls, tags, textobjects (shipped)"]
  it "evidence replaces how, origin kept; tests not run keep their text, ok" $ do
    rust <- shippedLanguage "rust" []
    let ev = Evidence (Proven "3 found in 2 files (e.g. src/a.rs:1 add)") (Unproven "not seen (no test file in the 40 sampled)") (Unproven "not run (olifant doctor --run)") (Proven "end of struct X (src/a.rs:3)") (Proven "expr wrappers parse with their placeholders") (Unproven "no [conjure.port]: conjure reads no fakes")
        cs = runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) ((input local) {diLanguages = [rust], diEvidence = [("rust", ev)]}))
        row n = head [(checkStatus c, checkDetail c, checkFix c) | c <- cs, checkName c == n]
    row "rust: units" `shouldBe` (Ok, "3 found in 2 files (e.g. src/a.rs:1 add) · from shipped", Nothing)
    row "rust: test units" `shouldBe` (Warn, "not seen (no test file in the 40 sampled) · from shipped", Nothing)
    row "rust: tests" `shouldBe` (Ok, "[test] format lines, tests_for {= dir =}/{= stem =}.rs (not run: olifant doctor --run) · from toolchain cargo-test", Nothing)
    row "rust: anchors" `shouldBe` (Ok, "end of struct X (src/a.rs:3) · from shipped", Nothing)
    row "rust: holes" `shouldBe` (Ok, "expr wrappers parse with their placeholders · from shipped", Nothing)
    map checkName [c | c <- cs, "rust: " `T.isPrefixOf` checkName c] `shouldSatisfy` (\ns -> take 4 (drop (length (takeWhile (/= "rust: test units") ns)) ns) == ["rust: test units", "rust: sibling tests", "rust: anchors", "rust: holes"])
    row "rust: sibling tests" `shouldBe` (Warn, "no [conjure.port]: conjure reads no fakes", Nothing)
  it "a Disproven row is a warning with the regenerate-or-provider fix; with --run, a run's evidence shows" $ do
    rust <- shippedLanguage "rust" []
    let ev = Evidence (Disproven "0 found in 2 files; src/a.rs:1 is `fn add() {}`") (Proven "1 found in 1 test files (e.g. src/a.rs:5 adds)") (Proven "ran cargo on src/a.rs, 1 results parsed (lines)") (Unproven "no [targets.*]") (Proven "x") (Unproven "no [conjure.port]: conjure reads no fakes")
        cs = runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) ((input local) {diLanguages = [rust], diEvidence = [("rust", ev)], diRan = True}))
        row n = head [(checkStatus c, checkDetail c, checkFix c) | c <- cs, checkName c == n]
    row "rust: units" `shouldBe` (Warn, "0 found in 2 files; src/a.rs:1 is `fn add() {}` · from shipped", Just "regenerate the key or supply a provider (see chapter 9, Providers)")
    row "rust: tests" `shouldBe` (Ok, "ran cargo on src/a.rs, 1 results parsed (lines) · from toolchain cargo-test", Nothing)
    -- an evidence warning is never an error: doctor's exit stays 0
    [c | c <- cs, checkStatus c == Bad, "rust: " `T.isPrefixOf` checkName c] `shouldBe` []
  it "a Disproven row of a layer init wrote says how to regenerate it; a hand-written layer's does not" $ do
    rust <- shippedLanguage "rust" []
    let ev = Evidence (Disproven "0 found in 2 files") (Proven "t") (Proven "r") (Unproven "a") (Proven "x") (Unproven "s")
        fixWith layer = head [checkFix c | c <- runIdentity (doctor (ports ["ast-grep", "cargo"] (Right ()) []) {dpRead = \f -> Identity (if f == "/proj/.olifant/languages/rust.toml" then Just layer else Nothing)} ((input local) {diLanguages = [rust], diEvidence = [("rust", ev)]})), checkName c == "rust: units"]
    fixWith (initHeader <> "\n[test]\nunit = ['^x']\n") `shouldBe` Just "regenerate the key or supply a provider (see chapter 9, Providers); or run olifant init --language rust again after deleting .olifant/languages/rust.toml"
    fixWith "# my own layer\n[test]\nunit = ['^x']\n" `shouldBe` Just "regenerate the key or supply a provider (see chapter 9, Providers)"

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b
