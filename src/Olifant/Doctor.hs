-- | `olifant doctor` (spec §4.7): what generators need in this project — the
-- config, packs, language adapters, ast-grep, each adapter's check tool on
-- this process's PATH, the model, what each language can do (and the key
-- that would enable what it cannot), and the plans runner — and how to fix
-- what is missing.
module Olifant.Doctor
  ( Status (..)
  , Check (..)
  , DoctorInput (..)
  , DoctorPorts (..)
  , doctor
  , grammarRow
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Capabilities (Capability (..), capabilities)
import Olifant.Evidence (Evidence (..), Proof (..))
import Olifant.Catalog (PlansConfig (..))
import Olifant.Language (CheckDef (..), Grammar (..), Language (..), TestDef (..), checkProgram, Probe (..), chooseCheckCommand, chooseTestCommand, grammarLabel, loadableGrammar, originOf, queriesOf)
import System.FilePath (dropExtension, (</>))
import Numeric (showFFloat)
import Olifant.Language.Grammar (GrammarFailure (..), GrammarInfo (..), failureDetail)
import System.Info (os)
import Olifant.Model (Backend (..), ModelConfig (..))

data Status = Ok | Warn | Bad
  deriving stock (Eq, Show)

data Check = Check {checkName :: Text, checkStatus :: Status, checkDetail :: Text, checkFix :: Maybe Text}
  deriving stock (Eq, Show)

-- | What the catalog says about the project: its root, packs, languages,
-- broken language adapters and generators (path or id, why), and `[model]`.
data DoctorInput = DoctorInput
  { diProject :: Maybe FilePath
  , diPacks :: [Text]
  , diLanguages :: [Language]
  , diBrokenLanguages :: [(Text, Text)]
  , diBrokenGenerators :: [(Text, Text)]
  , diModel :: ModelConfig
  , -- | `[recognize] model`: the backend recognition asks when unsure
    diRecognizeModel :: Maybe Text
  , -- | `[plans]`: the runner errands start, its timeout and `do` tools
    diPlans :: PlansConfig
  , -- | each language's evidence ('Olifant.Evidence'), by name; a language
    -- without any shows how each capability is provided, as before
    diEvidence :: [(Text, Evidence)]
  , -- | `doctor --run`: the tests row's evidence comes from a run
    diRan :: Bool
  }

-- | The outside world the doctor looks at.
data DoctorPorts m = DoctorPorts
  { dpFind :: String -> m (Maybe FilePath)
  -- ^ an executable on this process's PATH
  , dpPath :: m Text
  -- ^ this process's PATH, to show
  , dpProbe :: Backend -> m (Either Text ())
  -- ^ does the model answer (a short timeout)
  , dpEnvSet :: Text -> m Bool
  -- ^ is an environment variable set (never its value)
  , dpRead :: FilePath -> m (Maybe Text)
  -- ^ a file's text at this path, Nothing when there is none (a `command`
  -- candidate's `when` and `has`)
  , dpList :: FilePath -> m [FilePath]
  -- ^ the names of the files directly in this folder (a `when` entry with `*`)
  , dpGrammar :: Grammar -> m (Either GrammarFailure GrammarInfo)
  -- ^ the grammar loaded as a first use loads it (fetched, built, loaded):
  -- doctor is the warm-up
  }

doctor :: Monad m => DoctorPorts m -> DoctorInput -> m [Check]
doctor ports di = do
  astGrep <- tool "ast-grep" "ast-grep" "brew install ast-grep (or cargo install ast-grep)"
  tools <- traverse languageTools (diLanguages di)
  model <- modelCheck
  runner <- plansRunner
  grammars <- traverse (\l -> (langName l,) <$> dpGrammar ports (langGrammar l)) (diLanguages di)
  pure ([config] ++ packs ++ languages ++ broken ++ [astGrep] ++ concat tools ++ [model] ++ recognizeCheck ++ concatMap (capabilityRows grammars) (diLanguages di) ++ [runner])
  where
    -- what each language can do; an absent capability names its key
    capabilityRows grammars lang =
      [ case capHow c of
          Right how ->
            let origin = maybe "" (" · from " <>) (originOf lang (capKey c))
             in case lookup (langName lang) (diEvidence di) >>= \ev -> proofOf (capName c) ev of
                  Nothing -> Check name Ok (how <> origin) Nothing
                  Just (Proven t) -> Check name Ok (t <> origin) Nothing
                  -- tests are run only with `doctor --run`: not run is no warning
                  Just (Unproven _) | capName c == "tests", not (diRan di) -> Check name Ok (how <> " (not run: olifant doctor --run)" <> origin) Nothing
                  Just (Unproven t) -> Check name Warn (t <> origin) Nothing
                  Just (Disproven t) -> Check name Warn (t <> origin) (Just "regenerate the key or supply a provider (see chapter 9, Providers)")
          Left key ->
            let (k, note) = T.breakOn " (" key
             in Check name Warn "absent" (Just (fromMaybe ("add " <> k <> " to .olifant/languages/" <> langName lang <> ".toml" <> note) (capFix c)))
      | c <- capabilities lang
      , let name = langName lang <> ": " <> capName c
      ]
        ++ [grammarRow lang r | Just r <- [lookup (langName lang) grammars]]
        ++ [unloaded (lookup (langName lang) grammars) (queriesRow lang)]
    -- a queries row whose grammar does not load reads nothing
    unloaded = \case
      Just (Left f) | f /= NoSource -> \c -> if checkStatus c == Ok then c {checkStatus = Warn, checkDetail = checkDetail c <> " (not read: the grammar is not loaded)"} else c
      _ -> id
    -- the tree-sitter grammar and queries the generic anchor phrases read
    queriesRow lang = case loadableGrammar lang of
      Just g ->
        -- the files of each origin, in name order: the project's first
        let qs = queriesOf lang
            origins = [o | o <- nubOrd' ([o | (_, _, o) <- qs, o /= "shipped"] ++ ["shipped"]), o `elem` [o' | (_, _, o') <- qs]]
            group o = T.intercalate ", " [T.pack (dropExtension f) | (f, _, o') <- qs, o' == o] <> " (" <> o <> ")"
            nubOrd' = foldr (\x acc -> x : filter (/= x) acc) []
         in Check (langName lang <> ": queries") Ok (grammarLabel g <> "; " <> T.intercalate ", " (map group origins)) Nothing
      Nothing
        | Just _ <- grSource (langGrammar lang) -> Check (langName lang <> ": queries") Warn ("no queries for the " <> grName (langGrammar lang) <> " grammar (anchors from [targets.*] only)") Nothing
        | otherwise -> Check (langName lang <> ": queries") Warn "no grammar (anchors from [targets.*] only)" Nothing
    -- the evidence row a capability takes
    proofOf cap ev = lookup cap [("units", evUnits ev), ("test units", evTestUnits ev), ("tests", evTests ev), ("anchors", evAnchors ev), ("holes", evHoles ev)]
    plansRunner = do
      let plans = diPlans di
          runner = T.pack (pcRunner plans)
          detail at = T.intercalate "; " ([T.pack at, "timeout " <> T.pack (show (pcTimeout plans)) <> " s"] ++ ["do tools " <> T.intercalate ", " (pcDoTools plans) | not (null (pcDoTools plans))])
      dpFind ports (pcRunner plans) >>= \case
        Just at -> pure (Check ("plans runner " <> runner) Ok (detail at) Nothing)
        Nothing -> do
          path <- dpPath ports
          pure (Check ("plans runner " <> runner) Bad ("`" <> runner <> "` is not on this PATH: " <> path) (Just "install Claude Code, or set [plans] runner"))
    recognizeCheck = case diRecognizeModel di of
      Nothing -> []
      Just name
        | name `elem` map fst (mcBackends (diModel di)) -> [Check "recognize model" Ok ("recognition asks " <> name <> " when unsure") Nothing]
        | otherwise -> [Check "recognize model" Bad ("[recognize] model `" <> name <> "` is not under [model.backends]") (Just ("add [model.backends." <> name <> "] or change [recognize] model"))]
    config = case diProject di of
      Just root -> Check "config" Ok ("project at " <> T.pack root) Nothing
      Nothing -> Check "config" Bad "no .olifant/ here or above" (Just "run olifant in a project, or create .olifant/config.toml")
    packs = [Check ("pack " <> p) Ok "loaded" Nothing | p <- diPacks di]
    languages = [Check ("language " <> langName l) Ok ("loaded" <> (if null (langLayers l) then "" else " (" <> T.intercalate ", " (langLayers l) <> ")")) Nothing | l <- diLanguages di]
    broken =
      [Check ("language " <> p) Bad why (Just "fix the adapter file") | (p, why) <- diBrokenLanguages di]
        ++ [Check ("generator " <> g) Bad why (Just "fix its generator.toml") | (g, why) <- diBrokenGenerators di]
    tool name exe fix =
      dpFind ports exe >>= \case
        Just at -> pure (Check name Ok (T.pack at) Nothing)
        Nothing -> do
          path <- dpPath ports
          pure (Check name Bad ("`" <> T.pack exe <> "` is not on this PATH: " <> path) (Just fix))
    languageTools lang =
      (++)
        <$> maybe (pure []) (checkTool lang) (langCheck lang)
        <*> maybe (pure []) (testRunner lang) (langTest lang)
    atProject = case diProject di of
      Nothing -> Probe (const (pure Nothing)) (const (pure []))
      Just r -> Probe (\f -> dpRead ports (r </> f)) (\f -> dpList ports (r </> f))
    checkTool lang c0 =
      chooseCheckCommand atProject c0 >>= commandRow "check tool" "[check]" lang (\c -> (checkCommand c, checkInstall c, ""))
    testRunner lang td0 =
      chooseTestCommand atProject td0 >>= commandRow "test runner" "[test]" lang (\td -> (testCommand td, testInstall td, (if testFormat td /= testFormat td0 then "; format " <> testFormat td else "") <> "; tests_for = " <> testsFor td <> (if testFellBack td then "; reading console output: name {= report =} for JUnit" else "")))
    -- one line for the command candidate the project's files choose, named
    -- by what it is for and as written (`{= root =}/` dropped), found at the
    -- project root, and why
    commandRow kind table lang parts = \case
      Left why -> pure [Check (kind <> " (" <> langName lang <> ")") Bad why (Just ("add the file a " <> table <> " command candidate names, or a candidate without `when`"))]
      Right (def, why) -> let (command, install, more) = parts def in case checkProgram command of
        Nothing -> pure []
        Just exe -> do
          let shown = T.replace "{= root =}/" "" exe
              found = T.unpack (T.replace "{= root =}" (maybe "." T.pack (diProject di)) exe)
          c <- tool (kind <> " " <> shown <> " (" <> langName lang <> ")") found (fromMaybe ("install " <> shown <> " and make sure it is on the PATH your editor starts with") install)
          pure [if checkStatus c == Ok then c {checkDetail = checkDetail c <> maybe "" ("; " <>) why <> more} else c]
    modelCheck = case (mcDefault (diModel di), mcBackends (diModel di)) of
      (Nothing, []) -> pure (Check "model" Warn "no [model] configured: holes stay placeholders" (Just modelSnippet))
      (Nothing, _) -> pure (Check "model" Warn "[model] has backends but no default" (Just "set `default = \"NAME\"` under [model]"))
      (Just name, backends) -> case lookup name backends of
        Nothing -> pure (Check "model" Bad ("default `" <> name <> "` is not under [model.backends]") (Just ("add [model.backends." <> name <> "] or change default")))
        Just b@(OpenAICompatible url m) ->
          dpProbe ports b >>= \case
            Right () -> pure (Check "model" Ok (m <> " answers at " <> url) Nothing)
            Left why -> pure (Check "model" Warn (m <> " at " <> url <> ": " <> why) (Just ("start the server (e.g. `ollama serve`) and pull the model (`ollama pull " <> m <> "`)")))
        Just (Anthropic m keyEnv) ->
          dpEnvSet ports keyEnv >>= \case
            True -> pure (Check "model" Ok (m <> " via Anthropic; " <> keyEnv <> " is set") Nothing)
            False -> pure (Check "model" Warn (keyEnv <> " is not set: holes stay placeholders") (Just ("export " <> keyEnv <> "=… in the shell that starts olifant (or your editor)")))
        Just (Jev m keyEnv) ->
          dpEnvSet ports keyEnv >>= \case
            True -> pure (Check "model" Warn (m <> " via Jev answers decisions only, not holes; " <> keyEnv <> " is set") (Just "point [model] default at a chat backend and [decide] model at this one"))
            False -> pure (Check "model" Warn (keyEnv <> " is not set: decisions stay unsure") (Just ("export " <> keyEnv <> "=… in the shell that starts olifant (or your editor)")))

modelSnippet :: Text
modelSnippet =
  T.unlines
    [ "add to .olifant/config.toml:"
    , "[model]"
    , "default = \"local\""
    , "[model.backends.local]"
    , "kind = \"openai-compatible\""
    , "url = \"http://localhost:11434/v1\""
    , "model = \"qwen3-coder:30b\""
    ]

-- | `LANGUAGE: grammar`: the grammar loaded (its label, ABI and library;
-- the build's seconds when this run built it), or why not and the fix.
grammarRow :: Language -> Either GrammarFailure GrammarInfo -> Check
grammarRow lang = \case
  Right gi -> Check name Ok (giLabel gi <> ", ABI " <> tshow (giAbi gi) <> "; " <> T.pack (giLibrary gi) <> maybe "" (\t -> "; built now in " <> T.pack (showFFloat (Just 1) t "") <> " s") (giBuiltNow gi)) Nothing
  Left f -> Check name (if f == NoSource then Warn else Bad) (failureDetail g f) (fixFor f)
  where
    name = langName lang <> ": grammar"
    g = grName (langGrammar lang)
    tshow :: Show a => a -> Text
    tshow = T.pack . show
    fixFor = \case
      NoSource -> Just ("add grammar = { git = URL, rev = SHA } (or { source = \"dir\" }) to .olifant/languages/" <> langName lang <> ".toml")
      NeedsCompiler _ -> Just (if os == "darwin" then "xcode-select --install" else "install a C compiler (cc)")
      FetchFailed _ -> Just ("check the network, or put the sources under $OLIFANT_GRAMMAR_SOURCES/" <> g <> "/")
      AbiMismatch _ _ -> Just "pin a rev generated with tree-sitter ≤ 0.27, or update olifant"
      BuildFailed _ -> Nothing
      LoadFailed _ -> Nothing
