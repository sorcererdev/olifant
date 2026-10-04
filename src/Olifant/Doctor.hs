-- | `olifant doctor` (spec §4.7): what generators need in this project — the
-- config, packs, language adapters, ast-grep, each adapter's check tool on
-- this process's PATH, and the model — and how to fix what is missing.
module Olifant.Doctor
  ( Status (..)
  , Check (..)
  , DoctorInput (..)
  , DoctorPorts (..)
  , doctor
  ) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language (CheckDef (..), Language (..))
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
  }

doctor :: Monad m => DoctorPorts m -> DoctorInput -> m [Check]
doctor ports di = do
  astGrep <- tool "ast-grep" "ast-grep" "brew install ast-grep (or cargo install ast-grep)"
  checkTools <- traverse checkTool (mapMaybe (\l -> (l,) <$> langCheck l) (diLanguages di))
  model <- modelCheck
  pure ([config] ++ packs ++ languages ++ broken ++ [astGrep] ++ concat checkTools ++ [model])
  where
    config = case diProject di of
      Just root -> Check "config" Ok ("project at " <> T.pack root) Nothing
      Nothing -> Check "config" Bad "no .olifant/ here or above" (Just "run olifant in a project, or create .olifant/config.toml")
    packs = [Check ("pack " <> p) Ok "loaded" Nothing | p <- diPacks di]
    languages = [Check ("language " <> langName l) Ok "loaded" Nothing | l <- diLanguages di]
    broken =
      [Check ("language " <> p) Bad why (Just "fix the adapter file") | (p, why) <- diBrokenLanguages di]
        ++ [Check ("generator " <> g) Bad why (Just "fix its generator.toml") | (g, why) <- diBrokenGenerators di]
    tool name exe fix =
      dpFind ports exe >>= \case
        Just at -> pure (Check name Ok (T.pack at) Nothing)
        Nothing -> do
          path <- dpPath ports
          pure (Check name Bad ("`" <> T.pack exe <> "` is not on this PATH: " <> path) (Just fix))
    checkTool (lang, check) = case checkCommand check of
      exe : _ -> pure <$> tool ("check tool " <> exe <> " (" <> langName lang <> ")") (T.unpack exe) (installHint exe)
      [] -> pure []
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

-- | Where a check tool usually comes from.
installHint :: Text -> Text
installHint = \case
  "cargo" -> "install Rust with rustup; cargo lives in ~/.cargo/bin, or /opt/homebrew/opt/rustup/bin with Homebrew's rustup (keg-only: add it to PATH, then restart your editor from a new shell)"
  "elm" -> "npm install -g elm (or brew install elm)"
  "ghc" -> "install with ghcup; it lives in ~/.ghcup/bin"
  "cabal" -> "install with ghcup; it lives in ~/.ghcup/bin"
  exe -> "install " <> exe <> " and make sure it is on the PATH your editor starts with"

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
