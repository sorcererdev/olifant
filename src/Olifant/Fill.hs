-- | The fill phase (spec §6.4): one request for every hole in a plan, each
-- fill parsed as its kind and compile-checked, failing holes retried with
-- the diagnostics, and any still failing left as their placeholder.
module Olifant.Fill
  ( FillHole (..)
  , FillRequest (..)
  , Stance (..)
  , Filler (..)
  , ModelFiller (..)
  , parseFills
  , replayFiller
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Olifant.Toml qualified as Toml
import Data.Text (Text)

-- | One hole as the model sees it: what it is, the file around it with the
-- hole marked ⟦name⟧, and on a retry the previous fill and why it failed.
data FillHole = FillHole
  { fhName :: Text
  , fhKind :: Text
  , fhInstruction :: Text
  , fhFile :: FilePath
  , fhContext :: Text
  , fhPrevious :: [(Text, Text)]
  -- ^ every earlier attempt (fill, why it failed), oldest first
  , fhImports :: Bool
  -- ^ the fill may begin with import lines (the language says how they look)
  , fhImportExample :: Maybe (Text, Text)
  -- ^ the language's `[imports] example`: the name a fill needs, and the
  -- fill that starts with its import line
  }
  deriving stock (Eq, Show)

-- | How the model is asked: to fill holes well, (a conjure) to write the
-- least code that makes one failing test pass, (init --language) to write
-- whole files, one per hole, or (init --language's form) to answer the keys
-- of a configuration file, one TOML value per hole.
data Stance = FillStance | ConjureStance | FileStance | ValueStance
  deriving stock (Eq, Show)

-- | 'frContext': related files the model should see (a fix's context, e.g.
-- the port's trait it must call).
data FillRequest = FillRequest {frHoles :: [FillHole], frInputs :: [(Text, Text)], frContext :: [(FilePath, Text)], frStance :: Stance}
  deriving stock (Eq, Show)

-- | The port to a model: fills by hole name, or why there are none.
newtype Filler m = Filler {runFill :: FillRequest -> m (Either Text (Map Text Text))}

-- | A configured model: its name (for the plan), retries, and its port.
data ModelFiller m = ModelFiller {mfName :: Text, mfRetries :: Int, mfFiller :: Filler m}

-- | `[fills]` in a `fills.toml`: hole name = recorded code.
parseFills :: Text -> Either Text (Map Text Text)
parseFills src = do
  top <- Toml.parseToml src
  fills <- fromMaybe M.empty <$> Toml.optional "fills" Toml.table top
  traverse Toml.text fills

-- | Recorded fills as a model: every hole asked for must have one.
replayFiller :: Monad m => Map Text Text -> Filler m
replayFiller fills = Filler $ \req -> pure $ case [fhName h | h <- frHoles req, M.notMember (fhName h) fills] of
  [] -> Right fills
  missing -> Left (T.intercalate "; " ["no recorded fill for `" <> n <> "` in fills.toml" | n <- missing])
