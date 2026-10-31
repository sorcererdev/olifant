-- | Internal to Olifant.Server: `olifant/waive`, a survivor set aside for a
-- reason. A waiver is a line of `.olifant/state/waived.log` (ignored by git),
-- kept by the mutant and not its line, so it holds when the unit moves.
module Olifant.Server.Waive
  ( waiveAt
  , waivedIn
  , identityOf
  ) where

import Control.Exception (SomeException, try)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime)
import Olifant.App (Ports (..))
import Olifant.Mutate (Mutant (..))
import Olifant.Server.Site (Site (..), siteAt)
import Olifant.Server.State (ServerState (..))
import Olifant.Server.Unit (Unit (..), unitAt)
import Olifant.Workspace (Workspace (..))
import System.FilePath ((</>))

-- | A mutant's identity in a unit: what it changed from and to, one line
-- each, and its mutated line, trimmed. Never its line number.
type Identity = (Text, Text, Text)

-- | A multi-line text on one line, the way `olifant/mutate` sends it.
flat :: Text -> Text
flat = T.replace "\n" "\9166"

identityOf :: Mutant -> Identity
identityOf m = (squeeze (flat (muFrom m)), squeeze (flat (muTo m)), squeeze (mutatedLine m))
  where
    mutatedLine x = fromMaybe "" (lookup (muLine x) (zip [1 :: Int ..] (T.splitOn "\n" (muText x))))

-- | A field on one line, its whitespace single spaces: how it is written
-- and so how it is matched.
squeeze :: Text -> Text
squeeze = T.unwords . T.words

stateFile :: FilePath -> FilePath
stateFile root = root </> ".olifant/state/waived.log"

-- | Waive the survivor M of the unit at its line in PATH, for REASON.
-- Left is one sentence on why not.
waiveAt :: FilePath -> Text -> Mutant -> ServerState -> IO (ServerState, Either Text ())
waiveAt path reason m s0
  | T.null (squeeze reason) = pure (s0, Left "a waiver needs a reason: why is this mutant equivalent, or not worth a test?")
  | otherwise =
      siteAt path (muLine m) s0 >>= \case
        (s, Left why) -> pure (s, Left why)
        (s, Right site) ->
          unitAt (pSyntax (ssPorts s)) (siteLang site) (Just (siteModuleRel site)) (siteModuleText site) (muLine m) >>= \case
            Left why -> pure (s, Left (why <> ": a survivor belongs to one"))
            Right unit -> do
              let ports = ssPorts s
                  (from, to, line) = identityOf m
              now <- pNow ports
              let stamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
                  entry = T.intercalate "\t" [stamp, T.pack (siteModuleRel site), unitName unit, from, to, line, squeeze reason]
                  file = stateFile (siteRoot site)
              written <- try (readText (pFiles ports) file >>= \old -> writeText (pFiles ports) file (fromMaybe "" old <> entry <> "\n")) :: IO (Either SomeException ())
              pure (s, either (const (Left ("could not write " <> T.pack file))) Right written)

-- | The mutants waived in UNIT of the module REL (project-relative) under ROOT.
waivedIn :: Ports -> FilePath -> FilePath -> Text -> IO [Identity]
waivedIn ports root rel unit = do
  logged <- fromMaybe "" <$> readText (pFiles ports) (stateFile root)
  pure [(from, to, line) | _ : r : u : from : to : line : _ <- map (T.splitOn "\t") (T.lines logged), r == T.pack rel, u == unit]
