-- | Internal to Olifant.Server: the two guards every reader of a project's
-- files from a plan goes through: a path confined to the root, and IO that
-- cannot fail its caller.
module Olifant.Server.Confine
  ( confined
  , safely
  ) where

import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.List (isPrefixOf)
import Olifant.Language.Grammar (GrammarUnavailable)
import Olifant.Workspace (normalisePath)
import System.FilePath (isAbsolute)

-- | An anchor's path as one inside the project (normalised, relative), or
-- Nothing: an absolute path, or one that climbs out of the root, is never
-- read. The one place anchors are confined (a later allowlist of sibling
-- roots widens only this).
confined :: FilePath -> Maybe FilePath
confined path
  | isAbsolute path = Nothing
  | rel == ".." || "../" `isPrefixOf` rel = Nothing
  | otherwise = Just rel
  where
    rel = normalisePath path

-- | IO that cannot fail the caller: an exception (not an async one, nor a
-- grammar that cannot load: that stops the request) is the default.
safely :: a -> IO a -> IO a
safely def io =
  try io >>= \case
    Right v -> pure v
    Left (e :: SomeException)
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | Just (_ :: GrammarUnavailable) <- fromException e -> throwIO e
      | otherwise -> pure def
