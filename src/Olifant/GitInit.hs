-- | `olifant init`'s first commit, through git (the runtime's 'rtGitInit').
module Olifant.GitInit (gitInit) where

import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (canonicalizePath, findExecutable)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | `git init`, then everything added and committed — unless @dir@ is
-- inside another repository: a nested one would hide the new project from
-- it, so nothing is initialised or committed, and the 'Left' says so.
gitInit :: FilePath -> Text -> IO (Either Text ())
gitInit dir message =
  findExecutable "git" >>= \case
    Nothing -> pure (Left "git is not installed")
    Just git -> do
      let run args = readProcessWithExitCode git (["-C", dir] ++ args) ""
      (inRepo, top, _) <- run ["rev-parse", "--show-toplevel"]
      here <- canonicalizePath dir
      there <- if inRepo == ExitSuccess then Just <$> canonicalizePath (T.unpack (T.strip (T.pack top))) else pure Nothing
      case there of
        Just t | t /= here -> pure (Left ("it is inside the git repository at " <> T.pack t <> "; add and commit it there"))
        _ -> do
          (c1, _, e1) <- run ["init", "-q"]
          (c2, _, e2) <- run ["add", "-A"]
          (c3, _, e3) <- run ["commit", "-qm", T.unpack message]
          pure $ case [e | (c, e) <- [(c1, e1), (c2, e2), (c3, e3)], c /= ExitSuccess] of
            [] -> Right ()
            e : _ -> Left (T.strip (T.pack e))
