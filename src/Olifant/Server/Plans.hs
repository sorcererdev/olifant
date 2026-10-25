-- | Internal to Olifant.Server: a plan's anchors (`@path:Unit`) as the
-- server sees them: each resolved to its unit (over the editor's text),
-- stale and missing ones as diagnostics, completed after `@`, and the
-- plan's anchored lines as a list of files.
module Olifant.Server.Plans
  ( isPlanPath
  , Resolved (..)
  , resolveAnchor
  , planDiagnostics
  , anchorItems
  , FileItem (..)
  , planFiles
  ) where

import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.List (isPrefixOf)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..), PlansConfig (..))
import Olifant.Language.Adapter (languageFor)
import Olifant.Plans (Anchor (..), Line (..), Section (..), Verdict (..), parsePlan, sectionOf)
import Olifant.Server.State (ServerState (..))
import Olifant.Server.Unit (Named (..), fingerprint, unitNamed, unitsOf)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (isAbsolute, takeExtension, (</>))

-- | An absolute path under the project's plans folder, ending `.md`.
isPlanPath :: Project -> FilePath -> Bool
isPlanPath p path = takeExtension path == ".md" && (dir <> "/") `isPrefixOf` normalisePath path
  where
    dir = normalisePath (prRoot p </> pcDir (catalogPlans (prCatalog p)))

-- | An anchor resolved: its file (relative, absolute), its unit and that
-- unit's fingerprint when found, and the file's text when it exists.
data Resolved = Resolved
  { rsRel :: FilePath
  , rsAbs :: FilePath
  , rsUnit :: Maybe Named
  , rsFingerprint :: Maybe Text
  , rsText :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | The unit an anchor names, over the editor's text: none when the file
-- or the unit is missing, the language is unknown, or reading fails.
resolveAnchor :: ServerState -> Project -> Anchor -> IO Resolved
resolveAnchor s p a = do
  text <- maybe (pure Nothing) (safely Nothing . readText (prWorkspace p)) (confined (anPath a))
  unit <- case (text, languageFor (catalogLanguages (prCatalog p)) rel) of
    (Just t, Just lang) -> safely Nothing (either (const Nothing) (unitNamed (anUnit a)) <$> unitsOf (pSyntax (ssPorts s)) lang t)
    _ -> pure Nothing
  pure (Resolved rel (normalisePath (prRoot p </> rel)) unit (fingerprint <$> text <*> fmap namedLines unit) text)
  where
    rel = normalisePath (anPath a)

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

-- | IO that cannot fail the caller: an exception (not an async one) is the default.
safely :: a -> IO a -> IO a
safely def io =
  try io >>= \case
    Right v -> pure v
    Left (e :: SomeException)
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> pure def

-- | Each anchor resolved once, by its path and unit.
resolveAll :: ServerState -> Project -> [Anchor] -> IO (M.Map (FilePath, Text) Resolved)
resolveAll s p as = M.fromList <$> mapM (\a -> (key a,) <$> resolveAnchor s p a) (nub' as)
  where
    nub' = M.elems . M.fromList . map (\a -> (key a, a))

key :: Anchor -> (FilePath, Text)
key a = (anPath a, anUnit a)

-- | A plan's anchors that no longer hold: one verified at another
-- fingerprint (information), one whose unit is not found (a hint), each
-- over the anchor's columns on its line.
planDiagnostics :: ServerState -> Project -> FilePath -> Text -> IO [Diagnostic]
planDiagnostics s p _ text = do
  let anchored = [(l, a) | l <- parsePlan text, Just a <- [lnAnchor l]]
  resolved <- resolveAll s p (map snd anchored)
  pure (mapMaybe (\(l, a) -> diagnose l a =<< M.lookup (key a) resolved) anchored)
  where
    diagnose l a r = case (rsUnit r, lnVerdict l, rsFingerprint r) of
      (Nothing, _, _) -> Just (at l a DiagnosticSeverity_Hint "? not found")
      (Just _, Verified fp, Just now) | fp /= now -> Just (at l a DiagnosticSeverity_Information ("✗ changed since " <> fp))
      _ -> Nothing
    at l a sev msg = Diagnostic (columns l a) (Just sev) Nothing Nothing (Just "olifant") msg Nothing Nothing Nothing
    columns l a =
      let row = fromIntegral (lnRow l)
          col n = fromIntegral (utf16Length (T.take n (lnText l)))
       in Range (Position row (col (anStart a))) (Position row (col (anEnd a)))

utf16Length :: Text -> Int
utf16Length = T.foldl' (\n c -> n + if fromEnum c > 0xFFFF then 2 else 1) 0

-- | Completions for a line typed up to the cursor: after `@` or `@prefix`,
-- the project's files of a known language; after `@path:prefix`, the
-- units of that file.
anchorItems :: ServerState -> Project -> Text -> IO [CompletionItem]
anchorItems s p upTo = case T.breakOnEnd "@" upTo of
  ("", _) -> pure []
  (_, typed)
    | T.any (== ' ') typed -> pure []
    | (path, unitPrefix) <- T.breakOnEnd ":" typed, not (T.null path) -> do
        units <- case confined (T.unpack (T.dropEnd 1 path)) of
          Nothing -> pure []
          Just rel | Nothing <- languageFor langs rel -> pure []
          Just rel | Just lang <- languageFor langs rel -> safely [] $ readText ws rel >>= \case
            Nothing -> pure []
            Just t -> either (const []) id <$> unitsOf (pSyntax (ssPorts s)) lang t
        pure [item CompletionItemKind_Function (namedName n) (namedName n) | n <- units, unitPrefix `T.isPrefixOf` namedName n]
    | otherwise -> do
        files <- safely [] (listFiles ws ".")
        pure [item CompletionItemKind_File (T.pack f) (T.pack f <> ":") | f <- files, isJust (languageFor langs f), T.unpack typed `isPrefixOf` f]
  where
    ws = prWorkspace p
    langs = catalogLanguages (prCatalog p)
    item kind label insert = CompletionItem label Nothing (Just kind) Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just insert) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | A plan's anchored line: its section, row (0-based) and text; its file
-- (absolute) when that exists, and its unit's definition line (1-based)
-- when found.
data FileItem = FileItem {fiSection :: Text, fiRow :: Int, fiText :: Text, fiAbs :: Maybe FilePath, fiLine :: Maybe Int}
  deriving stock (Eq, Show)

-- | One item per anchored line of a plan, in plan order.
planFiles :: ServerState -> Project -> Text -> IO [FileItem]
planFiles s p text = do
  let ls = parsePlan text
      anchored = [(l, a) | l <- ls, Just a <- [lnAnchor l]]
  resolved <- resolveAll s p (map snd anchored)
  pure
    [ FileItem (secTitle (sectionOf ls (lnRow l))) (lnRow l) (T.strip (lnText l)) (rsAbs r <$ rsText r) (namedDef <$> rsUnit r)
    | (l, a) <- anchored
    , let r = fromMaybe (Resolved (anPath a) (anPath a) Nothing Nothing Nothing) (M.lookup (key a) resolved)
    ]
