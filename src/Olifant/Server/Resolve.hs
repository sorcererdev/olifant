-- | Internal to Olifant.Server: which unit a plan line is about. The
-- project's units profiled by their words (cached per file while its text
-- is unchanged), the lexical candidates for a line, and Jev's pick among
-- them, widened by the engine when it is unsure (headers, then code, then
-- the plan around the line).
module Olifant.Server.Resolve
  ( UnitRef (..)
  , refText
  , UnitProfile (..)
  , profileOf
  , lineTokens
  , identifiers
  , projectIndex
  , candidates
  ) where

import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.Dynamic (fromDynamic, toDyn)
import Data.IORef (modifyIORef', readIORef)
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..), catalogIgnores)
import Olifant.Language.Adapter (Language (..), languageFor)
import Olifant.Language.Test (isTestFile)
import Olifant.Server.Confine (confined, safely)
import Olifant.Server.State (ServerState (..))
import Olifant.Server.Unit (Named (..), unitsOf)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (splitDirectories, takeBaseName, takeDirectory, (</>))

-- | A unit by its file (relative to the root) and its qualified name.
data UnitRef = UnitRef {urPath :: FilePath, urName :: Text}
  deriving stock (Eq, Ord, Show)

-- | `path:Name`, as an anchor writes it.
refText :: UnitRef -> Text
refText r = T.pack (urPath r) <> ":" <> urName r

-- | A unit as the resolver sees it: its header line, its first 12 lines
-- from the header, its whole text, whether it is in a test file, and the
-- words of each (the name's words hold the file stem's too).
data UnitProfile = UnitProfile
  { upRef :: UnitRef
  , upHeader :: Text
  , upHead :: [Text]
  , upBody :: Text
  , upTest :: Bool
  , upNameToks :: Set Text
  , upHeaderToks :: Set Text
  , upPathToks :: Set Text
  , upBodyToks :: Set Text
  }
  deriving stock (Show)

-- | A unit of a file (relative path, its text, whether it is a test file).
profileOf :: FilePath -> Text -> Bool -> Named -> UnitProfile
profileOf rel text isTest n =
  UnitProfile
    { upRef = UnitRef rel (namedName n)
    , upHeader = T.strip header
    , upHead = take 12 (drop (d - 1) span')
    , upBody = T.intercalate "\n" span'
    , upTest = isTest
    , upNameToks = S.fromList (lineTokens (namedName n) ++ lineTokens (T.pack (takeBaseName rel)))
    , upHeaderToks = S.fromList (lineTokens header)
    , upPathToks = S.fromList (concatMap (lineTokens . T.pack) (filter (/= ".") (splitDirectories (takeDirectory rel))))
    , upBodyToks = S.fromList (lineTokens (T.intercalate "\n" span'))
    }
  where
    (a, z) = namedLines n
    d = namedDef n - a + 1
    span' = take (z - a + 1) (drop (a - 1) (T.splitOn "\n" text))
    header = case drop (d - 1) span' of
      h : _ -> h
      [] -> ""

-- | A line's words: split on anything not a letter or digit, then at camel
-- humps and between letters and digits; lower-cased, digits kept, stop
-- words out.
lineTokens :: Text -> [Text]
lineTokens = filter (`S.notMember` stopWords) . map T.toLower . concatMap humps . filter (not . T.null) . T.split (not . isAlphaNum)

-- | A word in its camel and digit parts: `Comparison2018Admin` is
-- Comparison, 2018, Admin; `HTTPServer` is HTTP, Server.
humps :: Text -> [Text]
humps = map T.pack . go . T.unpack
  where
    go [] = []
    go s@(c : _) =
      let (run, rest) = runOf c s in run : go rest
    runOf c s
      | isDigit c = span isDigit s
      | isUpper c = case span isUpper s of
          -- an acronym before a word: its last capital starts the word
          (ups@(_ : _ : _), r@(l : _)) | isLower l -> (init ups, last ups : r)
          (ups, r) -> let (ls, r') = span isLower r in (ups ++ ls, r')
      | otherwise = span isLower s

stopWords :: Set Text
stopWords = S.fromList (T.words "a an the of to in on at by for from with as is are be it its this that these those and or not no so than then into when which only can each per has have was were will should would there their they")

-- | The identifiers a line names: backticked words, and bare words that are
-- snake_case, CamelCase or hold a digit; case kept, in order, once each.
identifiers :: Text -> [Text]
identifiers line = nub (filter (not . T.null) (map trim (ticked ++ bare)))
  where
    pieces = T.splitOn "`" line
    ticked = [p | (i, p) <- zip [0 :: Int ..] pieces, odd i, i < length pieces - 1]
    outside = T.unwords [p | (i, p) <- zip [0 :: Int ..] pieces, even i || i == length pieces - 1]
    bare = filter isIdent (map trim (T.split isSpace outside))
    trim = T.dropAround (\c -> not (isAlphaNum c || c == '_')) . T.strip . fst . T.breakOn "("
    isIdent w =
      T.all (\c -> isAlphaNum c || c == '_') w
        && (T.any (== '_') w || T.any isDigit w || camel w)
    camel w = T.any isLower w && T.any isUpper (T.drop 1 w)

-- | Every unit of every file under the project's root that has a language,
-- skipping dot-folders, the languages' ignores and paths outside the root.
-- A file's profiles are kept while its text is unchanged; one that fails to
-- parse gives none.
projectIndex :: ServerState -> Project -> IO [UnitProfile]
projectIndex s p = do
  files <- filter keep <$> safely [] (listFilesSkipping ws (catalogIgnores cat) ".")
  concat <$> mapM indexed files
  where
    ws = prWorkspace p
    cat = prCatalog p
    langs = catalogLanguages cat
    keep f = isJust (confined f) && not (any (\d -> "." `T.isPrefixOf` T.pack d && d /= ".") (splitDirectories f)) && isJust (languageFor langs f)
    indexed f = case languageFor langs f of
      Nothing -> pure []
      Just lang -> safely [] $ readText ws f >>= \case
        Nothing -> pure []
        Just text -> do
          let key = normalisePath (prRoot p </> f)
          known <- M.lookup key <$> readIORef (ssIndexCache s)
          case known of
            Just (t, dyn) | t == text, Just ps <- fromDynamic dyn -> pure ps
            _ -> do
              units <- either (const []) id <$> safely (Right []) (unitsOf (pSyntax (ssPorts s)) lang (Just f) text)
              let isTest = maybe False (`isTestFile` f) (langTest lang)
                  ps = map (profileOf f text isTest) units
              modifyIORef' (ssIndexCache s) (M.insert key (text, toDyn ps))
              pure ps

-- | The top 8 units for a line, by score (none at zero): the line's words
-- against each unit's name and file stem (3 each), header and path (1.5
-- each), body (0.3 each, at most 1.5); 6 more when an identifier the line
-- names is the unit's name or its file stem; a test unit's score times 0.3
-- unless the line says test or stub. Ties: the shorter name, then the path.
candidates :: Text -> [UnitProfile] -> [(UnitProfile, Double)]
candidates line units = take 8 (sortOn order [(u, sc) | u <- units, let sc = scoreOf u, sc > 0])
  where
    toks = S.fromList (lineTokens line)
    idents = identifiers line
    saysTest = any (\t -> "test" `T.isPrefixOf` t || "stub" `T.isPrefixOf` t) (S.toList toks)
    hits set = fromIntegral (S.size (S.intersection toks set)) :: Double
    scoreOf u =
      let ref = upRef u
          lastPart = T.takeWhileEnd (/= '.') (urName ref)
          stem = T.pack (takeBaseName (urPath ref))
          named = any (`elem` [lastPart, urName ref, stem]) idents
          raw =
            3.0 * hits (upNameToks u)
              + 1.5 * hits (S.union (upHeaderToks u) (upPathToks u))
              + min 1.5 (0.3 * hits (upBodyToks u))
              + (if named then 6 else 0)
       in if upTest u && not saysTest then raw * 0.3 else raw
    order (u, sc) = (Down sc, T.length (urName (upRef u)), urPath (upRef u))
