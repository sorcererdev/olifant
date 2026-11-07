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
  , Round (..)
  , roundName
  , LineContext (..)
  , lineContext
  , bareLine
  , unitQuestion
  , Resolution (..)
  , resolveLine
  ) where

import Data.Maybe (listToMaybe)
import Olifant.Decide (Answer (..), Confidence (..), Decider (..), Question, choice, qKind, qOptions)
import Olifant.Plans (Anchor (..), Kind (..), Line (..), Section (..), sectionOf)
import Olifant.Server.LoopQuestions (accepted)
import Olifant.Server.Telemetry (Decision (..))
import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.Dynamic (fromDynamic, toDyn)
import Data.IORef (modifyIORef', readIORef)
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust)
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
          (ups, r) -> let (ls, r') = span lowerish r in (ups ++ ls, r')
      | otherwise = span lowerish s
    -- anything that is not a capital or an ASCII digit continues a word: a
    -- letter with no case or a digit outside ASCII never ends the run
    lowerish x = not (isUpper x || isDigit x)

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

-- | The rounds Jev is asked in, each widening the last: the candidates'
-- headers, their code, and the plan around the line.
data Round = Headers | Code | Around
  deriving stock (Eq, Show)

-- | A round's name in the record: `jev-1`, `jev-2`, `jev-3`.
roundName :: Round -> Text
roundName = \case
  Headers -> "jev-1"
  Code -> "jev-2"
  Around -> "jev-3"

-- | What a line is asked with: its words, the question it sits under, its
-- section's title, and the list lines around it.
data LineContext = LineContext {lcLine :: Text, lcQuestion :: Maybe Text, lcSection :: Text, lcAround :: Text}
  deriving stock (Eq, Show)

-- | A line's words: without its indentation, its `- `, and its anchor and
-- verdict.
bareLine :: Line -> Text
bareLine l = T.strip (dash (maybe (lnText l) (\a -> T.take (anStart a) (lnText l)) (lnAnchor l)))
  where
    dash t = let s = T.stripStart t in fromMaybe s (T.stripPrefix "- " s)

-- | A line's context in its plan: its words, the `- ?` it is a child of (that
-- question's words without the `? `), its section's title, and the three list
-- lines either side, each as bare, joined by newlines.
lineContext :: [Line] -> Line -> LineContext
lineContext ls l = LineContext (bareLine l) question (secTitle (sectionOf ls (lnRow l))) around
  where
    parent = listToMaybe [p | p <- reverse (take (lnRow l) ls), lnIndent p < lnIndent l, isList p]
    question = case parent of
      Just p | lnKind p == Question -> Just (T.strip (fromMaybe (bareLine p) (T.stripPrefix "? " (bareLine p))))
      _ -> Nothing
    listed = [x | x <- ls, isList x]
    before = reverse (take 3 (reverse [x | x <- listed, lnRow x < lnRow l]))
    after = take 3 [x | x <- listed, lnRow x > lnRow l]
    around = T.intercalate "\n" (map bareLine (before ++ after))
    isList x = case lnKind x of
      Heading _ -> False
      Prose -> False
      _ -> True

-- | Which of the candidates a line is about, in a round: each option is a
-- unit (`path:Name`) described by its header, or by its first lines in the
-- wider rounds; the last round adds the plan around the line.
unitQuestion :: Round -> LineContext -> [UnitProfile] -> Question
unitQuestion r c units = choice kind "Which unit would a reader open to check this plan line? Choose the unit the line is about." options context
  where
    kind = case r of
      Headers -> "plan-unit"
      Code -> "plan-unit-code"
      Around -> "plan-unit-around"
    options = [(refText (upRef u), describe u) | u <- units]
    describe u = case r of
      Headers -> upHeader u
      _ -> T.intercalate "\n" (upHead u)
    context =
      [("line", lcLine c), ("section", lcSection c)]
        ++ [("question", q) | Just q <- [lcQuestion c]]
        ++ [("around", lcAround c) | r == Around]

-- | A line resolved to a unit (in a round, at Jev's confidence), or not
-- (with its candidates, best first, and the top probability Jev gave in its
-- last round; 0 when it was not asked).
data Resolution = Resolved UnitRef Round Double | Unresolved [UnitRef] Double
  deriving stock (Eq, Show)

-- | Jev's pick among a line's candidates (best first, at most 8): headers,
-- then code, then the plan around, stopping at the first answer at the
-- gate; budget is asked before each call. No candidate: no call.
resolveLine :: Decider IO -> IO Bool -> LineContext -> [UnitProfile] -> IO (Resolution, [Decision])
resolveLine d budget c units0 = go [Headers, Code, Around] 0 []
  where
    units = take 8 units0
    refs = map upRef units
    go [] top ds = pure (Unresolved refs top, reverse ds)
    go _ top ds | null units = pure (Unresolved refs top, reverse ds)
    go (r : rest) top ds =
      budget >>= \case
        False -> pure (Unresolved refs top, reverse ds)
        True -> do
          let q = unitQuestion r c units
          (a, ps) <- weigh d q
          let dc = Decision (qKind q) (qOptions q) (chosen a) (confOf a) (roundName r)
              top' = case [p | (o, p) <- ps, o /= "unsure"] of
                p : _ -> p
                [] -> fromMaybe 0 (confOf a)
          case a of
            Chosen pick _ _
              | accepted q a, Just ref <- lookup pick [(refText u, u) | u <- refs] ->
                  pure (Resolved ref r (fromMaybe top' (confOf a)), reverse (dc : ds))
            _ -> go rest top' (dc : ds)
    chosen = \case
      Chosen x _ _ -> Just x
      _ -> Nothing
    confOf = \case
      Chosen _ _ (Confidence x) -> Just x
      _ -> Nothing
