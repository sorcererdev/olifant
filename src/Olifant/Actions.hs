-- | Code actions (spec §4.6): which generators apply at a cursor, with the
-- inputs the code there already says. An editor shows these as actions and
-- asks only for what is missing.
module Olifant.Actions
  ( Action (..)
  , Range (..)
  , FileAction (..)
  , Nearby (..)
  , matchPathSlots
  , actionsAt
  , actionsInFile
  ) where

import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe)
import Data.Text (Text)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime (..), fromGregorian)
import Olifant.Generator
import Olifant.Language
import Olifant.Query (Queries)
import Olifant.Template (Value (..), render)
import Text.Regex.TDFA (AllTextSubmatches (..), Regex, makeRegexM, matchM)

-- | A generator offered at a cursor: its id, the label to show, the inputs
-- bound from the path and the code, and the required inputs still missing.
data Action = Action {actGenerator :: Text, actLabel :: Text, actSets :: [(Text, Text)], actMissing :: [Text]}
  deriving stock (Eq, Show)

-- | A path pattern against a project-relative path: `{name}` matches one
-- segment (or the part of one between literals) and binds it; `*` matches
-- within a segment; `**` across segments.
matchPathSlots :: Text -> FilePath -> Maybe [(Text, Text)]
matchPathSlots pattern path = do
  rx <- makeRegexM (T.unpack ("^" <> regex <> "$")) :: Maybe Regex
  groups <- getAllTextSubmatches <$> (matchM rx path :: Maybe (AllTextSubmatches [] String))
  pure (zip names (map T.pack (drop 1 groups)))
  where
    (regex, names) = go pattern
    go t = case T.uncons t of
      Nothing -> ("", [])
      Just ('{', rest) ->
        let (name, after) = T.breakOn "}" rest
            (r, ns) = go (T.drop 1 after)
         in ("([^/]+)" <> r, name : ns)
      Just ('*', rest)
        | Just rest' <- T.stripPrefix "*" rest -> let (r, ns) = go rest' in (".*" <> r, ns)
        | otherwise -> let (r, ns) = go rest in ("[^/]*" <> r, ns)
      Just (c, rest) ->
        let (r, ns) = go rest
         in ((if c `elem` ("\\^$.|?+()[]" :: String) then T.pack ['\\', c] else T.singleton c) <> r, ns)

-- | A range in a file: 1-based lines, UTF-8 byte columns; the end is just
-- past the node.
data Range = Range {rangeStart :: (Int, Int), rangeEnd :: (Int, Int)}
  deriving stock (Eq, Show)

-- | An action at a place in a file; no range: the whole file.
data FileAction = FileAction {faAction :: Action, faRange :: Maybe Range}
  deriving stock (Eq, Show)

-- | A generator whose path matched but whose rule found nothing here, and
-- what it looks for.
data Nearby = Nearby {nbGenerator :: Text, nbLabel :: Text, nbWhy :: Text}
  deriving stock (Eq, Show)

-- | How one generator relates to a file.
data Candidate = Whole | Nodes [Match] | Near Text

-- | Every generator whose `applies_at` matches @path@ at the 1-based
-- (line, column), given the file's (possibly unsaved) text.
actionsAt :: Monad m => Syntax m -> [Language] -> Queries m -> [FilePath] -> (FilePath -> m (Maybe Text)) -> [(Text, GeneratorWith s)] -> FilePath -> (Int, Int) -> Text -> m [Action]
actionsAt syntax langs queries paths readFile' gens path (line, col) content =
  fmap catMaybes . traverse one =<< candidates syntax langs gens path content
  where
    -- COL is a UTF-8 byte column (Neovim's, LSP's utf-8); ast-grep's columns
    -- are characters, so the cursor is converted on its own line.
    cursor = (line - 1, charColumn (drop (line - 1) (T.lines content)))
    charColumn = \case
      l : _ -> T.length (TE.decodeUtf8Lenient (BS.take (col - 1) (TE.encodeUtf8 l)))
      [] -> col - 1
    one (gid, gen, ap, fromPath, cand) = case cand of
      Whole -> Just <$> offer queries paths readFile' path gid gen ap fromPath Nothing
      Nodes ms
        | node : _ <- sortOn size [m | m <- ms, matchStart m <= cursor, cursor <= matchEnd m] ->
            Just <$> offer queries paths readFile' path gid gen ap fromPath (Just node)
      _ -> pure Nothing
    size m = (fst (matchEnd m) - fst (matchStart m), snd (matchEnd m) - snd (matchStart m))

-- | Every place in the file where a generator applies (in file order), and
-- the generators whose path matched but whose rule found nothing.
actionsInFile :: Monad m => Syntax m -> [Language] -> Queries m -> [FilePath] -> (FilePath -> m (Maybe Text)) -> [(Text, GeneratorWith s)] -> FilePath -> Text -> m ([FileAction], [Nearby])
actionsInFile syntax langs queries paths readFile' gens path content = do
  cands <- candidates syntax langs gens path content
  results <- traverse one cands
  pure (sortOn (fmap rangeStart . faRange) (concat [as | Left as <- results]), [n | Right n <- results])
  where
    ls = T.lines content
    one (gid, gen, ap, fromPath, cand) = case cand of
      Whole -> Left . pure . (`FileAction` Nothing) <$> offer queries paths readFile' path gid gen ap fromPath Nothing
      Nodes ms -> Left <$> traverse (\m -> (`FileAction` Just (range m)) <$> offer queries paths readFile' path gid gen ap fromPath (Just m)) ms
      Near why -> pure (Right (Nearby gid (labelFor gen ap (M.fromList fromPath)) why))
    range m = Range (pos (matchStart m)) (pos (matchEnd m))
    pos (l, c) = (l + 1, 1 + BS.length (TE.encodeUtf8 (T.take c (maybe "" id (listToMaybe (drop l ls))))))

-- | Each generator whose path matches, with what its rule finds in the text.
candidates :: Monad m => Syntax m -> [Language] -> [(Text, GeneratorWith s)] -> FilePath -> Text -> m [(Text, GeneratorWith s, AppliesAt, [(Text, Text)], Candidate)]
candidates syntax langs gens path content = catMaybes <$> traverse one gens
  where
    one (gid, gen) = case genAppliesAt gen of
      Nothing -> pure Nothing
      Just ap -> case matchPathSlots (apPath ap) path of
        Nothing -> pure Nothing
        Just fromPath -> fmap (\c -> Just (gid, gen, ap, fromPath, c)) $ case apNode ap of
          Nothing -> pure Whole
          -- The rule may use the path's slots ({= command|pascal =}).
          Just rule -> case (languageFor langs path, renderRule (M.fromList [(k, VText v) | (k, v) <- fromPath]) rule) of
            (Just lang, Right r) ->
              querySyntax syntax lang r content >>= \case
                Right ms@(_ : _) -> pure (Nodes ms)
                _ -> pure (Near "no matching node in this file")
            _ -> pure (Near "no matching node in this file")

-- | The label, rendered with what is bound; the description when it needs more.
labelFor :: GeneratorWith s -> AppliesAt -> M.Map Text Text -> Text
labelFor gen ap sets = fromMaybe (genDescription gen) (either (const Nothing) Just . render (M.map VText sets) =<< apLabel ap)

-- | The action for a generator, with inputs from the path, the node and the
-- file; bound values that are not valid choices are asked for instead.
offer :: Monad m => Queries m -> [FilePath] -> (FilePath -> m (Maybe Text)) -> FilePath -> Text -> GeneratorWith s -> AppliesAt -> [(Text, Text)] -> Maybe Match -> m Action
offer queries paths readFile' path gid gen ap fromPath node = do
  let fromNode = [(k, v) | Just n <- [node], (k, re) <- apBind ap, Just v <- [capture re (matchText n)]]
      sets = M.fromList (fromPath ++ fromNode ++ [(k, T.pack path) | Just k <- [apFile ap]])
  resolved <- withChoices queries paths readFile' sets epoch gen
  let valid k v = case resolved of
        Right g | [ScalarInput (EnumOf _ _ (Resolved cs _))] <- [inputType s | s <- genInputs g, inputName s == k] -> v `elem` cs
        Right g | [ScalarInput (EnumS cs)] <- [inputType s | s <- genInputs g, inputName s == k] -> v `elem` cs
        _ -> True
      kept = M.filterWithKey valid sets
      required = [inputName s | s <- genInputs gen, not (inputOptional s), isNothing (inputDefault s)]
      unbound = [n | n <- required, M.notMember n kept]
  pure (Action gid (labelFor gen ap kept) (M.toAscList kept) unbound)
  where
    capture re t = do
      rx <- makeRegexM (T.unpack re) :: Maybe Regex
      groups <- getAllTextSubmatches <$> (matchM rx (T.unpack t) :: Maybe (AllTextSubmatches [] String))
      case drop 1 groups of
        (g : _) | not (null g) -> Just (T.pack g)
        _ -> Nothing
    epoch = UTCTime (fromGregorian 2026 1 1) 0
