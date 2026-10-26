-- | Internal to Olifant.Server: the `do` review (plans UI redesign, A and
-- D approach 1). Before a `do` the engine takes a reference to the working
-- tree (`git stash create`, else HEAD, which change nothing; the untracked
-- files listed, their blobs written so a change to one can be undone);
-- after it, the change is the diff against that reference plus the new
-- untracked files. A review keeps what the `do` did, said, ran and was
-- refused, under `.olifant/state/reviews/<plan>/<id>.json`, until it is
-- answered: kept, or reverted from the reference.
module Olifant.Server.Review
  ( TreeRef (..)
  , takeRef
  , Change (..)
  , FileChange (..)
  , changeSince
  , revertChange
  , Denial (..)
  , denialsIn
  , deniedWords
  , allowPattern
  , allowInConfig
  , ReviewTest (..)
  , testsOfFacts
  , Review (..)
  , reviewsDir
  , reviewPath
  , saveReview
  , loadReview
  , loadReviews
  , dropReview
  , reviewRecord
  , parseReview
  , stepKey
  , locateStep
  , keepLines
  , reviewPayload
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, void, when)
import Data.Aeson (Value (..), decodeStrict, encode, object, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAlphaNum, isAscii)
import Data.List (find, nub, sortOn)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Olifant.App (Ports (..))
import Olifant.Plans (Anchor (..), Kind (..), Line (..), TestState (..), parsePlan, testState)
import Olifant.Process (runBytes)
import Olifant.Server.Edits (unifiedDiff)
import Olifant.Server.Errand (Denial (..), denialsIn)
import Olifant.Workspace (Workspace (..))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Directory qualified as Dir
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, takeExtension, (</>))

-- | The tree before a `do`: a commit holding the tracked files as they were
-- (the index and the working tree; HEAD when nothing differed), and each
-- untracked file with its blob (empty when it was not written).
data TreeRef = TreeRef {trCommit :: Text, trUntracked :: [(FilePath, Text)]}
  deriving stock (Eq, Show)

-- | At most this many untracked files have their blobs written.
untrackedLimit :: Int
untrackedLimit = 2000

-- | A git command's output (it may fail: Nothing).
git :: FilePath -> [String] -> BS.ByteString -> IO (Maybe BS.ByteString)
git root args input =
  (try (runBytes "git" (["-C", root] ++ args) (Just root) input) :: IO (Either SomeException (Either Text (ExitCode, BS.ByteString, BS.ByteString)))) >>= \case
    Right (Right (ExitSuccess, out, _)) -> pure (Just out)
    _ -> pure Nothing

-- | NUL-separated paths.
zpaths :: BS.ByteString -> [FilePath]
zpaths = map (T.unpack . TE.decodeUtf8Lenient) . filter (not . BS.null) . BS.split 0

-- | Olifant's own state and the project's config (`p` writes it) are never
-- part of a change.
ours :: FilePath -> Bool
ours f = ".olifant/state/" `T.isPrefixOf` T.pack f || f == ".olifant/config.toml"

-- | The tree's reference at ROOT; Nothing when it is no repository or has
-- no commit. Only objects are written (no ref, no index, no file).
takeRef :: FilePath -> IO (Maybe TreeRef)
takeRef root = do
  stash <- git root ["stash", "create"] ""
  commit <- case firstLine =<< stash of
    Just c -> pure (Just c)
    Nothing -> (firstLine =<<) <$> git root ["rev-parse", "--verify", "-q", "HEAD"] ""
  case commit of
    Nothing -> pure Nothing
    Just c -> do
      untracked <- filter (not . ours) . maybe [] zpaths <$> git root ["ls-files", "-z", "--others", "--exclude-standard"] ""
      blobs <- hashed True root (take untrackedLimit untracked)
      pure (Just (TreeRef c (zip untracked (blobs ++ repeat ""))))
  where
    firstLine out = case T.lines (TE.decodeUtf8Lenient out) of
      l : _ | not (T.null (T.strip l)) -> Just (T.strip l)
      _ -> Nothing

-- | Each file's blob id (written to the object store when WRITE).
hashed :: Bool -> FilePath -> [FilePath] -> IO [Text]
hashed _ _ [] = pure []
hashed write root fs =
  git root (["hash-object"] ++ ["-w" | write] ++ ["--stdin-paths"]) (TE.encodeUtf8 (T.unlines (map T.pack fs))) <&&> \case
    Just out | let ls = T.lines (TE.decodeUtf8Lenient out), length ls == length fs -> ls
    _ -> map (const "") fs
  where
    io <&&> f = f <$> io

data Change = Modified | Added | Deleted
  deriving stock (Eq, Show)

-- | One file a `do` changed (root-relative), its text before and after
-- (Nothing: absent, or not text) and its unified diff.
data FileChange = FileChange {fcPath :: FilePath, fcChange :: Change, fcBefore :: Maybe Text, fcAfter :: Maybe Text, fcDiff :: Text}
  deriving stock (Eq, Show)

-- | The change at ROOT since REF, but the files EXCLUDED (root-relative:
-- the plan) and Olifant's state: tracked files that differ from the
-- reference, new untracked files, and untracked ones that changed or went.
changeSince :: FilePath -> [FilePath] -> TreeRef -> IO [FileChange]
changeSince root excluded ref = do
  let commit = T.unpack (trCommit ref)
      keep f = f `notElem` excluded && not (ours f)
  status <- maybe [] statusOf <$> git root ["diff", "--name-status", "-z", "--no-renames", commit] ""
  tracked <- forM [(c, f) | (c, f) <- status, keep f] $ \(c, f) -> do
    before <- if c == Added then pure Nothing else textOf <$> git root ["show", commit <> ":" <> f] ""
    after <- if c == Deleted then pure Nothing else readMaybe (root </> f)
    pure (FileChange f c before after (diffOf f before after))
  now <- filter keep . maybe [] zpaths <$> git root ["ls-files", "-z", "--others", "--exclude-standard"] ""
  let was = trUntracked ref
      fresh = [f | f <- now, isNothing (lookup f was)]
      kept = [(f, b) | (f, b) <- was, f `elem` now, b /= ""]
      gone = [(f, b) | (f, b) <- was, keep f, f `notElem` now, b /= ""]
  added <- forM fresh $ \f -> readMaybe (root </> f) <&> \after -> FileChange f Added Nothing after (diffOf f Nothing after)
  nowBlobs <- hashed False root (map fst kept)
  changed <- forM [(f, b) | ((f, b), b') <- zip kept nowBlobs, b' /= "", b' /= b] $ \(f, b) -> do
    before <- textOf <$> git root ["cat-file", "blob", T.unpack b] ""
    after <- readMaybe (root </> f)
    pure (FileChange f Modified before after (diffOf f before after))
  removed <- forM gone $ \(f, b) -> do
    before <- textOf <$> git root ["cat-file", "blob", T.unpack b] ""
    pure (FileChange f Deleted before Nothing (diffOf f before Nothing))
  pure (sortOn fcPath (tracked ++ added ++ changed ++ removed))
  where
    io <&> f = f <$> io
    textOf = (>>= \b -> either (const Nothing) Just (TE.decodeUtf8' b))
    readMaybe p = (try (BS.readFile p) :: IO (Either SomeException BS.ByteString)) <&> either (const Nothing) (textOf . Just)
    statusOf out = go (zpaths out)
      where
        go (s : f : rest) = (case take 1 s of "A" -> Added; "D" -> Deleted; _ -> Modified, f) : go rest
        go _ = []
    diffOf f before after
      | isNothing before && isNothing after = "Binary file " <> T.pack f <> " changed\n"
      | otherwise = unifiedDiff f (fromMaybe "" before) (fromMaybe "" after)

-- | Undo CHANGES at ROOT from REF: tracked files restored in the working
-- tree only (the index is left alone), new files removed, untracked ones
-- written back from their blobs. Left names what could not be restored.
revertChange :: FilePath -> TreeRef -> [FileChange] -> IO (Either Text ())
revertChange root ref changes = do
  let untracked = trUntracked ref
      isUntracked f = isJust (lookup f untracked)
      tracked = [fcPath c | c <- changes, not (isUntracked (fcPath c)), not (fcChange c == Added && not (inRef c))]
      inRef c = fcChange c /= Added
      newFiles = [fcPath c | c <- changes, fcChange c == Added]
  restored <- if null tracked then pure (Just "") else git root (["restore", "--source=" <> T.unpack (trCommit ref), "--worktree", "--"] ++ tracked) ""
  -- a new file: tracked ones added to the index stay there (its own act), the file goes
  removedOk <- forM newFiles $ \f -> (try (do e <- doesFileExist (root </> f); when e (Dir.removeFile (root </> f))) :: IO (Either SomeException ())) <&> either (const [f]) (const [])
  written <- forM [(fcPath c, b) | c <- changes, fcChange c /= Added, Just b <- [lookup (fcPath c) untracked], b /= ""] $ \(f, b) ->
    git root ["cat-file", "blob", T.unpack b] "" >>= \case
      Nothing -> pure [f]
      Just bytes -> (try (BS.writeFile (root </> f) bytes) :: IO (Either SomeException ())) <&> either (const [f]) (const [])
  let failed = [T.pack f | isNothing restored, f <- tracked] ++ map T.pack (concat removedOk ++ concat written)
  pure (if null failed then Right () else Left ("could not restore " <> T.intercalate ", " failed))
  where
    io <&> f = f <$> io

-- | The denials in words, each distinct command once with how often it was
-- tried; ALLOWED the commands a `do` may run (Bash patterns), said after.
deniedWords :: [Denial] -> [Text] -> [Text]
deniedWords ds allowed =
  [ "tried `" <> shown d <> "`" <> times (length (filter (== d) ds)) <> "; not allowed."
  | d <- nub ds ]
    ++ ["Allowed: " <> T.intercalate ", " ["`" <> a <> "`" | a <- map pattern allowed] <> "." | not (null ds), not (null allowed)]
  where
    shown d = if dnTool d == "Bash" then dnInput d else dnTool d <> (if T.null (dnInput d) then "" else " " <> dnInput d)
    times n = if n == 1 then "" else " " <> T.pack (show n) <> " times"
    pattern a = case T.stripPrefix "Bash(" a >>= T.stripSuffix ")" of
      Just p -> maybe p (<> " …") (T.stripSuffix "*" p)
      Nothing -> a

-- | What `p` allows for a denial: a Bash command's first two words (three
-- when the second is a flag: `python -m pytest`) as a prefix pattern; any
-- other tool by its name.
allowPattern :: Denial -> Text
allowPattern d
  | dnTool d == "Bash" =
      let ws = T.words (dnInput d)
          n = case ws of
            (_ : f : _ : _) | "-" `T.isPrefixOf` f -> 3
            _ -> 2
       in "Bash(" <> T.unwords (take n ws) <> "*)"
  | otherwise = dnTool d

-- | A config's text with PATTERNS added to `[plans] do_tools` (the ones not
-- there yet): the list rewritten on one line where it was, else made under
-- `[plans]`, else a `[plans]` table appended.
allowInConfig :: [Text] -> Text -> Text
allowInConfig patterns txt = case break (isTable "plans") ls of
  (_, []) -> T.stripEnd txt <> (if T.null (T.strip txt) then "" else "\n\n") <> "[plans]\n" <> listLine [] <> "\n"
  (pre, header : rest) ->
    let (table, after) = break isAnyTable rest
     in case break isDoTools table of
          (tpre, []) -> T.unlines (pre ++ [header, listLine []] ++ tpre ++ after)
          (tpre, start : more) ->
            let (span', rest') = closing (start : more)
                old = quoted (T.unlines span')
             in T.unlines (pre ++ [header] ++ tpre ++ [listLine old] ++ rest' ++ after)
  where
    ls = T.lines txt
    isTable name l = T.strip l == "[" <> name <> "]"
    isAnyTable l = "[" `T.isPrefixOf` T.strip l
    isDoTools l = case T.stripPrefix "do_tools" (T.strip l) of
      Just r -> "=" `T.isPrefixOf` T.stripStart r
      Nothing -> False
    -- the key's lines up to the one that closes its list
    closing xs =
      let go n (y : ys) acc
            | n + depth y <= 0 && T.any (== '[') (T.concat (reverse (y : acc))) = (reverse (y : acc), ys)
            | otherwise = go (n + depth y) ys (y : acc)
          go _ [] acc = (reverse acc, [])
       in go (0 :: Int) xs []
    depth l = T.count "[" (unquoted l) - T.count "]" (unquoted l)
    unquoted l = T.concat [p | (i, p) <- zip [0 :: Int ..] (T.splitOn "\"" l), even i]
    quoted t = [p | (i, p) <- zip [0 :: Int ..] (T.splitOn "\"" t), odd i]
    listLine old = "do_tools = [" <> T.intercalate ", " ["\"" <> escape p <> "\"" | p <- old ++ [p | p <- nub patterns, p `notElem` old]] <> "]"
    escape = T.replace "\"" "\\\"" . T.replace "\\" "\\\\"

-- | A test of the step as the review shows it: its name and `passed`,
-- `failed` or `not run`.
data ReviewTest = ReviewTest {rtName :: Text, rtState :: Text}
  deriving stock (Eq, Show)

-- | The step's tests as the facts of a run left them (the test children
-- rewritten, each with its mark).
testsOfFacts :: [Text] -> [ReviewTest]
testsOfFacts texts =
  [ ReviewTest (maybe (T.strip t) anUnit (lnAnchor l)) (case testState l of Just TestGreen -> "passed"; Just TestRed -> "failed"; _ -> "not run")
  | t <- texts, l <- take 1 (parsePlan t), lnKind l == TestChild ]

-- | One `do`, kept until it is answered. 'rvLines' are the plan lines it
-- proposes (its first a step line with the anchor added, for an unanchored
-- step); they land with `keep`. 'rvFacts' are the facts that landed with
-- its answer (row, the line before, after): `revert` puts the lines back.
data Review = Review
  { rvId :: Text
  , rvErrand :: Text
  , rvPlan :: FilePath
  , rvRow :: Int
  , rvStep :: Text
  , rvAt :: UTCTime
  , rvModel :: Text
  , rvSeconds :: Double
  , rvCost :: Maybe Double
  , rvStatus :: Text
  , rvWhy :: Maybe Text
  , rvSaid :: [Text]
  , rvLines :: [Text]
  , rvFacts :: [(Int, Text, Text)]
  , rvTests :: [ReviewTest]
  , rvDenials :: [Denial]
  , rvTools :: [Text]
  , rvAllowed :: [Text]
  , rvRef :: Maybe TreeRef
  , rvPrompt :: Text
  , rvAnswer :: Text
  , rvSession :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | Where a plan's reviews live (PLAN root-relative, without its `.md`).
reviewsDir :: FilePath -> FilePath -> FilePath
reviewsDir root plan = root </> ".olifant/state/reviews" </> (if takeExtension plan == ".md" then dropExtension plan else plan)

-- | A review's file: its id, made safe for a file name.
reviewPath :: FilePath -> Review -> FilePath
reviewPath root r = reviewsDir root (rvPlan r) </> safeName (rvId r) <> ".json"

safeName :: Text -> FilePath
safeName = T.unpack . T.map (\c -> if isAscii c && (isAlphaNum c || c `elem` ("-_." :: String)) then c else '-')

saveReview :: Ports -> FilePath -> Review -> IO ()
saveReview ports root r = writeText (pFiles ports) (reviewPath root r) (TE.decodeUtf8 (BL.toStrict (encode (reviewRecord r))) <> "\n")

-- | The review ID of PLAN, if it is open.
loadReview :: Ports -> FilePath -> FilePath -> Text -> IO (Maybe Review)
loadReview ports root plan rid = find ((== rid) . rvId) <$> loadReviews ports root plan

-- | A plan's open reviews, oldest first.
loadReviews :: Ports -> FilePath -> FilePath -> IO [Review]
loadReviews ports root plan = do
  let dir = reviewsDir root plan
  there <- doesDirectoryExist dir
  names <- if there then filter ((== ".json") . takeExtension) <$> listDirectory dir else pure []
  found <- forM names $ \n -> (>>= parseReview) . fmap TE.encodeUtf8 <$> readText (pFiles ports) (dir </> n)
  pure (sortOn rvAt [r | Just r <- found])

-- | The review answered: its file goes.
dropReview :: Ports -> FilePath -> Review -> IO ()
dropReview ports root r = void (try (removeFile (pFiles ports) (reviewPath root r)) :: IO (Either SomeException ()))

reviewRecord :: Review -> Value
reviewRecord r =
  object
    [ "id" .= rvId r, "errand" .= rvErrand r, "plan" .= rvPlan r, "row" .= rvRow r, "step" .= rvStep r
    , "at" .= rvAt r, "model" .= rvModel r, "seconds" .= rvSeconds r, "cost" .= rvCost r
    , "status" .= rvStatus r, "why" .= rvWhy r, "said" .= rvSaid r, "lines" .= rvLines r
    , "facts" .= [object ["row" .= n, "before" .= b, "after" .= a] | (n, b, a) <- rvFacts r]
    , "tests" .= [object ["test" .= rtName t, "status" .= rtState t] | t <- rvTests r]
    , "denials" .= [object ["tool" .= dnTool d, "input" .= dnInput d] | d <- rvDenials r]
    , "tools" .= rvTools r, "allowed" .= rvAllowed r
    , "ref" .= fmap (\t -> object ["commit" .= trCommit t, "untracked" .= [object ["file" .= f, "blob" .= b] | (f, b) <- trUntracked t]]) (rvRef r)
    , "prompt" .= rvPrompt r, "answer" .= rvAnswer r, "session" .= rvSession r
    ]

parseReview :: BS.ByteString -> Maybe Review
parseReview bytes = decodeStrict bytes >>= parseMaybe p
  where
    p :: Value -> Parser Review
    p = \case
      Object o ->
        Review <$> o .: "id" <*> o .: "errand" <*> o .: "plan" <*> o .: "row" <*> o .: "step"
          <*> o .: "at" <*> o .: "model" <*> o .: "seconds" <*> o .:? "cost"
          <*> o .: "status" <*> o .:? "why" <*> o .: "said" <*> o .: "lines"
          <*> (mapM fact =<< o .: "facts") <*> (mapM test =<< o .: "tests") <*> (mapM denial =<< o .: "denials")
          <*> o .: "tools" <*> o .: "allowed"
          <*> (traverse ref =<< o .:? "ref") <*> o .: "prompt" <*> o .: "answer" <*> o .:? "session"
      _ -> fail "a review is an object"
    fact = \case
      Object o -> (,,) <$> o .: "row" <*> o .: "before" <*> o .: "after"
      _ -> fail "fact"
    test = \case
      Object o -> ReviewTest <$> o .: "test" <*> o .: "status"
      _ -> fail "test"
    denial = \case
      Object o -> Denial <$> o .: "tool" <*> o .: "input"
      _ -> fail "denial"
    ref = \case
      Object o -> TreeRef <$> o .: "commit" <*> (mapM untracked =<< o .: "untracked")
      _ -> fail "ref"
    untracked = \case
      Object o -> (,) <$> o .: "file" <*> o .: "blob"
      _ -> fail "untracked"

-- | A step line as a review finds it again: its words past the checkbox and
-- before its anchor, and the anchor's path and unit (its marks and tick
-- change; this does not).
stepKey :: Text -> (Text, Maybe (FilePath, Text))
stepKey t = case take 1 (parsePlan t) of
  [l] ->
    let body = maybe (lnText l) (\a -> T.take (anStart a) (lnText l)) (lnAnchor l)
        words' = T.strip (foldr (\pre acc -> fromMaybe acc (T.stripPrefix pre acc)) (T.strip body) ["- [ ]", "- [x]"])
     in (words', (\a -> (anPath a, anUnit a)) <$> lnAnchor l)
  _ -> (T.strip t, Nothing)

-- | The review's step in the plan's lines now: at its row when it is still
-- there, else the first step with its key; Nothing when it is gone.
locateStep :: [Line] -> Review -> Maybe Line
locateStep ls r =
  let want = stepKey (rvStep r)
      isStep l = case lnKind l of Step _ -> True; TestChild -> True; _ -> False
      same l = isStep l && stepKey (lnText l) == want
   in case [l | l <- ls, lnRow l == rvRow r, same l] of
        l : _ -> Just l
        [] -> find same ls

-- | What `keep` writes in the plan (lines now): the step line with the
-- anchor the `do` added (a replace, when it proposed one), and the lines
-- to insert after the step. Nothing when the step is gone.
keepLines :: [Line] -> Review -> Maybe (Line, Maybe Text, [Text])
keepLines ls r = locateStep ls r <&> \step -> case rvLines r of
  first : rest | "- [" `T.isPrefixOf` T.strip first -> (step, Just first, rest)
  xs -> (step, Nothing, xs)
  where
    m <&> f = fmap f m

-- | The review as the window draws it (the conjure pause's shape): the
-- title, the first sentence, the step, what it said, the tests after it,
-- the denials in words, the landing (EDIT, its WorkspaceEdit, and the
-- unified diff per file), the offers and the details' sections. STEP is
-- the step now (its row and number), when it is still in the plan.
reviewPayload :: Review -> Maybe (Int, Int) -> [FileChange] -> Value -> Value
reviewPayload r step changes edit =
  object
    [ "id" .= rvId r
    , "kind" .= ("do" :: Text)
    , "plan" .= rvPlan r
    , "row" .= maybe (rvRow r) fst step
    , "title" .= object ["verb" .= ("do" :: Text), "step" .= fmap snd step, "unit" .= unit, "badge" .= badge, "model" .= rvModel r, "seconds" .= (round (rvSeconds r) :: Int), "cost" .= rvCost r]
    , "summary" .= summary
    , "step" .= T.strip (rvStep r)
    , "said" .= said
    , "tests" .= object ["ran" .= not (null (rvTests r)), "passed" .= count "passed", "failed" .= count "failed", "list" .= [object ["test" .= rtName t, "status" .= rtState t] | t <- rvTests r]]
    , "denials" .= [object ["tool" .= dnTool d, "input" .= dnInput d, "pattern" .= allowPattern d, "allowed" .= (allowPattern d `elem` rvAllowed r)] | d <- nub (rvDenials r)]
    , "denied" .= deniedWords (rvDenials r) (rvTools r)
    , "landing" .= object ["diff" .= T.concat (map fcDiff changes), "files" .= [object ["file" .= fcPath c, "status" .= changeName (fcChange c), "diff" .= fcDiff c] | c <- changes], "edit" .= edit]
    , "offers" .= offers
    , "details" .= details
    ]
  where
    count s = length (filter ((== s) . rtState) (rvTests r))
    unit = snd <$> snd (stepKey (rvStep r))
    badge = case rvStatus r of
      "answered" -> "◆ answered" :: Text
      "blocked" -> "✗ blocked"
      s -> "✗ " <> s
    files n = T.pack (show n) <> (if n == 1 then " file" else " files")
    changeName = \case Modified -> "modified" :: Text; Added -> "added"; Deleted -> "deleted"
    summary = case rvStatus r of
      "answered" | null changes -> "The do answered and changed no file."
      "answered" -> "The do changed " <> files (length changes) <> "."
      "blocked" -> "The do was blocked: " <> fromMaybe "it gave no reason" (rvWhy r) <> (if null changes then "" else " It changed " <> files (length changes) <> ".")
      s -> "The do ended (" <> s <> "): " <> fromMaybe "" (rvWhy r) <> (if null changes then "" else " It changed " <> files (length changes) <> ".")
    said = [w | l <- rvSaid r, Just w <- [sayLine l]]
    sayLine l
      | Just a <- T.stripPrefix "DONE" l = Just ("changed " <> T.strip a)
      | Just w <- T.stripPrefix "BLOCKED" l = Just ("blocked: " <> T.strip w)
      | "- ? " `T.isPrefixOf` T.strip l = Just ("asks: " <> T.strip (T.drop 4 (T.strip l)))
      | otherwise = Nothing
    newDenials = [d | d <- nub (rvDenials r), allowPattern d `notElem` rvAllowed r]
    offers =
      [offer "keep" "a" "keep the change" (if null (rvLines r) then "the files stay as the do left them" else "the files stay; its plan lines land under the step")]
        ++ [offer "revert" "r" "revert the change" "files restored, new files removed; nothing in the plan" | isJust (rvRef r), not (null changes)]
        ++ [offer "test" "t" "run the step's tests now" "their marks land in the plan, the tick when all are green"]
        ++ [offer "allow" "p" ("allow " <> T.intercalate ", " (nub (map (\d -> "`" <> allowPattern d <> "`") newDenials))) "in future dos: [plans] do_tools in .olifant/config.toml" | not (null newDenials)]
    offer choice key label detail = object ["choice" .= (choice :: Text), "key" .= (key :: Text), "label" .= (label :: Text), "detail" .= (detail :: Text)]
    details =
      [ section "the step" (T.strip (rvStep r)) Nothing
      , section "what it said" (T.unlines (rvSaid r)) Nothing
      , section "plan lines it proposes" (T.unlines (rvLines r)) Nothing
      , section "tests after it" (if null (rvTests r) then "none ran" else T.unlines [rtState t <> "  " <> rtName t | t <- rvTests r]) Nothing
      , section "denials" (if null (rvDenials r) then "none" else T.unlines [dnTool d <> "  " <> dnInput d | d <- rvDenials r]) Nothing
      , section "the run" (T.unlines (["model " <> rvModel r, "seconds " <> T.pack (show (rvSeconds r)), "cost " <> maybe "unknown" (\c -> "$" <> T.pack (show c)) (rvCost r), "session " <> fromMaybe "none" (rvSession r), "errand " <> rvErrand r] ++ ["before " <> trCommit t | Just t <- [rvRef r]])) Nothing
      , section "prompt" (rvPrompt r) Nothing
      , section "answer" (rvAnswer r) Nothing
      ]
        ++ [section ("diff · " <> T.pack (fcPath c)) (fcDiff c) (Just "diff") | c <- changes]
    section title body lang = object (["title" .= (title :: Text), "text" .= body] ++ ["lang" .= (l :: Text) | Just l <- [lang]])
