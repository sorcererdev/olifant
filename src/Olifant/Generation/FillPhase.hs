-- | The fill phase of `gen` (spec §6.4): one request for every hole, each
-- fill parsed as its kind (the adapter's `wrap`) and compile-checked (its
-- `[check]`), failing holes retried with the reason, and any still failing
-- left as their placeholder. Holes never block a generation.
module Olifant.Generation.FillPhase (fillPhase, applyFills, splitImports, addImports, projectFiles) where

import Control.Monad (foldM, forM, forM_)
import Data.Aeson (object, (.=))
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Char (isAlphaNum)
import Data.Functor ((<&>))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Fill
import Olifant.Generator.Definition (Verify (..))
import Olifant.Generation.Plan (HoleStatus (..), PlannedHole (..))
import Olifant.Generation.Tools (ProgressEvent (..), Tools (..))
import Olifant.Holes (HoleSite (..), dedented)
import Olifant.Language.Header (headerEnd)
import Olifant.Language
import Olifant.Workspace (Workspace (..), pruned)
import Olifant.Template (Value (..), render)
import Text.Regex.TDFA (AllTextSubmatches (..), Regex, makeRegexM, matchM, matchTest)
import System.FilePath (splitDirectories)

-- | A hole being tried: attempts so far and every previous (fill, why), oldest first.
data Try = Try {tryHole :: PlannedHole, tryAttempts :: Int, tryPrevious :: [(Text, Text)]}

fillPhase :: Monad m => Workspace m -> Tools m -> Map Text (Verify [Text]) -> [(Text, Text)] -> Map FilePath Text -> [PlannedHole] -> m (Map FilePath Text, [PlannedHole], [Text])
fillPhase ws tools verifyFor inputs base holes = case toolFiller tools of
  Nothing -> pure (base, holes, [])
  Just _ | null holes -> pure (base, holes, [])
  Just mf -> loop mf 1 M.empty [] [] [Try h 0 [] | h <- holes]
  where
    -- accepted: hole name -> (site, fill, attempts); notes: why a check could not run
    -- @stuck@: holes given up on for a reason no retry can change (their
    -- tests could not run at all: review I3)
    loop mf r accepted notes stuck pending = toolCancelled tools >>= \case
     True -> pure (finish mf accepted notes (stuck ++ [(t, "cancelled") | t <- pending]))
     False -> do
      toolProgress tools (Filling [name t | t <- pending] r (mfName mf))
      reply <- runFill (mfFiller mf) (FillRequest [fillHole t | t <- pending] inputs (toolFillContext tools) (toolStance tools))
      let tried = [t {tryAttempts = tryAttempts t + 1} | t <- pending]
          next accepted' notes' failed0
            | null failed || r >= 1 + max 0 (mfRetries mf) = do
                forM_ (failed ++ newlyStuck) (\(t, why) -> toolProgress tools (Unfilled (name t) why))
                pure (finish mf accepted' notes' (failed ++ stuck ++ newlyStuck))
            | otherwise = do
                forM_ newlyStuck (\(t, why) -> toolProgress tools (Unfilled (name t) why))
                loop mf (r + 1) accepted' notes' (stuck ++ newlyStuck) (map fst failed)
            where
              unrunnable (_, why) = "the tests could not run" `T.isPrefixOf` why
              newlyStuck = filter unrunnable failed0
              failed = filter (not . unrunnable) failed0
      case reply of
        -- An unreadable reply is a failed attempt, retried like any other.
        Left why -> next accepted notes [(t, why) | t <- tried]
        Right fills -> do
          let noFill = [(t, "the model returned no fill for `" <> name t <> "`") | t <- tried, M.notMember (name t) fills]
              offered = [(t, f) | t <- tried, Just f <- [M.lookup (name t) fills]]
              escaped = [(t, f, why) | (t, f) <- offered, Just why <- [escapes f]]
          parsed <- forM [(t, f) | (t, f) <- offered, Nothing <- [escapes f]] $ \(t, f) -> (t,f,) <$> parses (site t) (snd (splitImports (importsOf (holeFile (site t))) f))
          let badParse = escaped ++ [(t, f, "does not parse as " <> holeKind (site t)) | (t, f, False) <- parsed]
              candidates0 = [(t, f) | (t, f, True) <- parsed]
              known = [(s', f') | (s', f', _) <- M.elems accepted]
              with cs = applyFills langOf base (known ++ [(site t, f) | (t, f) <- cs])
          (broken0, note0) <- checkFiles (with candidates0) [site t | (t, _) <- candidates0]
          -- errors the files had with their placeholders are not the fills'
          -- (python review I1); checked only when something is broken
          already <- if M.null broken0 then pure M.empty else fst <$> checkFiles (with []) [site t | (t, _) <- candidates0]
          let notAlready = M.filter (not . null) . M.mapWithKey (\p es -> filter (`notElem` M.findWithDefault [] p already) es)
              broken1 = notAlready broken0
          -- Auto-import (no model round): in a file whose every error is a
          -- name not defined, a candidate using that name gets the one import
          -- line in the project that binds it, and all are judged again.
          imported <- if M.null broken1 then pure candidates0 else autoImport broken1 candidates0
          (candidatesA, brokenA, noteA) <-
            if map snd imported == map snd candidates0
              then pure (candidates0, broken1, note0)
              else do
                (b, n) <- checkFiles (with imported) [site t | (t, _) <- imported]
                pure (imported, notAlready b, n)
          -- A block whose first line came unindented and the rest one level
          -- in (written as if the first line sat at the marker's column): read
          -- one level up, when that alone compiles.
          rescued <- if M.null brokenA then pure candidatesA else forM candidatesA $ \(t, f) -> case reindented f of
            Just f' | M.member (holeFile (site t)) brokenA -> do
              (b, _) <- checkFiles (with [(t, f')]) [site t]
              pure (if M.null (notAlready b) then (t, f') else (t, f))
            _ -> pure (t, f)
          (candidates, broken, note) <-
            if map snd rescued == map snd candidatesA
              then pure (candidatesA, brokenA, noteA)
              else do
                (b, n) <- checkFiles (with rescued) [site t | (t, _) <- rescued]
                pure (rescued, notAlready b, n)
          -- All together compile: every candidate stands. Otherwise each is
          -- checked alone beside what is already accepted, so a fill that
          -- breaks another file is the one blamed (review I5).
          verdicts <-
            if M.null broken
              then pure [(t, f, Nothing) | (t, f) <- candidates]
              else forM candidates $ \(t, f) -> do
                (alone0, _) <- checkFiles (with [(t, f)]) [site t]
                let alone = notAlready alone0
                pure (t, f, if M.null alone then Nothing else Just (T.intercalate "; " (concat (M.elems alone))))
          let good = [(t, f) | (t, f, Nothing) <- verdicts]
              compileFailed = [(t, f, why) | (t, f, Just why) <- verdicts]
          -- Each good alone may still clash together: then none of them stands.
          (together, _) <- if M.null broken || null good then pure (M.empty, Nothing) else (\(b, n) -> (notAlready b, n)) <$> checkFiles (with good) [site t | (t, _) <- good]
          let clash = if M.null together then [] else [(t, f, "this fill and another do not compile together: " <> T.intercalate "; " (concat (M.elems together))) | (t, f) <- good]
              stood = if null clash then good else []
          -- A hole with `verify`: its fill must also pass that command (its
          -- tests), run over the files with every standing fill in place.
          verified <- forM stood $ \(t, f) -> case (M.lookup (bare (name t)) verifyFor, langOf (holeFile (site t))) of
            (Just verify, Just lang) -> do
              toolProgress tools (Verifying (name t))
              (scope, everything) <- projectFiles ws (with stood) lang [holeFile (site t)]
              outcome <- case verify of
                VerifyCommand argv -> runVerifyIn (toolVerifier tools) lang scope argv everything
                VerifyRedAt tid line -> verifyRedAt lang tid line everything
                VerifyKills tid keep new path mutated -> verifyKills lang tid keep new path mutated everything
                VerifyTests -> case langTest lang of
                  Nothing -> pure (Left (CheckFailed "verify = \"tests\" needs a [test] table in the language"))
                  Just td -> verifyTests lang [T.pack (testsForFile td (holeFile (site t)))] everything
                VerifyIds ids -> verifyTests lang ids everything
              let failedPrefix = case verify of
                    VerifyRedAt {} -> ""
                    VerifyKills {} -> ""
                    _ -> "the tests failed:\n"
              pure $ case outcome of
                Right (True, _) -> (t, f, Nothing)
                Right (False, out) -> (t, f, Just (failedPrefix <> T.intercalate "\n" (lastN 40 (T.lines out))))
                Left why -> (t, f, Just ("the tests could not run: " <> describeCheckFailure why))
            _ -> pure (t, f, Nothing)
          let good' = [(t, f) | (t, f, Nothing) <- verified]
              verifyFailed = [(t, f, why) | (t, f, Just why) <- verified]
              accepted' = foldr (\(t, f) -> M.insert (name t) (site t, f, tryAttempts t)) accepted good'
              failed = noFill ++ [(t {tryPrevious = tryPrevious t ++ [(f, why)]}, why) | (t, f, why) <- badParse ++ compileFailed ++ clash ++ verifyFailed]
          forM_ good' (\(t, _) -> toolProgress tools (Filled (name t) (tryAttempts t)))
          next accepted' (maybe notes (: notes) note) failed

    finish mf accepted notes failed =
      let status h = case (M.lookup (holeName (phSite h)) accepted, [why | (t, why) <- failed, name t == holeName (phSite h)]) of
            (Just (_, f, n), _) -> h {phStatus = HoleFilled (mfName mf) n, phFill = Just f}
            (_, why : _) -> h {phStatus = HoleUnfilled why}
            _ -> h
          warnings = ["holes were filled but not compile-checked: " <> n | n <- nub notes, not (M.null accepted)]
       in (applyFills langOf base [(s', f') | (s', f', _) <- M.elems accepted], map status holes, warnings)

    -- The tests (a module's file, or a block's one test) must pass.
    verifyTests lang ids everything =
      runTestsIn (toolTester tools) lang ids everything <&> \case
        Left why -> Left (CheckFailed why)
        Right run
          | Just why <- trBroken run -> Right (False, why)
          | null (trFailed run) -> Right (True, "")
          | otherwise -> Right (False, T.unlines [tid <> ": " <> fMessage fl <> "\n" <> fOutput fl | (tid, fl) <- trFailed run])

    -- A test draft: the test must fail, and at exactly its assertion line.
    verifyRedAt lang tid line everything =
      runTestsIn (toolTester tools) lang [tid] everything <&> \case
        Left why -> Left (CheckFailed why)
        Right run
          | Just why <- trBroken run -> Right (False, "the test cannot run: " <> why)
          | [(_, f)] <- trFailed run, fLine f == Just line -> Right (True, "")
          | [(_, f)] <- trFailed run -> Right (False, "the test fails before its assertion (line " <> maybe "?" (T.pack . show) (fLine f) <> "): " <> fMessage f)
          | otherwise -> Right (False, "the test passes before any code exists: it does not test the new behaviour")

    -- A kill: the tests that passed before and the new ones (KEEP) pass on
    -- the files, and a NEW one fails with the mutant in place; a sibling
    -- test already red is none of these.
    verifyKills lang tid keep new path mutated everything = do
      onOriginal <- runTestsIn (toolTester tools) lang [tid] everything
      onMutant <- runTestsIn (toolTester tools) lang [tid] (M.insert path mutated everything)
      let failing run = map fst (trFailed run)
      pure $ case (onOriginal, onMutant) of
        (Left why, _) -> Left (CheckFailed why)
        (_, Left why) -> Left (CheckFailed why)
        (Right o, Right m)
          | Just why <- trBroken o -> Right (False, "the new test cannot run: " <> why)
          | any (`elem` new) (failing o) -> Right (False, "the new test fails on the current code")
          | k : _ <- filter (`elem` keep) (failing o) -> Right (False, k <> " passed before and fails now")
          | Just why <- trBroken m -> Right (False, "the new test cannot run with the mutant in place: " <> why)
          | not (any (`elem` new) (failing m)) -> Right (False, "the new test does not notice the mutant (" <> T.pack path <> ")")
          | otherwise -> Right (True, "")

    name = holeName . site
    -- a repeated hole's name less its "[file:line]"
    bare = T.takeWhile (/= '[')
    lastN n xs = drop (length xs - n) xs
    site = phSite . tryHole

    -- Each candidate in a file that says names are not defined gets, for
    -- each such name its fill uses, the one project import line binding it;
    -- the re-check judges (other errors are often the cascade: pyright
    -- strict's "Type of … is unknown", review I1).
    autoImport broken cs = do
      let undefinedIn p = case supportFor . checkFormat =<< langCheck =<< langOf p of
            Nothing -> []
            Just support -> [n | m <- M.findWithDefault [] p broken, Just n <- [supportUndefined support (T.drop (length p + 2) m)]]
          wanted = nub [(p, n) | p <- M.keys broken, n <- undefinedIn p]
      if null wanted
        then pure cs
        else do
          found <- forM (nub [(p, n) | (p, n) <- wanted]) $ \(p, n) -> ((p, n),) <$> maybe (pure Nothing) (\l -> importFor l n) (langOf p)
          forM cs $ \(t, f) -> do
            let p = holeFile (site t)
                lines' = nub [l | ((q, n), Just l) <- found, q == p, usesName n f]
            forM_ lines' (\l -> toolProgress tools (AutoImported (name t) l))
            pure (t, if null lines' then f else T.intercalate "\n" lines' <> "\n\n" <> f)

    usesName n f = any (== n) (T.split (\c -> not (isAlphaNum c || c == '_')) f)

    -- The one import line in the project's files of this language that
    -- binds @n@ (an alias binds its alias), as a line for one name.
    importFor lang n = do
      paths <- listFiles ws "."
      let mine p = not (any (`elem` pruned) (splitDirectories p)) && any (`matchesGlob` p) (langFiles lang)
      texts <- forM (filter mine paths) (readText ws)
      let ls = [T.strip l | Just t <- texts, l <- T.lines t, maybe False (`matches` l) (langImports lang)]
          binding entry = case T.words entry of
            [x, "as", y] -> (x, y)
            ws' -> (T.unwords ws', T.unwords ws')
          froms = [ "from " <> m <> " import " <> e
                  | Just mg <- [langImportMerge lang], l <- ls, Just (m, es) <- [merging mg l], e <- es, snd (binding e) == n ]
          plains = [ l | l <- ls, Just rest <- [T.stripPrefix "import " l], let (x, y) = binding rest, (if x == y then T.takeWhile (/= '.') x else y) == n ]
      pure $ case nub (froms ++ plains) of
        [one] -> Just one
        _ -> Nothing

    fillHole t =
      let s = site t
       in FillHole (holeName s) (holeKind s) (holeInstruction s) (holeFile s) (context s) (tryPrevious t) (isJust (importsOf (holeFile s)))

    langOf = languageFor (toolLanguages tools)
    importsOf p = langImports =<< langOf p

    -- The file around the hole (100 lines each side), the hole marked.
    context s =
      let ls = T.splitOn "\n" (M.findWithDefault "" (holeFile s) base)
          i = holeLine s - 1
          marked = [if j == i then T.replace (holePlaceholder s) ("\10214" <> holeName s <> "\10215") l else l | (j, l) <- zip [0 ..] ls] -- placeholders are unique (they name their hole)
       in T.intercalate "\n" (take 201 (drop (max 0 (i - 100)) marked))

    parses s f = case (languageFor (toolLanguages tools) (holeFile s), lookup (holeKind s) . langHoles =<< languageFor (toolLanguages tools) (holeFile s)) of
      (Just lang, Just k) ->
        querySyntax (toolSyntax tools) lang (object ["kind" .= ("ERROR" :: Text)]) (T.replace "{= fill =}" f (T.replace "{= fill|indent =}" (indented f) (kindWrap k))) >>= \case
          Right [] -> pure True
          Right _ -> pure False
          Left _ -> pure True -- no parser: the check decides
      _ -> pure True

    indented f = T.intercalate "\n" [if T.null (T.strip l) then l else "    " <> l | l <- T.splitOn "\n" f]

    -- Broken files (with messages) among those holding these holes, by each
    -- language's check over the project as this plan has it.
    -- Also why a check could not run (it then validates nothing: review I6).
    checkFiles files sites = do
      let names = nub [langName l | s <- sites, Just l <- [languageFor (toolLanguages tools) (holeFile s)], isJust (langCheck l)]
          langs = [l | l <- toolLanguages tools, langName l `elem` names]
      foldM (\(acc, note) lang -> (\(b, n) -> (M.unionWith (++) acc b, maybe note Just n)) <$> checkLang files lang [holeFile s | s <- sites, fmap langName (languageFor (toolLanguages tools) (holeFile s)) == Just (langName lang)]) (M.empty, Nothing) langs
    checkLang files lang judged = do
      (scope, all') <- projectFiles ws files lang judged
      toolProgress tools (Checking (langName lang))
      runCheckIn (toolChecker tools) lang scope all' >>= \case
        Left f -> pure (M.empty, Just (describeCheckFailure f)) -- parse validation stands; the plan says so
        Right report -> pure (M.fromListWith (flip (++)) [(p, [T.pack p <> ": " <> why]) | (p, why) <- reportBroken report], Nothing)

-- | The language's files and its check's context, from disk, with FILES
-- (a plan's versions) over them: what a check, a verify or a test run runs
-- over. Only the judged files' package ([check] root): its scope, and its
-- files by project path.
projectFiles :: Monad m => Workspace m -> Map FilePath Text -> Language -> [FilePath] -> m (FilePath, Map FilePath Text)
projectFiles ws files lang judged = do
  paths <- listFiles ws "."
  let (scope, chosen) = checkScope lang (nub (paths ++ M.keys files)) judged
      wanted = S.fromList chosen
  disk <- forM [p | p <- chosen, M.notMember p files] (\p -> (p,) <$> readText ws p)
  pure (scope, M.union (M.filterWithKey (\p _ -> p `S.member` wanted) files) (M.fromList [(p, t) | (p, Just t) <- disk]))

-- | Every fill in place of its placeholder. Placeholders name their hole
-- (adapters must use @{= hole =}@), so each is found by its text wherever
-- earlier fills moved it; continuation lines take that line's indentation.
--
-- A fill's leading import lines (the language's `imports`) are not code for
-- the hole: each joins the file's imports, unless the file has it already
-- (or, with `[import_merge]`, adds its new names to the line that imports
-- from the same module).
applyFills :: (FilePath -> Maybe Language) -> Map FilePath Text -> [(HoleSite, Text)] -> Map FilePath Text
applyFills langOf = foldl (\files (s, f) -> M.adjust (fillIn s f) (holeFile s) files)
  where
    fillIn s f t = case T.breakOn (holePlaceholder s) t of
      (_, "") -> t
      (before, after) ->
        let lang = langOf (holeFile s)
            (imports, code) = splitImports (langImports =<< lang) f
            lineStart = snd (T.breakOnEnd "\n" before)
            indent = T.takeWhile (`elem` (" \t" :: String)) lineStart
            body = T.intercalate ("\n" <> indent) (dedented code)
         in maybe id (\l -> addImports l imports) lang (before <> body <> T.drop (T.length (holePlaceholder s)) after)

-- | A fill's leading import lines (blank lines between them dropped), and
-- the code after them.
splitImports :: Maybe Text -> Text -> ([Text], Text)
splitImports Nothing f = ([], f)
splitImports (Just re) f = go [] (T.splitOn "\n" f)
  where
    go acc (l : rest)
      | T.null (T.strip l) && not (null acc) = go acc rest
      | matches re l = go (acc ++ [T.strip l]) rest
    go acc rest = (acc, T.intercalate "\n" rest)

matches :: Text -> Text -> Bool
matches re l = maybe False (`matchTest` T.unpack (T.strip l)) (makeRegexM (T.unpack re) :: Maybe Regex)

-- | The module and names of an import line, by `[import_merge]`.
merging :: ImportMerge -> Text -> Maybe (Text, [Text])
merging m l = case (makeRegexM (T.unpack (mergePattern m)) :: Maybe Regex) of
  Nothing -> Nothing
  Just re -> case matchM re (T.unpack l) of
    Just (AllTextSubmatches (_ : modu : names : _)) -> Just (T.pack modu, filter (not . T.null) (map T.strip (T.splitOn "," (T.pack names))))
    _ -> Nothing

-- | Each import line the file lacks, in order: merged into the line that
-- imports from its module (`[import_merge]`), or put after the file's last
-- top-level import (with none, after its leading comments and docstring, a
-- blank line between).
addImports :: Language -> [Text] -> Text -> Text
addImports lang new t = case langImports lang of
  Nothing -> t
  Just re -> T.intercalate "\n" (foldl (add re) (T.splitOn "\n" t) (nub new))
  where
    topLevel l = not (T.isPrefixOf " " l) && not (T.isPrefixOf "\t" l)
    add re ls line
      | line `elem` ls = ls
      | Just m <- langImportMerge lang
      , Just (modu, names) <- merging m line
      , (i, have) : _ <- [(i, ns) | (i, l) <- zip [0 :: Int ..] ls, i >= headerEnd ls, topLevel l, Just (mo, ns) <- [merging m l], mo == modu] =
          case filter (`notElem` have) names of
            [] -> ls
            more -> case render (M.fromList [("module", VText modu), ("names", VText (T.intercalate ", " (have ++ more)))]) (mergeWrite m) of
              Right merged -> take i ls ++ [merged] ++ drop (i + 1) ls
              Left _ -> ls
      | otherwise = case [i | (i, l) <- zip [0 :: Int ..] ls, i >= headerEnd ls, topLevel l, matches re l] of
          [] ->
            let (header, rest) = splitAt (headerEnd ls) ls
             in header ++ [""  | not (null header)] ++ [line] ++ (case rest of "" : _ -> rest; _ -> "" : rest)
          is -> let (upTo, rest) = splitAt (last is + 1) ls in upTo ++ [line] ++ rest


-- | Why a fill cannot stand in its hole before any parse: it unbalances
-- brackets (it would close the code around it), or it is a placeholder.
escapes :: Text -> Maybe Text
escapes f
  | "olifant:" `T.isInfixOf` f = Just "the fill is the placeholder, not code"
  | not (balanced (0 :: Int) (T.unpack f)) = Just "the fill reaches outside its hole (unbalanced brackets)"
  | otherwise = Nothing
  where
    balanced d = \case
      [] -> d == 0
      '"' : rest -> balanced d (skipString rest)
      c : rest
        | c `elem` ("([{" :: String) -> balanced (d + 1) rest
        | c `elem` (")]}" :: String) -> d > 0 && balanced (d - 1) rest
        | otherwise -> balanced d rest
    skipString = \case
      [] -> []
      '\\' : _ : rest -> skipString rest
      '"' : rest -> rest
      _ : rest -> skipString rest

-- | A fill whose first line is unindented while every other non-blank line
-- is indented at least @c > 0@: those lines @c@ less (Nothing otherwise).
reindented :: Text -> Maybe Text
reindented f = case T.lines (T.dropWhile (== '\n') f) of
  first : rest@(_ : _)
    | indentOf first == 0
    , ns@(_ : _) <- [indentOf l | l <- rest, not (T.null (T.strip l))]
    , c <- minimum ns
    , c > 0 ->
        Just (T.intercalate "\n" (first : [T.drop c l | l <- rest]))
  _ -> Nothing
  where
    indentOf = T.length . T.takeWhile (== ' ')
