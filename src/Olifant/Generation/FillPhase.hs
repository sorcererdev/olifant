-- | The fill phase of `gen` (spec §6.4): one request for every hole, each
-- fill parsed as its kind (the adapter's `wrap`) and compile-checked (its
-- `[check]`), failing holes retried with the reason, and any still failing
-- left as their placeholder. Holes never block a generation.
module Olifant.Generation.FillPhase (fillPhase, applyFills, fillLines, splitImports, addImports, isImportLine, projectFiles, reindented, cutToHole, looksLikeFile, topLevelNames, markerOnly, wholeFile, wholeFileReason, cutSentence, testsFailed, passesAgainstRaise, assertionSpan) where

import Control.Monad (foldM, forM, forM_)
import Data.Aeson (object, (.=))
import Data.List (isSuffixOf, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Char (isAlphaNum, isSpace)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Fill
import Olifant.Generator.Definition (Verify (..))
import Olifant.Generation.Plan (HoleStatus (..), PlannedHole (..))
import Olifant.Generation.Steer (RoundHole (..), RoundState (..), Seed (..), Steer (..), Steering (..))
import Olifant.Generation.Tools (ProgressEvent (..), Tools (..))
import Olifant.Holes (HoleSite (..), dedented)
import Olifant.Language.Header (headerEnd)
import Olifant.Language.TestUnits (TestUnit (..), testUnitsIn)
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
  Just mf -> do
    -- each file's top-level unit names, once, for the files with a hole of
    -- lines (what tells a whole-file answer)
    tops <- fmap M.fromList . forM (nub [holeFile (phSite h) | h <- holes, linesKind (phSite h)]) $ \p ->
      (p,) <$> maybe (pure []) (\l -> topNames l (M.findWithDefault "" p base)) (langOf p)
    case toolSteering tools of
      Nothing -> loop tops mf 1 M.empty [] [] [Try h 0 [] | h <- holes]
      -- a steered loop starts after its seed's rounds, each hole with its
      -- previous attempts
      Just st -> let Seed done previous = stSeed st in loop tops mf (done + 1) M.empty [] [] [Try h 0 (M.findWithDefault [] (holeName (phSite h)) previous) | h <- holes]
  where
    -- by the language's [unit] query (spans at column 0), else its
    -- [unit] name over the lines at column 0
    topNames l text
      | null (langUnitKinds l) = pure (topLevelNames l text)
      | otherwise =
          querySyntax (toolSyntax tools) l (object ["any" .= [object ["kind" .= k] | k <- langUnitKinds l]]) text <&> \case
            Left _ -> topLevelNames l text
            Right ms ->
              let ls = T.splitOn "\n" text
                  nameIn a z = listToMaybe [n | i <- [a .. z], Just n <- [unitNameOf l (fromMaybe "" (listToMaybe (drop i ls)))]]
               in nub [n | Match (a, c) (z, _) _ _ <- ms, c == 0, Just n <- [nameIn a z]]
    -- accepted: hole name -> (site, fill, attempts, filler); notes: why a check could not run
    -- @stuck@: holes given up on for a reason no retry can change (their
    -- tests could not run at all: review I3)
    loop tops mf r accepted notes stuck pending = toolCancelled tools >>= \case
     True -> pure (finish accepted notes (unfilled (stuck ++ [(t, "cancelled") | t <- pending])))
     False -> do
      toolProgress tools (Filling [name t | t <- pending] r (mfName mf))
      reply <- runFill (mfFiller mf) (FillRequest [fillHole t | t <- pending] inputs (toolFillContext tools) (toolStance tools))
      let tried = [t {tryAttempts = tryAttempts t + 1} | t <- pending]
          -- FAILED0: each hole still red, why, and its verify's run if one ran
          next accepted' notes' failed0
            | null failed || (unsteered && r >= 1 + max 0 (mfRetries mf)) = do
                forM_ (failed' ++ newlyStuck) (\(t, why) -> toolProgress tools (Unfilled (name t) why))
                pure (finish accepted' notes' (unfilled (failed' ++ stuck ++ newlyStuck)))
            | Just st <- toolSteering tools = do
                -- a red round (never an unrunnable or a green one): the hook
                -- says again, climb or pause; the filler's retries do not count
                forM_ newlyStuck (\(t, why) -> toolProgress tools (Unfilled (name t) why))
                stSteer st (RoundState r (mfName mf) [RoundHole (name t) why (tryPrevious t) run | (t, why, run) <- failed]) >>= \case
                  Again -> loop tops mf (r + 1) accepted' notes' (stuck ++ newlyStuck) (map fst failed')
                  Climb mf' -> loop tops mf' (r + 1) accepted' notes' (stuck ++ newlyStuck) (map fst failed')
                  Pause why -> pure (finish accepted' notes' (unfilled (stuck ++ newlyStuck) ++ [(t, HolePaused why) | (t, _) <- failed']))
            | otherwise = do
                forM_ newlyStuck (\(t, why) -> toolProgress tools (Unfilled (name t) why))
                loop tops mf (r + 1) accepted' notes' (stuck ++ newlyStuck) (map fst failed')
            where
              unsteered = isNothing (toolSteering tools)
              unrunnable (_, why, _) = "the tests could not run" `T.isPrefixOf` why
              newlyStuck = [(t, why) | (t, why, _) <- filter unrunnable failed0]
              failed = filter (not . unrunnable) failed0
              failed' = [(t, why) | (t, why, _) <- failed]
      case reply of
        -- An unreadable reply is a failed attempt, retried like any other.
        Left why -> next accepted notes [(t, why, Nothing) | t <- tried]
        Right fills -> do
          let noFill = [(t, "the model returned no fill for `" <> name t <> "`", Nothing) | t <- tried, M.notMember (name t) fills]
          -- A fill that repeats the file around its hole (a local model
          -- answers with the whole file) is cut to the hole; one that still
          -- answers a file is not placed, and asked again saying so.
          cut <- forM [(t, f) | t <- tried, Just f <- [M.lookup (name t) fills]] $ \(t, f) -> case cutFill (site t) f of
            Just (f', n) -> toolProgress tools (CutToHole (name t) n f') >> pure (t, f')
            Nothing -> pure (t, f)
          let wholeFiles = [(t, f, wholeFile) | (t, f) <- cut, answersAFile tops (site t) f]
          forM_ wholeFiles (\(t, _, _) -> toolProgress tools (WholeFile (name t)))
          let offered = [(t, f) | (t, f) <- cut, not (answersAFile tops (site t) f)]
              escaped = wholeFiles ++ [(t, f, why) | (t, f) <- offered, Just why <- [escapes f]]
          parsed <- forM [(t, f) | (t, f) <- offered, Nothing <- [escapes f]] $ \(t, f) -> (t,f,) <$> parses (site t) (snd (splitImports (importsOf (holeFile (site t))) f))
          let badParse = escaped ++ [(t, f, "does not parse as " <> holeKind (site t)) | (t, f, False) <- parsed]
              candidates0 = [(t, f) | (t, f, True) <- parsed]
              known = [(s', f') | (s', f', _, _) <- M.elems accepted]
              with cs = applyFills langOf base (known ++ [(site t, f) | (t, f) <- cs])
          (broken0, note0) <- checkFiles (with candidates0) [site t | (t, _) <- candidates0]
          -- errors the files had with their placeholders are not the fills'
          -- (python review I1); checked only when something is broken
          already <- if M.null broken0 then pure M.empty else fst <$> checkFiles (with []) [site t | (t, _) <- candidates0]
          let notAlready = M.filter (not . null) . M.mapWithKey (\p es -> if p `elem` toolJudged tools then es else filter (`notElem` M.findWithDefault [] p already) es)
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
          -- A block whose first line came less indented than the rest (written
          -- as if the first line sat at the marker's column): read with the
          -- rest brought up to it when, with only this fill placed (so another
          -- hole's errors in the file do not count), that compiles; or, when
          -- the fill as written has a syntax error, when that leaves fewer
          -- findings or no syntax error (the rest may still not type-check:
          -- the reason is then the type error, not the indent).
          rescued <- if M.null brokenA then pure candidatesA else forM candidatesA $ \(t, f) -> case reindented (langComment <$> langOf (holeFile (site t))) f of
            Just f' | M.member (holeFile (site t)) brokenA -> do
              let alone g = M.findWithDefault [] (holeFile (site t)) . notAlready . fst <$> checkFiles (with [(t, g)]) [site t]
              before <- alone f
              after <- alone f'
              let better = null after || (any syntaxError before && (length after < length before || not (any syntaxError after)))
              if not (null before) && better
                then toolProgress tools (Reindented (name t) f') >> pure (t, f')
                else pure (t, f)
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
          -- Holes of one file sharing a verify (a test's arrangement and
          -- assertion) are one: it runs once for them, and they stand or
          -- are asked again together (none stands while another is pending).
          let verifyOf t = (,holeFile (site t)) <$> M.lookup (bare (name t)) verifyFor
              pendingOf key = [t' | t' <- tried, verifyOf t' == Just key, name t' `notElem` [name u | (u, _) <- stood]]
          (verified, _) <- foldM (\(done, outcomes) (t, f) -> case (verifyOf t, langOf (holeFile (site t))) of
            (Just key@(verify, _), Just lang)
              | o : _ <- pendingOf key -> pure (done ++ [(t, f, (Just ("verified with `" <> name o <> "`, which has no fill that stands"), Nothing))], outcomes)
              | Just out <- lookup key outcomes -> pure (done ++ [(t, f, out)], outcomes)
              | otherwise -> do
                  out <- verifyWith (with stood) t verify lang
                  pure (done ++ [(t, f, out)], outcomes ++ [(key, out)])
            _ -> pure (done ++ [(t, f, (Nothing, Nothing))], outcomes)) ([], []) stood
          let good' = [(t, f) | (t, f, (Nothing, _)) <- verified]
              verifyFailed = [(t, f, why, run) | (t, f, (Just why, run)) <- verified]
              accepted' = foldr (\(t, f) -> M.insert (name t) (site t, f, tryAttempts t, mfName mf)) accepted good'
              failed = noFill ++ [(t {tryPrevious = tryPrevious t ++ [(f, why)]}, why, run) | (t, f, why, run) <- [(t, f, why, Nothing) | (t, f, why) <- badParse ++ compileFailed ++ clash] ++ verifyFailed]
          forM_ good' (\(t, _) -> toolProgress tools (Filled (name t) (tryAttempts t)))
          next accepted' (maybe notes (: notes) note) failed

    -- A hole's verify over FILES (every standing fill in place):
    -- Nothing when it passes, else why not; and the tests' run, when one ran.
    verifyWith files t verify lang = do
      toolProgress tools (Verifying (name t))
      (scope, everything) <- projectFiles ws files lang [holeFile (site t)]
      outcome <- case verify of
        VerifyCommand argv -> fmap noRun <$> runVerifyIn (toolVerifier tools) lang scope argv everything
        VerifyRedAt tid line passing -> verifyRedAt lang tid line passing everything
        VerifyKills tid keep new path mutated -> fmap noRun <$> verifyKills lang tid keep new path mutated everything
        VerifyTests -> case langTest lang of
          Nothing -> pure (Left (CheckFailed "verify = \"tests\" needs a [test] table in the language"))
          Just td -> verifyTests lang [T.pack (testsForFile td (holeFile (site t)))] everything
        VerifyKeep own keep -> verifyKeep lang own keep everything
      let failedPrefix = case verify of
            VerifyRedAt {} -> ""
            VerifyKills {} -> ""
            _ -> testsFailed <> "\n"
      pure $ case outcome of
        Right (True, _, run) -> (Nothing, run)
        Right (False, out, run) -> (Just (failedPrefix <> T.intercalate "\n" (lastN 40 (T.lines out))), run)
        Left why -> (Just ("the tests could not run: " <> describeCheckFailure why), Nothing)
    noRun (ok, out) = (ok, out, Nothing)

    unfilled = map (fmap HoleUnfilled)

    -- FAILED: each hole not filled, and its status (unfilled or paused).
    finish accepted notes failed =
      let status h = case (M.lookup (holeName (phSite h)) accepted, [st | (t, st) <- failed, name t == holeName (phSite h)]) of
            (Just (_, f, n, by), _) -> h {phStatus = HoleFilled by n, phFill = Just f}
            (_, st : _) -> h {phStatus = st}
            _ -> h
          warnings = ["holes were filled but not compile-checked: " <> n | n <- nub notes, not (M.null accepted)]
       in (applyFills langOf base [(s', f') | (s', f', _, _) <- M.elems accepted], map status holes, warnings)

    -- The tests (a module's file, or a block's one test) must pass.
    verifyTests lang ids everything =
      runTestsIn (toolTester tools) lang ids everything <&> \case
        Left why -> Left (CheckFailed why)
        Right run
          | Just why <- trBroken run -> Right (False, why, Just run)
          | null (trFailed run) -> Right (True, "", Just run)
          | otherwise -> Right (False, T.unlines [tid <> ": " <> fMessage fl <> "\n" <> fOutput fl | (tid, fl) <- trFailed run], Just run)

    -- A code block's conjure: its test's file runs whole; its test passes,
    -- and each test it keeps that the run has (the failure named by its id).
    verifyKeep lang own keep everything =
      runTestsIn (toolTester tools) lang [fst (T.breakOn "::" own)] everything <&> \case
        Left why -> Left (CheckFailed why)
        Right run
          | Just why <- trBroken run -> Right (False, why, Just run)
          | otherwise -> case keptVerdict own keep run of
              Right () -> Right (True, "", Just run)
              Left (at, fl) -> Right (False, fromMaybe own at <> ": " <> fMessage fl <> "\n" <> fOutput fl, Just run)

    -- A test conjure: the test must fail, and at its assertion: at or after
    -- its first assert line, before its end, as the fills left the file
    -- (an arrangement or assertion over several lines moves it); a test
    -- not found there, at LINE exactly. Or, PASSING, pass: a regression
    -- test (unless its function's body raises: then a pass asserts nothing).
    verifyRedAt lang tid line passing everything =
      runTestsIn (toolTester tools) lang [tid] everything <&> \case
        Left why -> Left (CheckFailed why)
        Right run
          | Just why <- trBroken run -> Right (False, "the test cannot run: " <> why, Just run)
          | [(_, f)] <- trFailed run, Just l <- fLine f, inside l -> Right (True, "", Just run)
          | [(_, f)] <- trFailed run -> Right (False, "the test fails before its assertion (line " <> maybe "?" (T.pack . show) (fLine f) <> "): " <> fMessage f, Just run)
          | passing -> Right (True, "", Just run)
          | otherwise -> Right (False, passesAgainstRaise, Just run)
      where
        (file, test) = (T.unpack (fst (T.breakOn "::" tid)), T.drop 2 (snd (T.breakOn "::" tid)))
        (from, to) = maybe (line, line) id (M.lookup file everything >>= assertionSpan lang test)
        inside l = l >= from && l <= to

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
      let undefinedIn p = case supportOf =<< langCheck =<< langOf p of
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
          -- a line importing names from a module, rewritten for the one name (`[import_merge] write`)
          froms = [ line
                  | Just mg <- [langImportMerge lang], l <- ls, Just (m, es) <- [merging mg l], e <- es, snd (binding e) == n
                  , Right line <- [render (M.fromList [("module", VText m), ("names", VText e)]) (mergeWrite mg)] ]
          -- a line importing a module whole (an `[[imports.resolve]]` without names): its first segment, or its alias
          plains = [ l | l <- ls, Just rest <- [wholeModule l], let (x, y) = binding rest, (if x == y then T.takeWhile (/= '.') x else y) == n ]
          wholeModule l = listToMaybe [m | Resolve pat g Nothing <- maybe [] imResolve (langImportsDef lang), Just m <- [regexGroup pat g l]]
      pure $ case nub (froms ++ plains) of
        [one] -> Just one
        _ -> Nothing

    fillHole t =
      let s = site t
          instruction = if linesKind s then markerOnly <> "; " <> holeInstruction s else holeInstruction s
       in FillHole (holeName s) (holeKind s) instruction (holeFile s) (context s) (tryPrevious t) (isJust (importsOf (holeFile s))) (langOf (holeFile s) >>= langImportsDef >>= imExample)

    -- A hole of lines (a block or statements): what a whole-file answer is cut to.
    linesKind s = T.takeWhile (/= '@') (holeKind s) `elem` ["block", "stmt"]

    -- The file's other holes' placeholders beside this one's (a conjure's
    -- arrangement and assertion).
    siblings s = [holePlaceholder (phSite h) | h <- holes, holeFile (phSite h) == holeFile s, holeName (phSite h) /= holeName s]

    cutFill s f
      | linesKind s, Just l <- langOf (holeFile s) = cutToHole l (importsOf (holeFile s)) (siblings s) (M.findWithDefault "" (holeFile s) base) (holeLine s) f
      | otherwise = Nothing

    -- A fill of lines that, its leading imports aside, still holds a test,
    -- or a unit the file defines, at its own top level ('looksLikeFile'):
    -- a file, not the hole's lines.
    answersAFile tops s f = linesKind s && maybe False (\l -> looksLikeFile l (M.findWithDefault [] (holeFile s) tops) (snd (splitImports (importsOf (holeFile s)) f))) (langOf (holeFile s))

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
applyFills langOf = foldl (\files (s, f) -> M.adjust (fillPlaceholder (langOf (holeFile s)) (holePlaceholder s) f) (holeFile s) files)

-- | TEXT with the fill F in place of PLACEHOLDER, as 'applyFills' places it.
fillPlaceholder :: Maybe Language -> Text -> Text -> Text -> Text
fillPlaceholder lang placeholder f t = case T.breakOn placeholder t of
  (_, "") -> t
  (before, after) ->
    let (imports, code) = splitImports (langImports =<< lang) f
        lineStart = snd (T.breakOnEnd "\n" before)
        indent = T.takeWhile (`elem` (" \t" :: String)) lineStart
        body = T.intercalate ("\n" <> indent) (dedented code)
     in maybe id (\l -> addImports l imports) lang (before <> body <> T.drop (T.length placeholder) after)

-- | TEXT with its lines L1..L2 (1-based) the fill F, at L1's indentation: what a hole
-- over those lines lands as, filled with F (a conjure's round, kept).
fillLines :: Maybe Language -> Text -> (Int, Int) -> Text -> Text
fillLines lang text (l1, l2) f = fillPlaceholder lang marker f (T.intercalate "\n" (take (l1 - 1) ls ++ [indent <> marker] ++ drop l2 ls))
  where
    ls = T.splitOn "\n" text
    indent = T.takeWhile (`elem` (" \t" :: String)) (fromMaybe "" (listToMaybe (drop (l1 - 1) ls)))
    marker = "\0olifant-fill\0"

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

-- | The line at I and those up to the one closing the bracket it opens at
-- its end, as one (each stripped), and the closing line's index.
spanning :: [Text] -> Int -> Maybe (Text, Int)
spanning ls i = case drop i ls of
  l : rest
    | "(" `T.isSuffixOf` T.stripEnd l
    , (inner, close : _) <- break (T.isInfixOf ")") rest ->
        Just (T.concat (map T.strip (l : inner ++ [close])), i + length inner + 1)
  _ -> Nothing

-- | A top-level import line, by the language's `imports`.
isImportLine :: Language -> Text -> Bool
isImportLine lang l = not (T.isPrefixOf " " l) && not (T.isPrefixOf "\t" l) && maybe False (`matches` l) (langImports lang)

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
      -- one written over several lines (its bracket open at the end of its
      -- first, closed lines later): its names read as one line's
      | Just m <- langImportMerge lang
      , Just (modu, names) <- merging m line
      , (i, end, have) : _ <- [(i, end, ns) | (i, l) <- zip [0 :: Int ..] ls, i >= headerEnd ls, topLevel l, Just (joined, end) <- [spanning ls i], Just (mo, ns) <- [merging m joined], mo == modu] =
          case filter (`notElem` have) names of
            [] -> ls
            -- the closing bracket on a line of its own: each name a line above it
            more
              | T.strip (ls !! end) == ")" ->
                  let lastName = ls !! (end - 1)
                      indent = if end - 1 > i then T.takeWhile (`elem` (" \t" :: String)) lastName else "    "
                      -- the name before them without its comma gets one
                      above
                        | end - 1 > i, not ("," `T.isSuffixOf` T.stripEnd lastName) = take (end - 1) ls ++ [T.stripEnd lastName <> ","]
                        | otherwise = take end ls
                   in above ++ [indent <> n <> "," | n <- more] ++ drop end ls
              | otherwise -> case render (M.fromList [("module", VText modu), ("names", VText (T.intercalate ", " (have ++ more)))]) (mergeWrite m) of
                  Right merged -> take i ls ++ [merged] ++ drop (end + 1) ls
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

-- | A fill whose first non-blank line is indented less (@a@) than every
-- other non-blank line (at least @c@), and opens no block (it does not end
-- with @:@, an open bracket or a backslash): those lines @c - a@ less
-- (Nothing otherwise).
-- A trailing comment (the language's COMMENT) is not the line's end.
reindented :: Maybe Text -> Text -> Maybe Text
reindented comment f = case T.lines (T.dropWhile (== '\n') f) of
  first : rest@(_ : _)
    | not (T.null (T.strip first))
    , not (any (`T.isSuffixOf` T.stripEnd (uncommented first)) [":", "(", "[", "{", "\\"])
    , a <- indentOf first
    , ns@(_ : _) <- [indentOf l | l <- rest, not (T.null (T.strip l))]
    , c <- minimum ns
    , c > a ->
        Just (T.intercalate "\n" (first : [T.drop (c - a) l | l <- rest]))
  _ -> Nothing
  where
    indentOf = T.length . T.takeWhile (== ' ')
    uncommented l = maybe l (\c -> if T.null c then l else fst (T.breakOn c l)) comment

-- | A check's finding that says the file does not parse (pyright's words
-- for Python's indentation and syntax errors), not that it does not type.
syntaxError :: Text -> Bool
syntaxError m = any (`T.isInfixOf` m) ["IndentationError", "SyntaxError", "Unexpected indentation", "Unindent not expected", "Unindent amount does not match", "Expected indented block"]

-- | What every hole of lines is asked first, and a whole-file answer again.
markerOnly :: Text
markerOnly = "answer with the lines that replace the marker only"

-- | Why a fill that still answers a file once cut is not placed, and what
-- its retry is told.
wholeFile :: Text
wholeFile = wholeFileReason <> "; " <> markerOnly <> ": no `def` line, no imports, no other tests"

-- | 'wholeFile''s reason, as the step log and progress say it.
wholeFileReason :: Text
wholeFileReason = "the model answered with more than the hole: a whole file"

-- | A cut fill, as the step log, progress and the HUD say it.
cutSentence :: Int -> Text
cutSentence n = "cut to the hole (" <> T.pack (show n) <> " lines dropped)"

-- | A fill of lines cut to its hole, when it repeats the file around it
-- (TEXT, the hole on line LINE, 1-based; SIBLINGS, the placeholders of the
-- file's other holes). Its anchor is the line opening the block the hole
-- is in (the nearest line above it, less indented, ending with `:`: a
-- test's def): a fill holding it once (whitespace aside) repeats the file,
-- and loses it, all before it, and all after the block's end (the next line
-- as shallow as it). Then, holding at its own base level the nearest line
-- below the hole that is no other hole's and says something (a letter or
-- digit), it loses that line and all after it; and a hole beside another
-- (a test's arrangement and assertion, answered alike) keeps its side of
-- the first `assert` (with none after its first line, the arrangement is
-- `pass` and the assertion all of it). Import lines before the cut that
-- the file lacks are kept, first (IMPORTS, the language's). The fill as
-- cut and how many of its lines were dropped; Nothing when nothing is cut
-- (no anchor, or one held twice), or nothing would be left.
cutToHole :: Language -> Maybe Text -> [Text] -> Text -> Int -> Text -> Maybe (Text, Int)
cutToHole lang imports siblings text line f = do
  opens <- langOpensBlock lang
  let fl = T.splitOn "\n" f
      ls = T.splitOn "\n" text
      hole = fromMaybe "" (listToMaybe (drop (line - 1) ls))
      above = reverse (take (line - 1) ls)
      below = drop line ls
      sibling l = any (`T.isInfixOf` l) siblings
      saying = listToMaybe . filter (\l -> T.any isAlphaNum l && not (sibling l)) . filter (not . blank)
      next = listToMaybe . filter (not . blank)
      siblingAbove = maybe False sibling (next above)
      siblingBelow = maybe False sibling (next below)
  opener <- listToMaybe [l | l <- above, not (blank l), indentOf l < indentOf hole, matches opens (T.stripEnd (uncommented l))]
  -- the opener, held once: all before it, and after its block
  i <- case [i | (i, l) <- zip [0 :: Int ..] fl, norm l == norm opener] of
    [i] | i < length fl - 1 -> Just i
    _ -> Nothing
  let d = indentOf (fl !! i)
      inBlock = takeWhile (\l -> blank l || indentOf l > d) (drop (i + 1) fl)
      have = map T.strip ls
      brought = [T.strip l | l <- take i fl, not (" " `T.isPrefixOf` l), maybe False (`matches` l) imports, T.strip l `notElem` have]
      base = case [indentOf l | l <- inBlock, not (blank l)] of
        [] -> 0
        ns -> minimum ns
      -- the line below, at the fill's base level, and all after it
      beforeBelow = case saying below of
        Just b | k : _ <- [k | (k, l) <- zip [0 :: Int ..] inBlock, k > 0, indentOf l == base, norm l == norm b] -> take k inBlock
        _ -> inBlock
      -- beside another hole: this side of the first assertion
      asserting l = any (`matches` l) (langAssertion lang)
      body = trimmed beforeBelow
      firstAssert = listToMaybe [k | (k, l) <- zip [0 :: Int ..] body, asserting l]
      split
        | siblingBelow = case firstAssert of
            Just k | k > 0 -> take k body
            _ -> [T.replicate base " " <> e | Just e <- [langEmptyStatement lang]]
        | siblingAbove = maybe body (`drop` body) firstAssert
        | otherwise = body
      kept = trimmed split
      whole = trimmed fl
      result = brought ++ ["" | not (null brought)] ++ kept
      -- an arrangement with nothing before the assertion, in a language
      -- with no empty statement: an empty side
      emptySide = siblingBelow && null kept
  if (null kept && not emptySide) || kept == whole then Nothing else Just (T.intercalate "\n" result, length whole - length kept - length brought)
  where
    blank = T.null . T.strip
    norm = T.filter (not . isSpace)
    indentOf = T.length . T.takeWhile (`elem` [' ', '\t'])
    trimmed = reverse . dropWhile blank . reverse . dropWhile blank
    uncommented l = if T.null (langComment lang) then l else fst (T.breakOn (langComment lang) l)

-- | Whether code for a hole of lines is a file, in LANG, given the file's
-- top-level unit names NAMES: at its own top level (dedented), a test (by
-- the `[test] unit` rules), or a unit (named by `[unit] name`) of a name
-- the file defines at its own top level. An import alone is no file (a
-- local import is code; leading ones join the file's).
looksLikeFile :: Language -> [Text] -> Text -> Bool
looksLikeFile lang names code = any defines tops || any (atTop . fst . tuLines) tests
  where
    ls = dedented code
    top l = not (" " `T.isPrefixOf` l) && not ("\t" `T.isPrefixOf` l)
    tops = filter top ls
    defines l = maybe False (`elem` names) (unitNameOf lang l)
    tests = maybe [] (\td -> testUnitsIn (testUnitRules td) (testAbove td) (T.intercalate "\n" ls)) (langTest lang)
    atTop i = maybe False (\l -> top l && not (T.null (T.strip l))) (listToMaybe (drop (i - 1) ls))

-- | The names of TEXT's units at column 0 by the language's `[unit] name`
-- (when its `[unit]` query cannot run).
topLevelNames :: Language -> Text -> [Text]
topLevelNames lang text = nub [n | l <- T.splitOn "\n" text, not (" " `T.isPrefixOf` l), not ("\t" `T.isPrefixOf` l), Just n <- [unitNameOf lang l]]

-- | A unit's name on its line, by the language's `[unit] name`.
unitNameOf :: Language -> Text -> Maybe Text
unitNameOf lang = \l -> listToMaybe (mapMaybe ($ l) namers)
  where
    namers = map regexName (langUnitNames lang)

-- | What a fill whose tests failed is unfilled for begins with (each
-- failing test's id and message follow, the last 40 lines kept).
testsFailed :: Text
testsFailed = "the tests failed:"

-- | Why a new test that passes is refused while its function's body
-- raises (it has none yet): the pass asserts nothing about the function.
passesAgainstRaise :: Text
passesAgainstRaise = "this test passes against a body that raises: it asserts nothing about the function"

-- | The lines (1-based) where a test's assertion may fail in a test file:
-- the first line of the test NAME (its path below the file, joined by
-- @::@; the test whose path ends so, by the `[test] unit` rules) that
-- matches the language's `[test] assertion`, to the test's last line;
-- none without such a test or an assertion in it.
assertionSpan :: Language -> Text -> Text -> Maybe (Int, Int)
assertionSpan lang name text = do
  td <- langTest lang
  let want = T.splitOn "::" name
      ls = T.splitOn "\n" text
  u <- listToMaybe [u | u <- testUnitsIn (testUnitRules td) (testAbove td) text, want `isSuffixOf` tuPath u]
  let (a0, z) = tuLines u
  a <- listToMaybe [i | i <- [a0 + 1 .. z], Just l <- [listToMaybe (drop (i - 1) ls)], any (`matches` l) (langAssertion lang)]
  pure (a, z)

-- | Group G of expression RE matched on L (none: no match, or no such group).
regexGroup :: Text -> Int -> Text -> Maybe Text
regexGroup re g l = do
  rx <- makeRegexM (T.unpack re) :: Maybe Regex
  groups <- getAllTextSubmatches <$> (matchM rx (T.unpack l) :: Maybe (AllTextSubmatches [] String))
  T.strip . T.pack <$> listToMaybe (drop g groups)
