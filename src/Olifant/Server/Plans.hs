-- | Internal to Olifant.Server: a plan's anchors (`@path:Unit`) as the
-- server sees them: each resolved to its unit (over the editor's text),
-- stale and missing ones as diagnostics, completed after `@`, and the
-- plan's anchored lines as a list of files.
module Olifant.Server.Plans
  ( isPlanPath
  , Resolved (..)
  , resolveAnchor
  , planDiagnostics
  , AnchorPrints
  , PlanToken (..)
  , planTokenTypes
  , planTokenModifiers
  , planTokens
  , planTokensWith
  , claimHints
  , checksCanRun
  , claimHover
  , claimLine
  , whyBody
  , planCounts
  , countsText
  , keepFooter
  , citationLocations
  , keepEdit
  , planFolds
  , anchorItems
  , newStepSnippet
  , newStepText
  , FileItem (..)
  , planFiles
  , ErrandAnswer (..)
  , runErrand
  , beliefsUnder
  , Reopen (..)
  , currentBranch
  , gitOut
  , gitText
  , confined
  , safely
  ) where

import Control.Lens ((&), (.~), (?~), (^.))
import Control.Monad (filterM, when)
import Language.LSP.Protocol.Lens qualified as L
import Data.Functor ((<&>))
import Data.Dynamic (fromDynamic, toDyn)
import Data.IORef (modifyIORef', readIORef)
import Data.List (find, isPrefixOf, nub)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Text.Encoding qualified as TE
import Olifant.Language.Adapter (Language (..), Probe (..), TestDef (..), chooseTestCommand)
import Olifant.Plans (Kind (..), childrenOf, testState)
import Olifant.Process (Cancel, runBytes)
import Olifant.Server.Errand (Belief (..), BeliefCheck (..), Block (..), Ghost (..), Grounded (..), RunFailure (..), RunResult (..), Status (..), Verb (..), cutBody, doAnswered, doTestCommand, doTestTools, errandPromptWith, projectUnitWords, groundingLimit, parseAnswer, runClaude, statusName)
import Olifant.Server.LoopTypes (Fact (..))
import Data.Time (UTCTime, getCurrentTimeZone)
import Olifant.Server.Journal (DoEnd (..), doEvent)
import Olifant.Server.Journal qualified as Journal
import Olifant.Server.Review (Review (..), archiveErrand, changeSince, keptDiff, saveReview, takeRef, testsOfFacts)
import Olifant.Server.Telemetry (ErrandRecord (..), Record (..), appendRecord, errandId)
import Olifant.Server.State (appendStepLine, closeDoc, projectFor)
import System.Exit (ExitCode (..))
import System.FilePath (dropTrailingPathSeparator, makeRelative, splitDirectories, takeDirectory, takeFileName)
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (Catalog (..), PlansConfig (..))
import Olifant.Language.Adapter (languageFor)
import Olifant.Plans (Anchor (..), Line (..), Section (..), Verdict (..), parsePlan, sectionOf)
import Olifant.Server.State (ServerConfig (..), ServerState (..))
import System.Directory (doesFileExist, findExecutable)
import Olifant.Server.Confine (confined, safely)
import Olifant.Server.Claims (CheckRecord (..), ClaimStore, SentenceEntry (..), checkable, lineKey, claimsOf, sentencesOf, unchecked)
import Olifant.Server.Triage (Route (..))
import Olifant.Server.Checker (CheckMode (..), Citation (..), Confirmed (..), Judgement (..), Print (..), evidenceText, judgementName)
import Olifant.Server.Stale (CitedFile (..), citedPrints, printsNow, staleKeys)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Numeric (showFFloat)
import Olifant.Plans.Sentences (Sentence (..), sentenceKey)
import Data.Set (Set)
import Data.Set qualified as S
import Olifant.Server.Unit (Named (..), fingerprint, unitLookupIn, unitsOf, unitsWithKinds)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (takeExtension, (</>))

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
  , rsWhy :: Maybe Text
  -- ^ why the unit is missing, when the units were read: none of that
  -- name, or a name several units have
  }
  deriving stock (Eq, Show)

-- | The unit an anchor names, over the editor's text: none when the file
-- or the unit is missing, the language is unknown, or reading fails.
resolveAnchor :: ServerState -> Project -> Anchor -> IO Resolved
resolveAnchor s p a = do
  text <- maybe (pure Nothing) (safely Nothing . readText (prWorkspace p)) (confined (anPath a))
  found <- case (text, languageFor (catalogLanguages (prCatalog p)) rel) of
    (Just t, Just lang) -> safely (Left Nothing) (either (const (Left Nothing)) (either (Left . Just) Right . unitLookupIn (anUnit a)) <$> unitsWithKinds (pSyntax (ssPorts s)) lang (Just rel) t)
    _ -> pure (Left Nothing)
  let unit = either (const Nothing) Just found
  pure (Resolved rel (normalisePath (prRoot p </> rel)) unit (fingerprint <$> text <*> fmap namedLines unit) text (either id (const Nothing) found))
  where
    rel = normalisePath (anPath a)

-- | Each anchor resolved once, by its path and unit.
resolveAll :: ServerState -> Project -> [Anchor] -> IO (M.Map (FilePath, Text) Resolved)
resolveAll s p as = M.fromList <$> mapM (\a -> (key a,) <$> resolveAnchor s p a) (nub' as)
  where
    nub' = M.elems . M.fromList . map (\a -> (key a, a))

key :: Anchor -> (FilePath, Text)
key a = (anPath a, anUnit a)

-- | A plan's anchors that no longer hold, over the anchor's columns on its
-- line: one whose unit is not found (a hint, anywhere), and a claim of the
-- user's own verified at another fingerprint (information; "changed since
-- this was verified"), but not inside a question (evidence is history) nor
-- a ticked step. Steps raise no changed-since. With them, the fingerprints
-- read: every resolved anchor's, Nothing for a unit not found.
planDiagnostics :: ServerState -> Project -> FilePath -> Text -> IO ([Diagnostic], AnchorPrints)
planDiagnostics s p path text = do
  let ls = parsePlan text
      anchored = [(l, a) | l <- ls, Just a <- [lnAnchor l]]
  resolved <- resolveAll s p (map snd anchored)
  store <- claimsOf s (prRoot p) path
  ss <- sentencesOf s (prRoot p) path ls
  checking <- M.findWithDefault S.empty (normalisePath path) <$> readIORef (ssChecking s)
  can <- checksCanRun s p
  -- the cited files as they are now (read once, kept until they change)
  let prints = citedPrints store
  now <- printsNow s p (map ptPath prints)
  clock <- pNow (ssPorts s)
  let stale = staleKeys now store
      citedNow = M.fromList [((path, fromMaybe "" (ptUnit pr)), maybe (cdHash f) (\u -> M.lookup u (cdUnits f)) (ptUnit pr)) | pr <- prints, let path = normalisePath (ptPath pr), Just f <- [M.lookup path now]]
  let rows = T.splitOn "\n" text
      utf16At r c = fromIntegral (utf16Length (T.take c (if r < length rows then rows !! r else "")))
      inUtf16 d =
        let Range (Position r0 c0) (Position r1 c1) = d ^. L.range
         in d & L.range .~ Range (Position r0 (utf16At (fromIntegral r0) (fromIntegral c0))) (Position r1 (utf16At (fromIntegral r1) (fromIntegral c1)))
  pure
    ( mapMaybe (\(l, a) -> diagnose ls l a =<< M.lookup (key a) resolved) anchored ++ map inUtf16 (claimHints can store checking stale ss)
        ++ [summary | can, Just summary <- [planSummary ls (planCounts clock ss store stale)]]
    , M.union (M.map (\r -> rsFingerprint r <* rsUnit r) resolved) citedNow )
  where
    diagnose ls l a r = case (rsUnit r, lnVerdict l, rsFingerprint r) of
      (Nothing, _, _) -> Just (at l a DiagnosticSeverity_Hint "? not found")
      (Just _, Verified fp, Just now) | fp /= now, lnKind l == Claim, not (any (shelters . lnKind) (ancestorsIn ls l)) -> Just (at l a DiagnosticSeverity_Information "changed since this was verified")
      _ -> Nothing
    shelters = \case Question -> True; Step True -> True; _ -> False
    at l a sev msg = Diagnostic (columns l a) (Just sev) Nothing Nothing (Just "olifant") msg Nothing Nothing Nothing
    columns l a =
      let row = fromIntegral (lnRow l)
          col n = fromIntegral (utf16Length (T.take n (lnText l)))
       in Range (Position row (col (anStart a))) (Position row (col (anEnd a)))

-- | Can the claim pass check at all: the runner is found (or the server
-- answers checks from a recording), and neither `pass_budget` nor
-- `pass_cost` is 0.
checksCanRun :: ServerState -> Project -> IO Bool
checksCanRun s p = do
  let cfg = catalogPlans (prCatalog p)
      runner = pcRunner cfg
  found <- case scCheckAnswers (ssConfig s) of
    Just _ -> pure True
    Nothing
      | '/' `elem` runner -> doesFileExist runner
      | otherwise -> isJust <$> findExecutable runner
  pure (found && maybe True (> 0) (pcPassBudget cfg) && maybe True (> 0) (pcPassCost cfg))

-- | The claim pass's margin: one diagnostic per sentence (source
-- `olifant-plan`, over the sentence's range in code points; 'planDiagnostics'
-- turns them into UTF-16), from 'claimLine', except the lines only the hover
-- shows ('hoverOnly'): an open step, a step the checker could not tell
-- about, a ticked step found done. CHECKING: the keys being checked now;
-- STALE: the keys whose cited code changed since their check.
claimHints :: Bool -> ClaimStore -> S.Set Text -> S.Set Text -> [Sentence] -> [Diagnostic]
claimHints can store checking stale = mapMaybe hint
  where
    hint x = do
      (code, sev, msg) <- claimLine can store checking stale x
      if code `elem` hoverOnly then Nothing else
        pure (Diagnostic (Range (Position (fromIntegral (snRow x)) (fromIntegral (snCol x))) (Position (fromIntegral (snEndRow x)) (fromIntegral (snEndCol x)))) (Just sev) (Just (InR code)) Nothing (Just "olifant-plan") msg Nothing Nothing Nothing)

-- | The codes the hover shows and the margin does not; their lines carry no glyph.
hoverOnly :: [Text]
hoverOnly = ["step-open", "step-unsure", "step-ticked-done"]

-- | A sentence's margin line: its code, severity and message (glyph first,
-- except 'hoverOnly' lines). A claim: `claim-checking` (…), `claim-right`
-- (✓), `claim-wrong` (✗, a warning), `claim-unsure` (?). A step's progress:
-- `step-done` (✓, with "Mark done?" on a checkbox), `step-misfit` (!, a
-- warning), `step-open`, `step-unsure`; on a ticked step `step-ticked` (!,
-- a warning: the code does not do it) or `step-ticked-done`. A stale
-- verdict: `claim-right-stale`, `claim-wrong-stale`, `step-done-stale`,
-- `step-misfit-stale` (the sign dimmed) and `step-stale` (○: an open step
-- whose code moved). No check: `claim-queued` (○) for a 'checkable'
-- sentence when checks can run (CAN), saying whether a save checks it.
claimLine :: Bool -> ClaimStore -> S.Set Text -> S.Set Text -> Sentence -> Maybe (Text, DiagnosticSeverity, Text)
claimLine can store checking stale x
  | key `S.member` checking = Just ("claim-checking", DiagnosticSeverity_Hint, "… Checking")
  | otherwise = do
      e <- M.lookup key store
      case seCheck e of
        Just c ->
          let cf = cjConfirmed c
              why = [cfWhy cf | not (T.null (cfWhy cf))]
              code' = [w | let w = evidenceText cf, not (T.null w)]
              line ws = T.intercalate " · " ws
              hint co t = Just (co, DiagnosticSeverity_Hint, t)
              warn co t = Just (co, DiagnosticSeverity_Warning, t)
              ticked = snKind x == Step True
              changed = "the code changed since"
           in if key `S.member` stale && cfJudgement cf /= JCannotTell
                then case cfJudgement cf of
                  JRight -> hint "claim-right-stale" (line (["✓ Right when checked", changed] ++ why ++ code'))
                  JWrong -> hint "claim-wrong-stale" (line (["✗ Wrong when checked", changed] ++ why ++ code'))
                  JDone -> hint "step-done-stale" (line (["✓ Done when checked", changed] ++ why ++ code'))
                  JMisfit -> hint "step-misfit-stale" (line (["! Would not work when checked", changed] ++ why ++ code'))
                  _ -> hint "step-stale" "○ The code this step would change has changed · Check this"
                else case cfJudgement cf of
                  JRight -> hint "claim-right" (line (["✓ Right"] ++ why ++ code'))
                  JWrong -> warn "claim-wrong" (line (["✗ Wrong"] ++ why ++ code'))
                  JDone
                    | ticked -> hint "step-ticked-done" (line (["Done in the code"] ++ why ++ code'))
                    | otherwise -> hint "step-done" (line (["✓ Done in the code"] ++ why ++ code' ++ ["Mark done?" | snKind x == Step False]))
                  j | ticked, j `elem` [JOpen, JMisfit] -> warn "step-ticked" (line (["! Ticked, but the code does not do this yet"] ++ why))
                  JMisfit -> warn "step-misfit" (line (["! As written this would not work"] ++ why ++ code'))
                  JOpen -> hint "step-open" (line (["Still open"] ++ why))
                  JCannotTell
                    | cjMode c == CheckProgress -> hint "step-unsure" (line (["Could not tell"] ++ why))
                    | otherwise -> hint "claim-unsure" (line (["? Could not tell"] ++ why))
        Nothing
          | checkable x e && can ->
              Just ("claim-queued", DiagnosticSeverity_Hint, if seOwed e then "○ Unchecked · the next save checks it" else "○ Unchecked · Check this, or Check the plan")
          | otherwise -> Nothing
  where
    key = sentenceKey (snText x)

-- | The hover of the sentences on a row: one line each, without its glyph;
-- none when no sentence there has a line.
claimHover :: ClaimStore -> S.Set Text -> [Sentence] -> Maybe Text
claimHover store stale ss = case [if code `elem` hoverOnly then msg else T.drop 2 msg | x <- ss, Just (code, _, msg) <- [claimLine True store S.empty stale x]] of
  [] -> Nothing
  ls -> Just (T.intercalate "\n\n" (ls ++ [keepFooter | any checked ss]))
  where
    checked x = isJust (M.lookup (sentenceKey (snText x)) store >>= seCheck)

-- | *Show why*'s page for the sentence KEY and its check: the verdict and
-- why, every citation, and who checked it, when, at what cost.
whyBody :: Text -> CheckRecord -> Text
whyBody key ch =
  T.unlines $
    ["# " <> key, "", word <> (if T.null (cfWhy cf) then "" else " · " <> cfWhy cf), "", "## The code it rests on", ""]
      ++ ["- " <> T.pack (ciPath x) <> maybe "" (":" <>) (ciUnit x) <> maybe "" (\n -> " (line " <> T.pack (show n) <> ")") (ciLine x) | x <- cfEvidence cf]
      ++ ["- (none cited)" | null (cfEvidence cf)]
      ++ [ ""
         , "Checked " <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (cjAt ch)) <> " by " <> cjModel ch <> maybe "" (\c -> ", $" <> T.pack (showFFloat (Just 3) c "")) (cjCost ch) <> ", " <> T.pack (show (round (cjSeconds ch) :: Int)) <> " s; the verdict's unit: " <> (let t = evidenceText cf in if T.null t then "none" else t) <> " (" <> judgementName (cfJudgement cf) <> ")." ]
  where
    cf = cjConfirmed ch
    word = case cfJudgement cf of
      JRight -> "Right"
      JWrong -> "Wrong"
      JCannotTell -> "Could not tell"
      JDone -> "Done in the code"
      JOpen -> "Still open"
      JMisfit -> "As written this would not work"

-- | What *Check the plan* would check: (unchecked, stale) sentences, one
-- per key; ticked steps are in neither.
planCounts :: UTCTime -> [Sentence] -> ClaimStore -> S.Set Text -> (Int, Int)
planCounts now ss store stale =
  ( length (unchecked now ss store)
  , S.size (S.fromList [k | x <- ss, let k = sentenceKey (snText x), k `S.member` stale, snKind x /= Step True]) )

-- | "12 unchecked, 3 stale": the parts that are not 0.
countsText :: (Int, Int) -> Text
countsText (n, m) = T.intercalate ", " ([T.pack (show n) <> " unchecked" | n > 0] ++ [T.pack (show m) <> " stale" | m > 0])

-- | The plan's summary (`plan-unchecked`, information) on its first heading
-- (row 0 when it has none): "12 unchecked, 3 stale · Check the plan". None
-- when there is nothing to check.
planSummary :: [Line] -> (Int, Int) -> Maybe Diagnostic
planSummary ls counts@(n, m)
  | n + m == 0 = Nothing
  | otherwise =
      let target = find (\l -> case lnKind l of Heading _ -> True; _ -> False) ls
          row = maybe 0 lnRow target
          width = maybe 0 (utf16Length . lnText) target
       in Just (Diagnostic (Range (Position (fromIntegral row) 0) (Position (fromIntegral row) (fromIntegral width))) (Just DiagnosticSeverity_Information) (Just (InR "plan-unchecked")) Nothing (Just "olifant-plan") (countsText counts <> " · Check the plan") Nothing Nothing Nothing)

-- | The last line of a checked sentence's hover. The server does not know
-- the client's key for *Keep this verdict*; durandal.nvim's default is named.
keepFooter :: Text
keepFooter = "gd: the code · Keep this verdict: <leader>mpk"

-- | Where a checked sentence's citations are, in the checker's order (ROOT
-- the project root; STARTS the 1-based first line of each cited unit that
-- resolved): the line cited, else the unit's first line, else line 1. None
-- for an entry with no check.
citationLocations :: FilePath -> M.Map (FilePath, Text) Int -> SentenceEntry -> [Location]
citationLocations root starts e =
  [ Location (filePathToUri (normalisePath (root </> ciPath c))) (Range (Position n 0) (Position n 0))
  | Just ch <- [seCheck e], c <- cfEvidence (cjConfirmed ch)
  , let line = fromMaybe 1 (ciLine c <|> (ciUnit c >>= \u -> M.lookup (ciPath c, u) starts))
        n = fromIntegral (max 0 (line - 1)) ]
  where
    a <|> b = maybe b Just a

-- | *Keep this verdict*: the check as one evidence line after the
-- sentence's last row (`- ✓ why (path:Unit)` or `- ✗ …`), one level under a
-- bullet or step, at the sentence's indent under prose. Nothing for a check
-- that could not tell, or when the same line is already in the run of
-- evidence lines directly below. A new one goes after that run, so a line's
-- kept verdicts read oldest first.
keepEdit :: [Line] -> Sentence -> CheckRecord -> Maybe TextEdit
keepEdit ls x ch = do
  glyph <- case cfJudgement cf of
    JRight -> Just "✓"
    JWrong -> Just "✗"
    _ -> Nothing
  first <- find ((== snRow x) . lnRow) ls
  lastRow <- find ((== snEndRow x) . lnRow) ls
  let indent = lnIndent first + (if isBullet first then 2 else 0)
      body = glyph <> " " <> T.intercalate " " ([cfWhy cf | not (T.null (cfWhy cf))] ++ ["(" <> w <> ")" | let w = evidenceText cf, not (T.null w)])
      line = T.replicate indent " " <> "- " <> body
      isEvidence l = lnIndent l == indent && any (`T.isPrefixOf` T.drop indent (lnText l)) ["- ✓ ", "- ✗ "]
      run = takeWhile isEvidence (dropWhile ((<= snEndRow x) . lnRow) ls)
      lastOfRun = if null run then lastRow else last run
  if any ((== T.strip line) . T.strip . lnText) run then Nothing else do
    let end = Position (fromIntegral (lnRow lastOfRun)) (fromIntegral (utf16Length (lnText lastOfRun)))
    Just (TextEdit (Range end end) ("\n" <> line))
  where
    cf = cjConfirmed ch
    isBullet l = "- " `T.isPrefixOf` T.drop (lnIndent l) (lnText l)

-- | (path, unit) -> the unit's fingerprint now; Nothing = not found. A key
-- that is absent has not been read yet. Beside the anchors', the prints of
-- the plan's checks: (path, unit) for a unit, (path, "") for a whole file's
-- hash, so a change to cited code is noticed as an anchor's is.
type AnchorPrints = M.Map (FilePath, Text) (Maybe Text)

-- | One span of a plan: row, column and length in UTF-16 code units, its
-- legend type and its modifiers.
data PlanToken = PlanToken {ptRow :: Int, ptCol :: Int, ptLen :: Int, ptType :: Text, ptMods :: [Text]}
  deriving stock (Eq, Show)

-- | The semantic-token legend, in order.
planTokenTypes, planTokenModifiers :: [Text]
planTokenTypes =
  [ "planMarker", "planStep", "planStepDone", "planQuestion", "planEvidence", "planClaim", "planTest"
  , "planAlso", "planJournal", "planAnchorPath", "planAnchorUnit", "planVerdict", "planFingerprint" ]
planTokenModifiers = ["done", "stale", "notFound", "checked", "wrong", "resolved"]

-- | What every span of a plan is, from the parser's lines alone (no I/O):
-- markers, each kind's text, and an anchor's path, unit, verdict and
-- fingerprint. `done` marks the lines below a ticked step; `stale` and
-- `notFound` come from the fingerprints the diagnostics pass last read
-- (a key absent from them is neither). Ascending and never overlapping.
planTokens :: AnchorPrints -> [Line] -> [PlanToken]
planTokens = planTokensWith M.empty

-- | 'planTokens', with a bare line's text marked by its claim entry:
-- `checked` (the checker found it right) or `wrong` (`resolved` is no
-- longer given; it stays in the legend for older clients).
planTokensWith :: ClaimStore -> AnchorPrints -> [Line] -> [PlanToken]
planTokensWith store prints ls = concatMap tokensOf ls
  where
    claimMods l = case cfJudgement . cjConfirmed <$> (M.lookup (lineKey l) store >>= seCheck) of
      Just JRight -> ["checked"]
      Just JWrong -> ["wrong"]
      _ -> []
    parentOf = parentIn ls
    ancestors = ancestorsIn ls
    below l = any (\a -> lnKind a == Step True) (ancestors l)
    tokensOf l = case lnKind l of
      Heading _ -> []
      Prose -> []
      kind ->
        let txt = lnText l
            ind = lnIndent l
            end = T.length (T.stripEnd txt)
            markerLen = case kind of
              Step _ -> 6; Question -> 4; TestChild -> 8; AlsoChild -> 7; _ -> 2
            marker = (ind, markerLen, "planMarker", [])
            mark = testState l
            -- an evidence line's mark and a test child's trailing mark
            (afterMarker, evMark) = case kind of
              Evidence -> (ind + 3 + T.length (T.takeWhile (== ' ') (T.drop (ind + 3) txt)), [(ind + 2, 1, "planVerdict", [])])
              AlsoChild -> (ind + markerLen, [])
              _ -> (ind + markerLen, [])
            textEnd = case (kind, mark) of
              (TestChild, Just _) -> end - 1
              _ -> end
            tail' = case (kind, mark) of
              (TestChild, Just _) -> [(end - 1, 1, "planVerdict", [])]
              _ -> []
            textType = case kind of
              Step False -> "planStep"
              Step True -> "planStepDone"
              Question -> "planQuestion"
              TestChild -> "planTest"
              AlsoChild -> "planAlso"
              Journal -> "planJournal"
              Evidence -> "planEvidence"
              Claim | Just _ <- lnAnchor l, Just p <- parentOf l, lnKind p == Question -> "planEvidence"
              _ -> "planClaim"
            piece from to = [(from, n, textType, claimMods l) | let n = T.length (T.stripEnd (T.take (to - from) (T.drop from txt))), to > from, n > 0]
            anchored = case lnAnchor l of
              Nothing -> piece afterMarker textEnd
              Just a ->
                let pathLen = 2 + T.length (T.pack (anPath a))
                    unitStart = anStart a + pathLen
                    spaces = T.length (T.takeWhile (== ' ') (T.drop (anEnd a) txt))
                    v = anEnd a + spaces
                    fresh = M.lookup (anPath a, anUnit a) prints
                    stale = case (lnVerdict l, fresh) of (Verified fp, Just (Just now)) | now /= fp -> ["stale"]; _ -> []
                    missing = ["notFound" | fresh == Just Nothing]
                    (verdict, vEnd) = case lnVerdict l of
                      Verified _ -> ([(v, 1, "planVerdict", stale), (v + 2, 7, "planFingerprint", stale)], v + 9)
                      NotFound -> ([(v, 11, "planVerdict", [])], v + 11)
                      NoVerdict -> ([], anEnd a)
                 in piece afterMarker (anStart a)
                      ++ [(anStart a, pathLen, "planAnchorPath", []), (unitStart, T.length (anUnit a), "planAnchorUnit", missing)]
                      ++ verdict
                      ++ piece vEnd textEnd
            spans = marker : evMark ++ anchored ++ tail'
         in [ PlanToken (lnRow l) (col c) (utf16Length (T.take n (T.drop c txt))) ty (["done" | below l] ++ ms)
            | (c, n, ty, ms) <- spans
            , let col k = utf16Length (T.take k txt)
            ]

-- | A line's parent: the nearest preceding step or question with a smaller indent.
parentIn :: [Line] -> Line -> Maybe Line
parentIn ls l = case [p | p <- reverse (takeWhile ((< lnRow l) . lnRow) ls), isSQ (lnKind p), lnIndent p < lnIndent l] of
  p : _ -> Just p
  [] -> Nothing
  where
    isSQ = \case Step _ -> True; Question -> True; _ -> False

-- | A line's parent, its parent's, and so on.
ancestorsIn :: [Line] -> Line -> [Line]
ancestorsIn ls l = maybe [] (\p -> p : ancestorsIn ls p) (parentIn ls l)

-- | The ranges a plan folds besides its excerpts: a ticked step with
-- children, and an answered question (a direct child that is evidence, a
-- verified anchored claim, or a claim whose key is in CHECKED: a belief
-- with a stored check; a bare belief alone keeps it open; an ask's answer,
-- grounded claims of any verdict and a bare conclusion, folds), each from its
-- row to its last descendant (start, end; inclusive).
planFolds :: Set Text -> [Line] -> [(Int, Int)]
planFolds checked ls =
  [ (lnRow l, lnRow (last kids))
  | l <- ls
  , let kids = childrenOf ls (lnRow l)
  , not (null kids)
  , case lnKind l of
      Step True -> True
      Question ->
        let direct = [c | c <- kids, lnIndent c == lnIndent l + 2]
         in any answers direct || (any grounded direct && any bare direct)
      _ -> False
  ]

  where
    -- an ask's answer: anchored claims, whatever their verdicts, then a bare conclusion
    grounded c = lnKind c == Evidence || (lnKind c == Claim && lnAnchor c /= Nothing)
    bare c = lnKind c == Claim && lnAnchor c == Nothing
    -- evidence, a verified anchored claim, or a belief whose check is stored
    answers c = case lnKind c of
      Evidence -> True
      Claim
        | Just _ <- lnAnchor c -> case lnVerdict c of Verified _ -> True; _ -> False
        | otherwise -> lineKey c `S.member` checked
      _ -> False

utf16Length :: Text -> Int
utf16Length = T.foldl' (\n c -> n + if fromEnum c > 0xFFFF then 2 else 1) 0

-- | Completions for a line typed up to the cursor. After `@`, whatever was
-- typed matches fuzzily (a subsequence, any case) the project's units
-- (by unit name) and files (by basename or path): units first, files
-- second, each with a `filterText` (basename, unit, path) so a client can
-- filter on its own; the list is bounded ('anchorLimit'). A unit inserts
-- `path:Unit`; a file inserts `path:` and asks the client to complete again.
-- After `@path:prefix`, the units of that file by prefix.
anchorItems :: ServerState -> Project -> Text -> IO [CompletionItem]
anchorItems s p upTo = case T.breakOnEnd "@" upTo of
  ("", _) -> pure []
  (_, typed)
    | T.any (== ' ') typed -> pure []
    | (path, unitPrefix) <- T.breakOnEnd ":" typed, not (T.null path) -> do
        units <- unitsIn (T.unpack (T.dropEnd 1 path))
        pure [item CompletionItemKind_Function (namedName n) (namedName n) | n <- units, unitPrefix `T.isPrefixOf` namedName n]
    | otherwise -> do
        files <- take fileScan . filter (\f -> isJust (confined f) && not (any (\d -> "." `isPrefixOf` d && d /= ".") (splitDirectories f))) <$> safely [] (listFiles ws ".")
        let fileHits = [f | f <- files, subseq typed (T.pack (takeFileName f)) || subseq typed (T.pack f)]
        -- units are looked for only once something is typed, and only in files of a known language
        unitHits <-
          if T.null typed
            then pure []
            else
              fmap concat . mapM (\f -> map (f,) <$> unitsIn f) $ filter (isJust . languageFor langs) files
        let units = [unitItem f n | (f, n) <- unitHits, subseq typed (namedName n)]
            filesItems = [fileItem f | f <- fileHits]
        pure (take anchorLimit (units ++ filesItems))
  where
    ws = prWorkspace p
    langs = catalogLanguages (prCatalog p)
    unitsIn rel = case confined rel of
      Nothing -> pure []
      Just r | Nothing <- languageFor langs r -> pure []
      Just r | Just lang <- languageFor langs r -> safely [] $ readText ws r >>= \case
        Nothing -> pure []
        Just t -> cachedUnits r lang t
    cachedUnits r lang t = do
      let k = (langName lang, r)
      known <- M.lookup k <$> readIORef (ssUnitCache s)
      case known of
        Just (t', d) | t' == t, Just us <- fromDynamic d -> pure us
        _ -> do
          us <- either (const []) id <$> unitsOf (pSyntax (ssPorts s)) lang (Just r) t
          modifyIORef' (ssUnitCache s) (M.insert k (t, toDyn us))
          pure us
    subseq needle hay = go (T.unpack (T.toLower needle)) (T.unpack (T.toLower hay))
      where
        go [] _ = True
        go _ [] = False
        go (x : xs) (y : ys) = if x == y then go xs ys else go (x : xs) ys
    blank kind label insert = CompletionItem label Nothing (Just kind) Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just insert) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    item = blank
    unitItem f n =
      let full = T.pack f <> ":" <> namedName n
       in blank CompletionItemKind_Function full full & L.filterText ?~ T.unwords [T.pack (takeFileName f), namedName n, T.pack f]
    fileItem f =
      -- a file with units inserts `path:` and completes again; one without is just `path`
      if isJust (languageFor langs f)
        then
          blank CompletionItemKind_File (T.pack f) (T.pack f <> ":")
            & L.filterText ?~ T.unwords [T.pack (takeFileName f), T.pack f]
            & L.command ?~ Command "complete the unit" "editor.action.triggerSuggest" Nothing
        else blank CompletionItemKind_File (T.pack f) (T.pack f) & L.filterText ?~ T.unwords [T.pack (takeFileName f), T.pack f]

-- | How many anchor completions at most, and how many files are searched
-- for units in one request.
anchorLimit, fileScan :: Int
anchorLimit = 50
fileScan = 400

-- | A new step as a snippet (holes: the step, the anchor) and as plain text
-- (the same line with its holes filled by their placeholders).
newStepSnippet, newStepText :: Text
newStepSnippet = "- [ ] ${1:step} @${2:anchor}"
newStepText = "- [ ] step @anchor"

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
    , let r = fromMaybe (Resolved (anPath a) (anPath a) Nothing Nothing Nothing Nothing) (M.lookup (key a) resolved)
    ]

-- | An errand's answer: its blocks, each ghost line with its anchor's
-- fingerprint when that resolved (Nothing otherwise); the lines the
-- grammar dropped, the anchored lines that did not resolve, the run's
-- seconds and cost, and (a `do`) the files changed in the tree and the
-- facts its step's tests left (their marks, its tick). 'eaId' is the id of
-- the errand's record (outcomes name it).
data ErrandAnswer = ErrandAnswer
  { eaId :: Text
  , eaBlocks :: [(Block, [Maybe Text])]
  , eaDropped :: Int
  , eaUnresolved :: Int
  , eaSeconds :: Double
  , eaCost :: Maybe Double
  , eaChanged :: [FilePath]
  , eaFacts :: [Fact]
  , eaReview :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | Ask, validate or do one line of a plan (its path, its 0-based row): the
-- line grounded in the units its section points at, the runner run from the
-- project's root, its answer read into ghost blocks and logged. Left is one
-- sentence: the line does not fit the verb, the runner failed, or it
-- answered nothing in the grammar. Nothing is written to the plan. Every
-- errand that reaches the runner, however it ends, appends a record to
-- `.olifant/state/errands.jsonl` and a line to the step log; a line the
-- verb refuses writes neither. A `do` that answered runs AFTER over the
-- plan's lines and its row (the step's tests) before its record. A `do`
-- in a git repository takes the tree's reference first; one that answered,
-- or changed the tree however it ended, leaves a review (its id to
-- REVIEWED, and in the answer).
-- | The author's beliefs under a question (its bare claims), each with
-- what its check found; none unless at least one has a verdict.
beliefsUnder :: ClaimStore -> [Line] -> Line -> [Belief]
beliefsUnder store ls q
  | lnKind q /= Question = []
  | any (\(Belief _ c) -> c /= NotChecked) beliefs = beliefs
  | otherwise = []
  where
    bare = [c | c <- childrenOf ls (lnRow q), lnIndent c == lnIndent q + 2, lnKind c == Claim, lnAnchor c == Nothing]
    beliefs = [Belief (lineKey c) (checkOf (M.lookup (lineKey c) store >>= seCheck)) | c <- bare]
    checkOf = \case
      Just c | JRight <- cfJudgement (cjConfirmed c) -> BelievedRight (evidenceText (cjConfirmed c))
      Just c | JWrong <- cfJudgement (cjConfirmed c) -> BelievedWrong (cfWhy (cjConfirmed c)) (evidenceText (cjConfirmed c))
      _ -> NotChecked

-- | A do that continues an earlier one's thread: its id, its session, and
-- what the user asked for.
data Reopen = Reopen {roParent :: Text, roSession :: Text, roAsk :: Text}
  deriving stock (Eq, Show)

runErrand :: (ServerState -> Project -> [Line] -> Int -> IO [Fact]) -> (Text -> IO ()) -> Maybe Reopen -> Cancel -> Verb -> FilePath -> Int -> ServerState -> IO (ServerState, Either Text ErrandAnswer)
runErrand after reviewed reopen token verb path row s0 =
  projectFor path s0 >>= \case
    (s, Nothing) -> pure (s, Left "this plan is in no Olifant project")
    (s, Just (_, p)) -> (s,) <$> errand s p
  where
    errand s p = do
      text <- case M.lookup path (ssDocs s) of
        Just (_, t) -> pure t
        Nothing -> fromMaybe "" <$> safely Nothing (readText (pFiles (ssPorts s)) path)
      let ls = parsePlan text
          cfg = catalogPlans (prCatalog p)
          root = prRoot p
      case find ((== row) . lnRow) ls of
        Just target | fits (lnKind target) -> do
          let sec = sectionOf ls row
              around = target : childrenOf ls row ++ [l | l <- ls, lnRow l >= secRow sec, lnRow l < secEnd sec]
              anchors = take groundingLimit (foldl (\acc a -> if any ((== key a) . key) acc then acc else acc ++ [a]) [] [a | l <- around, Just a <- [lnAnchor l]])
          grounded <- catMaybes <$> mapM (\a -> grounding a <$> resolveAnchor s p a) anchors
          tests <- projectTests (workspaceProbe (pFiles (ssPorts s)) (prRoot p)) p
          let tools = if verb == Do then testTools p tests else []
              asked = maybe "" (\ro -> "Further change asked: " <> roAsk ro <> "\n\n") reopen
          store <- claimsOf s root path
          let beliefs = if verb == Ask then beliefsUnder store ls target else []
              prompt = asked <> errandPromptWith beliefs verb (projectUnitWords (catalogLanguages (prCatalog p))) (if verb == Do then listToMaybe' (mapMaybe doTestCommand tests) else Nothing) text ls target grounded
              rel = makeRelative root (normalisePath path)
          branch <- currentBranch root
          now <- pNow (ssPorts s)
          -- this ending's record and step log line (neither can fail the
          -- errand); its id, which the answer carries
          let record status why session mrr blocks dropped ghosts resolved unresolved changed = do
                let rid = errandId now verbName rel row session
                appendRecord (ssPorts s) root . AnErrand $
                  ErrandRecord
                    { erId = rid, erAt = now, erVerb = verbName, erPlan = rel, erRow = row, erSection = secTitle sec
                    , erBranch = branch, erModel = pcModel cfg, erStatus = status, erWhy = why
                    , erSession = session, erSeconds = rrSeconds <$> mrr, erTurns = mrr >>= rrTurns
                    , erTokens = mrr >>= rrTokens, erCost = mrr >>= rrCost, erModels = maybe [] rrModels mrr
                    , erPromptChars = T.length prompt, erGrounded = length grounded
                    , erBlocks = blocks, erLines = ghosts, erDropped = dropped, erUnresolved = unresolved
                    , erChanged = changed, erDenials = maybe 0 rrDenials mrr, erParent = roParent <$> reopen, erAuto = False, erMode = Nothing, erVerdict = Nothing }
                appendStepLine (ssPorts s) root
                  [ "plan-" <> verbName
                  , T.pack rel <> ":" <> tshow row
                  , tshow ghosts <> " lines"
                  , tshow (resolved :: Int) <> " resolve"
                  , tshow (maybe 0 rrSeconds mrr) <> " s"
                  , maybe "cost unknown" (\c -> tshow c <> " usd") (mrr >>= rrCost)
                  , pcModel cfg
                  , statusName status
                  ]
                pure rid
          let onDisk f = safely Nothing (readText (pFiles (ssPorts s)) f)
              -- the state and project over the files as a do left them: the
              -- editor's text of a file it wrote is older than the disk (the
              -- editor reloads only after the answer), so it is set aside for
              -- the step's tests
              landed before = do
                wrote <- filterM (\(f, was) -> (/= was) <$> onDisk f) before
                projectFor path (foldr (closeDoc . fst) s wrote) <&> \case
                  (s', Just (_, p')) -> (s', p')
                  (_, Nothing) -> (s, p)
          -- before a do, the disk under each file open in the editor (but
          -- the plan), to tell which ones it wrote
          before <- if verb == Do then mapM (\f -> (f,) <$> onDisk f) [f | f <- M.keys (ssDocs s), f /= normalisePath path, (normalisePath root <> "/") `isPrefixOf` f] else pure []
          -- a do's tree before it: what its review is the change since
          ref <- if verb == Do then safely Nothing (takeRef root) else pure Nothing
          let -- a do's review, when it answered or changed the tree (its id);
              -- every do is archived and journaled, however it ended
              review rid status why mrr ghosts facts answered
                | verb /= Do = pure Nothing
                | otherwise = do
                    changes <- maybe (pure []) (safely [] . changeSince root [rel]) ref
                    let said = [l | Just rr <- [mrr], l <- map T.strip (T.lines (rrText rr)), any (`T.isPrefixOf` l) ["DONE", "BLOCKED", "- ? "]]
                        r = Review
                          { rvId = rid, rvErrand = rid, rvPlan = rel, rvRow = lnRow target, rvStep = lnText target
                          , rvAt = now, rvModel = pcModel cfg, rvSeconds = maybe 0 rrSeconds mrr, rvCost = mrr >>= rrCost
                          , rvStatus = status, rvWhy = why
                          , rvSaid = said
                          , rvLines = map ghText ghosts
                          , rvFacts = [(n, maybe "" lnText (find ((== n) . lnRow) ls), t) | Replace n t <- facts]
                          , rvTests = testsOfFacts [t | Replace _ t <- facts]
                          , rvDenials = maybe [] rrDenied mrr, rvTools = tools ++ pcDoTools cfg, rvAllowed = []
                          , rvRef = ref, rvPrompt = prompt, rvAnswer = maybe "" rrText mrr, rvSession = mrr >>= rrSession
                          , rvParent = roParent <$> reopen, rvDiff = keptDiff changes, rvChoice = Nothing }
                        end = case status of
                          "answered" -> DoAnswered (case [T.strip d | l <- said, Just d <- [T.stripPrefix "DONE" l]] of
                            d : _ | not (T.null d) -> d
                            _ -> T.pack (show (length changes)) <> (if length changes == 1 then " file changed" else " files changed"))
                          "blocked" -> DoBlocked (fromMaybe "no reason given" why)
                          "empty" -> DoEmpty
                          "timeout" -> DoTimeout
                          "cancelled" -> DoCancelled
                          -- the runner by its name, not its path
                          _ -> DoError (T.replace (T.pack (pcRunner cfg)) (T.pack (takeFileName (pcRunner cfg))) (fromMaybe status why))
                    safely () (archiveErrand (ssPorts s) root r)
                    zone <- getCurrentTimeZone
                    _ <- safely [] (Journal.record (ssPorts s) zone root rel [doEvent rid (lnText target) (lnRow target) now (isJust reopen) (pcModel cfg) end])
                    case ref of
                      Just _ | answered || not (null changes) -> do
                        saved <- safely False (saveReview (ssPorts s) root r >> pure True)
                        if saved then reviewed rid >> pure (Just rid) else pure Nothing
                      _ -> pure Nothing
          runClaude token cfg verb tools (roSession <$> reopen) root prompt >>= \case
            Left f -> do
              changed <- if verb == Do then changedFiles root else pure []
              rid <- record (rfStatus f) (Just (rfWhy f)) (rfSession f) Nothing 0 0 0 0 0 (length changed)
              _ <- review rid (statusName (rfStatus f)) (Just (rfWhy f)) Nothing [] [] False
              pure (Left (rfWhy f))
            Right rr -> case parseAnswer verb ls target (rrText rr) of
              ([], dropped) | verb /= Do || not (doAnswered (rrText rr)) -> do
                changed <- if verb == Do then changedFiles root else pure []
                let why = verbName <> " answered nothing in the grammar"
                rid <- record Empty (Just why) (rrSession rr) (Just rr) 0 dropped 0 0 0 (length changed)
                _ <- review rid "empty" (Just why) (Just rr) [] [] False
                pure (Left why)
              (blocks, dropped) -> do
                fps <- mapM (mapM (fpOf s p) . blLines) blocks
                changed <- if verb == Do then changedFiles root else pure []
                facts <- if verb == Do then safely [] (landed before >>= \(s', p') -> after s' p' ls row) else pure []
                let ghosts = concatMap blLines blocks
                    resolved = length (filter isJust (concat fps))
                    unresolved = length (filter (isJust . ghAnchor) ghosts) - resolved
                    blockedWhy = listToMaybe' [T.strip w | l <- T.lines (rrText rr), Just w <- [T.stripPrefix "BLOCKED" (T.strip l)]]
                rid <- record Ok Nothing (rrSession rr) (Just rr) (length blocks) dropped (length ghosts) resolved unresolved (length changed)
                rv <- review rid (maybe "answered" (const "blocked") blockedWhy) blockedWhy (Just rr) ghosts facts True
                -- after its own line, the do's tests journaled against the plan as the do read it
                when (verb == Do && not (null facts)) $ safely () $ do
                  fp <- maybe (pure Nothing) (fmap rsFingerprint . resolveAnchor s p) (lnAnchor target)
                  zone <- getCurrentTimeZone
                  Journal.recordTests (ssPorts s) zone root rel ls row facts fp now
                pure (Right (ErrandAnswer rid (zip blocks fps) dropped unresolved (rrSeconds rr) (rrCost rr) changed facts rv))
        _ -> pure (Left (verbName <> " needs a " <> kindName <> " line"))
    fits = case verb of
      Ask -> (== Question)
      Validate -> \case Claim -> True; Heading _ -> True; _ -> False
      Do -> \case Step _ -> True; TestChild -> True; _ -> False
    verbName = case verb of Ask -> "ask"; Validate -> "validate"; Do -> "do"
    kindName = case verb of Ask -> "question"; Validate -> "claim or heading"; Do -> "step or test"
    -- a resolved unit's lines, for the prompt
    grounding a r = case (rsUnit r, rsText r) of
      (Just n, Just t) ->
        let (from, to) = namedLines n
            ls = T.splitOn "\n" t
         in Just (Grounded a (fromMaybe "" (listToMaybe' (drop (namedDef n - 1) ls))) (cutBody (T.intercalate "\n" (take (to - from + 1) (drop (from - 1) ls)))))
      _ -> Nothing
    listToMaybe' = foldr (const . Just) Nothing
    fpOf s p g = case ghAnchor g of
      Nothing -> pure Nothing
      Just a -> (\r -> rsFingerprint r <* rsUnit r) <$> resolveAnchor s p a
    -- the files the run changed: tracked ones changed since HEAD and new
    -- untracked ones, but not the plan nor Olifant's state; nothing when git
    -- cannot say (no repository, no HEAD). Both calls only read.
    changedFiles root = do
      diffed <- gitOut root ["diff", "--name-only", "--relative", "HEAD"]
      untracked <- gitOut root ["ls-files", "--others", "--exclude-standard"]
      let plan = normalisePath path
      pure $ case (diffed, untracked) of
        (Just d, Just u) -> [f | f <- nub (map (normalisePath . (root </>)) (d ++ u)), f /= plan, not ((normalisePath (root </> ".olifant/state") <> "/") `isPrefixOf` f)]
        _ -> []

-- | The branch checked out at ROOT; Nothing when it is no repository or
-- HEAD is detached.
currentBranch :: FilePath -> IO (Maybe Text)
currentBranch root =
  gitOut root ["rev-parse", "--abbrev-ref", "HEAD"] <&> \case
    Just [b] | b /= "HEAD" -> Just (T.pack b)
    _ -> Nothing

-- | A read-only git command's output lines; Nothing when it fails.
gitOut :: FilePath -> [String] -> IO (Maybe [String])
gitOut root args =
  safely Nothing $ runBytes "git" (["-C", root] ++ args) (Just root) "" >>= \case
    Right (ExitSuccess, out, _) -> pure (Just (map T.unpack (filter (not . T.null) (T.lines (TE.decodeUtf8Lenient out)))))
    _ -> pure Nothing

-- | A read-only git command's whole output; Nothing when it fails.
gitText :: FilePath -> [String] -> IO (Maybe Text)
gitText root args =
  runBytes "git" (["-C", root] ++ args) (Just root) "" <&> \case
    Right (ExitSuccess, out, _) -> Just (TE.decodeUtf8Lenient out)
    _ -> Nothing

-- | A `do`'s test tools: the project's (first) test command as its prompt
-- prints it, up to the tests slot ('doTestTools'), e.g.
-- `Bash(uv run --frozen pytest -q*)`; none without a `[test]`.
testTools :: Project -> [[Text]] -> [Text]
testTools p tests = take 1 [ts | ts <- map (doTestTools (prRoot p)) tests, not (null ts)] >>= id

-- | The project's test commands, the first language's first: each the
-- candidate the project's files choose ('chooseTestCommand', by EXISTS and
-- `has`, from the project root), @{= root =}@ the project's folder.
projectTests :: Monad m => Probe m -> Project -> m [[Text]]
projectTests probe p = concat <$> traverse chosen [td | l <- catalogLanguages (prCatalog p), Just td <- [langTest l]]
  where
    chosen td = chooseTestCommand probe td <&> \case
      Right (c, _) -> [map (T.replace "{= root =}" (T.pack (dropTrailingPathSeparator (prRoot p)))) (testCommand c)]
      Left _ -> []

-- | A candidate's `when` files through the workspace, from the project's root.
workspaceProbe :: Monad m => Workspace m -> FilePath -> Probe m
workspaceProbe ws root = Probe
  { probeRead = \f -> readText ws (root </> f)
  , probeList = \d -> (\fs -> [f | f <- fs, takeDirectory f == "."]) <$> listFiles ws (root </> d)
  }

tshow :: Show a => a -> Text
tshow = T.pack . show
