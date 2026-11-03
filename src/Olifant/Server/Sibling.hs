-- | Internal to Olifant.Server: a sibling test, read. Conjure learns a service's fakes and
-- its constructor from the nearest test that already builds an owner with a port's fake
-- (language-neutral 4b, milestone 4; spike D), instead of the pack's `[conjure.fake]` and
-- `[conjure.constructor]` tables, which stay the first test's fallback.
--
-- Everything is read through the language: the definitions by its grammar's queries
-- ('definitionKinds'), a parameter's type by `[unit] annotation`, the tests by `[test]`. No
-- literal of any language is named here.
module Olifant.Server.Sibling
  ( Sibling (..)
  , ConjureLanguage (..)
  , conjureLanguage
  , scanDef
  , siblingLine
  , siblingShapes
  , conjuredFakes
  , siblingCandidates
  , findSibling
  , servicePorts
  , serviceBase
  , filesUnder
  , splitTopArgs
  , escapeRe
  ) where

import Data.Char (isAlphaNum, isSpace)
import Data.Functor ((<&>))
import Data.List (isPrefixOf, nub, sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Glob (matchesGlob)
import Olifant.Language (ConjureDef (..), CtorShape (..), FakeShape (..), Language (..), PortShape (..), TestDef, isTestFile, testsForFile)
import Olifant.Language.TestUnits (testSourcesIn)
import Olifant.Language.TreeSitter (definitionKinds)
import Olifant.Server.ConjureScan (Port (..), balanced, ctorParams, groups, occurrencesWith, portsIn, servicePaths, snake)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (joinPath, makeRelative, splitDirectories, takeDirectory, takeExtension, (</>))

-- | What a sibling test says about the project, read deterministically. 'sbTest' the
-- sibling test file; 'sbOwner' the definition its constructor call builds; 'sbFakes' (port
-- type, the fake's class, the parameter) for each argument where a port type is expected;
-- 'sbAct' the act form (@plain@, @await@, @.await@); 'sbFake' and 'sbCtor' the shapes the
-- pack used to declare, learned from it ('ctFiles' set by 'findSibling').
data Sibling = Sibling {sbTest :: FilePath, sbOwner :: Text, sbFakes :: [(Text, Text, Text)], sbAct :: Text, sbFake :: FakeShape, sbCtor :: CtorShape}
  deriving stock (Eq, Show)

-- | The language one conjure request runs on: the site's, with `[conjure.fake]` and
-- `[conjure.constructor]` the sibling's when one reads ('clSibling'); else as declared (the
-- first test's recipe), or neither (the floor). 'clPorts' the service's ports.
data ConjureLanguage = ConjureLanguage {clLanguage :: Language, clSibling :: Maybe Sibling, clPorts :: [Port]}

-- | Once per conjure request (a resume rebuilds it from the same files): LANG for UNIT of
-- the module (REL, its text, its test file's text).
conjureLanguage :: Workspace IO -> Language -> (FilePath, Text, Maybe Text) -> Text -> IO ConjureLanguage
conjureLanguage ws lang site@(rel, _, _) unit = do
  ports <- servicePorts ws lang rel
  findSibling ws lang ports site unit <&> \case
    Just sb -> ConjureLanguage lang {langConjure = (\cj -> cj {cjFake = Just (sbFake sb), cjConstructor = Just (sbCtor sb)}) <$> langConjure lang} (Just sb) ports
    Nothing -> ConjureLanguage lang Nothing ports

-- | The conjure def a flag scan of TEST (the test file's current text) uses: with a sibling,
-- its fakes' classes plus the library doubles TEST binds ('conjuredFakes'), re-derived at
-- every scan (a fill that binds a @Mock()@ is seen); without one, as declared.
scanDef :: ConjureLanguage -> Text -> Maybe ConjureDef
scanDef cl test = case clSibling cl of
  Nothing -> langConjure (clLanguage cl)
  Just _ -> (\cj -> cj {cjFake = learned <$> cjFake cj}) <$> langConjure (clLanguage cl)
  where
    learned fs = fs {fsClasses = nub (fsClasses fs ++ conjuredFakes (fsBound fs) (clPorts cl) test)}

-- | The steps.log words for the conjure language: the sibling read, or the fallback taken.
siblingLine :: ConjureLanguage -> [Text]
siblingLine cl = case clSibling cl of
  Just sb ->
    let fs = sbFake sb
        ct = sbCtor sb
     in [ "conjure-sibling", T.pack (sbTest sb), "owner=" <> sbOwner sb, "start=" <> ctStart ct, "files=" <> T.intercalate "," (ctFiles ct)
        , "fakes=" <> T.intercalate "," [p <> ":" <> ty <> "=" <> c | (ty, c, p) <- sbFakes sb]
        , "classes=" <> T.intercalate "," (fsClasses fs), "prefixes=" <> T.intercalate "," (fsPrefixes fs), "keyword=" <> fromMaybe "-" (fsKeyword fs), "bound=" <> fsBound fs, "act=" <> sbAct sb ]
  Nothing -> ["conjure-sibling", "none", maybe "floor" (\cj -> if isJust (cjFake cj) || isJust (cjConstructor cj) then "declared shapes" else "floor") (langConjure (clLanguage cl))]

-- | The shapes a sibling test teaches for UNIT, by LANG's `[unit] annotation` and the
-- service's PORTS: (test path TP, test text TT, subject text ST).
--
-- - The owner is a class-like definition of ST whose own span holds a definition named UNIT
--   (first), or one the test calls (@.n(@), and which the test calls with arguments.
-- - Its constructor is a definition inside the owner whose parameters carry the call's
--   argument names (by keyword), or as many as it passes (by position).
-- - A fake is what an argument passes where the parameter's type is a port.
--
-- Nothing without an annotation, or when a step finds nothing.
siblingShapes :: Language -> [Port] -> Text -> (FilePath, Text, Text) -> Maybe Sibling
siblingShapes lang ports unit0 (tp, tt, st) = do
  ann <- langUnitAnnotation lang
  defs <- definitionKinds lang st
  let unit = lastSegment unit0
      paramRe = "^([[:alnum:]_]+)" <> T.replace "{= name =}" "" ann
      sls = T.lines st
      lineAt n = fromMaybe "" (listToMaybe (drop (n - 1) sls))
      inside (a, z) (a', z') = a <= a' && z' <= z && (a, z) /= (a', z')
      members own = [(lastSegment n, s) | (n, _, s, _) <- defs, own `inside` s]
      called n = ("." <> n <> "(") `T.isInfixOf` tt
      owners =
        sortOn (\(_, _, ms) -> unit `notElem` map fst ms)
          [ (lastSegment n, own, ms)
          | (n, cap, own, _) <- defs
          , "definition.class" `T.isPrefixOf` cap || "definition.impl" `T.isPrefixOf` cap
          , let ms = members own
          , any (\(m, _) -> m == unit || called m) ms
          ]
  (owner, ownSpan, ms, args) <- listToMaybe [(n, own, ms, as) | (n, own, ms) <- owners, Just as <- [callArgs n tt], not (null as)]
  let ownerLine = lineAt (fst ownSpan)
      named = [(p, sep, v) | a <- args, Just (p, sep, v) <- [keywordArg a]]
      keyworded = length named == length args
      ctorOf (_, (a, _)) = do
        let l = lineAt a
            stripped = T.stripStart l
            (before, after) = T.breakOn "(" stripped
        if T.null after then Nothing else Just ()
        let shape = CtorShape [] (ownerRe ownerLine owner) (escapeRe (before <> "(")) paramRe
            text = T.intercalate "\n" (drop (a - 1) (take (snd ownSpan) sls))
            ps = ctorParams shape text
        if keyworded
          then if all (\(p, _, _) -> isJust (lookup p ps)) named then Just (shape, ps) else Nothing
          else if length ps == length args then Just (shape, ps) else Nothing
  (ctor, params) <- listToMaybe (mapMaybe ctorOf ms)
  let portNames = map ptName ports
      typed = if keyworded then [(p, ty, v, Just sep) | (p, sep, v) <- named, Just ty <- [lookup p params]] else [(p, ty, T.strip a, Nothing) | ((p, ty), a) <- zip params args]
      fakes = [(ty, cls, p, v, sep) | (p, ty, v, sep) <- typed, ty `elem` portNames, Just cls <- [classOf v]]
      classOf v = case reverse [c | (b, _) <- T.breakOnAll "(" v, let c = T.takeWhileEnd isIdent b, not (T.null c)] of
        c : _ -> Just c
        [] | T.all isIdent v, not (T.null v) -> snd <$> bindingOf v
        [] -> Nothing
      bindingOf v = listToMaybe [(l, c) | l <- T.lines tt, Just c <- [boundTo v l]]
  if null fakes then Nothing else Just ()
  -- the binding form, learned from the line binding a fake the call passes; none: no fake
  -- is recognised by its binding (form only)
  let bound' = fromMaybe "" (listToMaybe [learnBound v c l | (_, c, _, v, _) <- fakes, T.all isIdent v, Just (l, _) <- [bindingOf v]])
      prefixes = nub [pre | (ty, c, _, _, _) <- fakes, Just pre <- [T.stripSuffix ty c], not (T.null pre)]
      keyword = listToMaybe ["{= param =}" <> sep <> "{= var =}" | (_, _, _, _, Just sep) <- fakes]
      classes = nub [c | (_, c, _, _, _) <- fakes]
      act = case [l | l <- T.lines tt, ("." <> unit <> "(") `T.isInfixOf` l] of
        l : _
          | "await " `T.isInfixOf` l -> "await"
          | ".await" `T.isInfixOf` l -> ".await"
        _ -> "plain"
  pure (Sibling tp owner [(ty, c, p) | (ty, c, p, _, _) <- fakes] act (FakeShape bound' classes prefixes keyword Nothing) ctor)
  where
    lastSegment = last . ("" :) . T.splitOn "."
    -- a call of N in T (not a method of something): its top-level arguments
    callArgs n t = listToMaybe [splitTopArgs (T.dropEnd 1 r') | (b, a) <- occurrencesWith False n t, maybe True (\(_, c) -> c /= '.') (T.unsnoc b), Just r <- [T.stripPrefix "(" a], Just r' <- [balanced r]]
    keywordArg a = do
      let a' = T.strip a
          (p, rest) = T.span isIdent a'
          r1 = T.stripStart rest
      (c, r2) <- T.uncons r1
      if T.null p || c `notElem` ['=', ':'] || "=" `T.isPrefixOf` r2 then Nothing else Just (p, T.singleton c, T.strip r2)
    -- @v = C(@ on a line: C
    boundTo v l = do
      let s = T.stripStart l
      r <- T.stripPrefix v s
      r2 <- T.stripPrefix "=" (T.stripStart r)
      if "=" `T.isPrefixOf` r2 then Nothing else Just ()
      let (c, r3) = T.span isIdent (T.stripStart r2)
      if T.null c || not ("(" `T.isPrefixOf` r3) then Nothing else Just c
    -- the line binding V to C, as an expression: V and C groups, spaces any
    learnBound v c l =
      let (indent, s) = T.span isSpace l
          afterV = T.drop (T.length v) s
          (between, _) = T.breakOn c afterV
       in (if T.null indent then "^" else "^[[:space:]]+") <> "([[:alnum:]_]+)" <> spaced between <> "([[:alnum:]_]+)\\("
    spaced t = T.concat [if T.all isSpace w then "[[:space:]]*" else escapeRe w | w <- T.groupBy (\a b -> isSpace a == isSpace b) t]
    ownerRe l n = "^" <> escapeRe (fst (T.breakOn n l)) <> "([[:alnum:]_]+)"

-- | The classes CONJURED (the test file being conjured) binds by the learned binding form
-- BOUND that stand for a port whatever their class: bound to a variable whose last word is a
-- port's last word (@repository = Mock()@), or configured by the test (an attribute of it
-- assigned: @thing.find_by_id = …@). A library double is no sibling's fake; this is how it is
-- known without a declared `classes`.
conjuredFakes :: Text -> [Port] -> Text -> [Text]
conjuredFakes bound' ports conjured
  | T.null bound' = []
  | otherwise = nub [c | l <- ls, Just [_, var, c] <- [groups bound' l], byName var || configured var]
  where
    ls = T.lines conjured
    lastWord = snd . T.breakOnEnd "_"
    portWords = map (lastWord . snake . ptName) ports
    byName var = lastWord (T.toLower var) `elem` portWords
    configured var = or [True | l <- ls, Just r <- [T.stripPrefix (var <> ".") (T.stripStart l)], let (n, r') = T.span isIdent r, not (T.null n), Just r'' <- [T.stripPrefix "=" (T.stripStart r')], not ("=" `T.isPrefixOf` r'')]

-- | Where a sibling is looked for, nearest first: REL itself, then the other source files of
-- its folder by name, then those of the folders beside it (under its parent) by distance,
-- then by name. Test files and files of another extension are left out.
siblingCandidates :: TestDef -> FilePath -> [FilePath] -> [FilePath]
siblingCandidates td rel0 files = rel : sortOn (\f -> (takeDirectory f /= dir, distance f, f)) others
  where
    rel = normalisePath rel0
    dir = takeDirectory rel
    up = takeDirectory dir
    ext = takeExtension rel
    under f = up == "." || (up <> "/") `isPrefixOf` f
    others = nub [f | f0 <- files, let f = normalisePath f0, f /= rel, under f, takeExtension f == ext, not (isTestFile td f)]
    distance f = length (splitDirectories (makeRelative up (takeDirectory f))) + (if takeDirectory f == dir then 0 else 1)

-- | The sibling of UNIT in the module (REL, its text, its test file's text): every candidate
-- ('siblingCandidates') whose test reads ('siblingShapes'), nearest first, merged. The
-- nearest gives the owner, the constructor, the keyword and the act; the fakes' classes and
-- prefixes are all of theirs, the binding form the nearest one's that shows one. The
-- constructor's files are the sibling's and the unit's folders under the service base.
findSibling :: Workspace IO -> Language -> [Port] -> (FilePath, Text, Maybe Text) -> Text -> IO (Maybe Sibling)
findSibling ws lang ports (rel0, moduleText, ownTest) unit = case langTest lang of
  Nothing -> pure Nothing
  Just _ | null ports || isNothingAnn -> pure Nothing
  Just td -> do
    let rel = normalisePath rel0
        up = takeDirectory (takeDirectory rel)
        ext = takeExtension rel
        cj = langConjure lang
        base = serviceBase cj (servicePaths cj rel)
    listed <- map (normalisePath . (up </>)) <$> listFiles ws up
    let readOne f = do
          let tf = testsForFile td f
          tt <- if f == rel then maybe (readText ws tf) (pure . Just) ownTest else readText ws tf
          st <- if f == rel then pure (Just moduleText) else readText ws f
          pure $ case (tt, st) of
            (Just tt', Just st')
              | not (null (testSourcesIn lang (Just tf) tt'))
              , Just sb <- siblingShapes lang ports unit (tf, tt', st') ->
                  let rd d = maybe d (`makeRelative` d) base
                      globs = nub [T.pack (rd (takeDirectory g) </> "**" </> ("*" <> ext)) | g <- [f, rel]]
                   in Just sb {sbCtor = (sbCtor sb) {ctFiles = globs}}
            _ -> Nothing
    found <- catMaybes <$> traverse readOne (siblingCandidates td rel listed)
    pure $ case found of
      sb : _ ->
        let all' = map sbFake found
            bound' = fromMaybe "" (listToMaybe [b | b <- map fsBound all', not (T.null b)])
         in Just sb {sbFake = (sbFake sb) {fsBound = bound', fsClasses = nub (concatMap fsClasses all'), fsPrefixes = nub (concatMap fsPrefixes all')}}
      [] -> Nothing
  where
    isNothingAnn = not (isJust (langUnitAnnotation lang))

-- | The service's ports for the module REL: the `[conjure.port]` shape over the files under
-- the `[conjure] service` folder; none without them.
servicePorts :: Workspace IO -> Language -> FilePath -> IO [Port]
servicePorts ws lang rel = case (serviceBase cj (servicePaths cj (normalisePath rel)), cj >>= cjPort) of
  (Just b, Just ps) -> do
    fs <- filesUnder ws b (psFiles ps)
    texts <- concat <$> traverse (\f -> maybe [] (pure . (f,)) <$> readText ws f) fs
    pure (portsIn ps texts)
  _ -> pure []
  where
    cj = langConjure lang

-- | The service's folder: the pack's `[conjure] service` with the wire's SETS in it; none
-- without one, or when a set it names is missing.
serviceBase :: Maybe ConjureDef -> [(Text, Text)] -> Maybe FilePath
serviceBase cj sets = do
  t <- cj >>= cjService
  segs <- traverse seg (T.splitOn "/" t)
  pure (joinPath (map T.unpack segs))
  where
    seg x = case T.stripSuffix "=}" =<< T.stripPrefix "{=" x of
      Just k -> lookup (T.strip k) sets
      Nothing -> Just x

-- | The files under BASE (root-relative) that one of GLOBS (relative to BASE) matches.
filesUnder :: Workspace IO -> FilePath -> [Text] -> IO [FilePath]
filesUnder ws base globs = do
  found <- listFiles ws base
  pure [normalisePath (base </> f) | f <- found, any (`matchesGlob` f) globs]

-- | Arguments split at top-level commas.
splitTopArgs :: Text -> [Text]
splitTopArgs t = filter (not . T.null) (map T.strip (go (0 :: Int) "" t))
  where
    go depth acc x = case T.uncons x of
      Nothing -> [acc]
      Just (c, rest)
        | c == ',', depth == 0 -> acc : go depth "" rest
        | c `elem` ['(', '[', '{'] -> go (depth + 1) (T.snoc acc c) rest
        | c `elem` [')', ']', '}'] -> go (depth - 1) (T.snoc acc c) rest
        | otherwise -> go depth (T.snoc acc c) rest

-- | TEXT as a literal in a POSIX expression.
escapeRe :: Text -> Text
escapeRe = T.concatMap (\c -> if c `elem` ("\\.[](){}*+?^$|" :: String) then T.pack ['\\', c] else T.singleton c)

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c == '_'
