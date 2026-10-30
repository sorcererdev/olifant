-- | Internal to Olifant.Server: where a request at a cursor stands (the
-- module and its test file) and the helpers the editor's commands share:
-- the step log, a run over the editor's files, the modules a file imports.
module Olifant.Server.Site
  ( Site (..)
  , siteAt
  , siteFile
  , siteTestFile
  , importOf
  , importedContext
  , importedModules
  , appendStepLine
  , runOver
  , ident
  , fileOf
  , testName
  , tshow
  ) where

import Control.Monad (forM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Char (isAlphaNum)
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Anchor (locateContainers)
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (catalogLanguages)
import Olifant.Generation.FillPhase (projectFiles)
import Olifant.Language (ImportsDef (..), Language (..), ModuleOf (..), Resolve (..), Syntax, TestId, TestRun, languageFor, moduleOf, runTestsIn, testsForFile)
import Olifant.Template (Value (..), parseTemplate, render)
import Text.Regex.TDFA (AllTextSubmatches (..), Regex, makeRegexM, matchM)
import Olifant.Server.State (ServerState (..), appendStepLine, projectFor)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (makeRelative, (</>))

-- | Where a command at a cursor stands: the module and its test
-- file (the cursor in either), and their texts.
data Site = Site
  { siteRoot :: FilePath
  , siteProject :: Project
  , siteLang :: Language
  , siteModuleRel :: FilePath
  , siteTestRel :: FilePath
  , siteOnTest :: Bool
  , siteModuleText :: Text
  , siteTestText :: Maybe Text
  }

siteFile, siteTestFile :: Site -> FilePath
siteFile site = normalisePath (siteRoot site </> siteModuleRel site)
siteTestFile site = normalisePath (siteRoot site </> siteTestRel site)

-- | The site of a cursor (1-based line) in a file: its project, its
-- language's tests, the module and the test file; or why there is none.
siteAt :: FilePath -> Int -> ServerState -> IO (ServerState, Either Text Site)
siteAt path line s0 =
  projectFor path s0 >>= \case
    (s, Nothing) -> pure (s, Left (T.pack path <> " is in no Olifant project"))
    (s, Just (root, project)) -> fmap (s,) . runExceptT $ do
      let rel = makeRelative root path
          ws = prWorkspace project
      lang <- maybe (throwE ("no language knows " <> T.pack rel)) pure (languageFor (catalogLanguages (prCatalog project)) rel)
      td <- maybe (throwE (langName lang <> " has no [test] table: Olifant cannot find its tests")) pure (langTest lang)
      paths <- lift (listFiles ws ".")
      let moduleRel = fromMaybe rel (listToMaybe [m | m <- paths, m /= rel, testsForFile td m == rel])
          onTest = moduleRel /= rel
          testRel = if onTest then rel else testsForFile td rel
      moduleText <- lift (readText ws moduleRel) >>= maybe (throwE ("cannot read " <> T.pack moduleRel)) pure
      testText0 <- lift (readText ws testRel)
      pure (Site root project lang moduleRel testRel onTest moduleText testText0)

-- | The line importing a unit into a test file, by the language's
-- `[imports] write` over the module of MODULEREL (`module_of`) and the
-- unit's owner: the first segment of its qualified name QUALIFIED (`Table`
-- for `Table.seats`). Nothing for a language with no `write`.
importOf :: Language -> FilePath -> Text -> Maybe Text
importOf lang moduleRel qualified = do
  im <- langImportsDef lang
  w <- imWrite im
  tp <- either (const Nothing) Just (parseTemplate w)
  let m = moduleOf (fromMaybe (ModuleOf [] [] ".") (imModuleOf im)) moduleRel
      owner = fst (T.breakOn "." qualified)
  either (const Nothing) Just (render (M.fromList [("module", VText m), ("owner", VText owner)]) tp)

-- | The project modules a conjure sees beside its file and the file beside it
-- (KNOWN, never repeated): the modules the import lines of TEXTS name (by
-- the language's `[[imports.resolve]]`), each found as the first of its
-- `[imports] paths` that is a file, then what those import (one level, so
-- the cap cuts these first); in import order, at most 'importedFiles' of
-- them, each at most 'importedLines' lines (cut with a comment saying how
-- many more) and 'importedTotal' in all. A module that is no file of the
-- project (the standard library, a dependency) is skipped. A language with
-- no `resolve` sees none.
importedContext :: Workspace IO -> Language -> [FilePath] -> [Text] -> IO [(FilePath, Text)]
importedContext ws lang known texts = case langImportsDef lang of
  Just im | not (null (imResolve im)) -> do
    let sep = maybe "." moSeparator (imModuleOf im)
        resolved seen ts = do
          found <- forM (concatMap (importedModulesWith (langComment lang) sep (imResolve im)) ts) $ \m ->
            let path = T.intercalate "/" (filter (not . T.null) (T.splitOn sep m))
             in firstFile [T.unpack p | Right tp <- map parseTemplate (imPaths im), Right p <- [render (M.singleton "path" (VText path)) tp]]
          pure (dedupe [(p, t) | Just (p, t) <- found, p `notElem` seen, not (T.null (T.strip t))])
    direct <- resolved known texts
    -- one level further: what those import (a port's models)
    further <- resolved (known ++ map fst direct) (map snd direct)
    pure (capped importedTotal (take importedFiles (direct ++ further)))
  _ -> pure []
  where
    firstFile = \case
      [] -> pure Nothing
      p : rest -> readText ws p >>= maybe (firstFile rest) (pure . Just . (p,))
    dedupe = foldr (\x acc -> x : filter ((/= fst x) . fst) acc) []
    capped _ [] = []
    capped budget ((p, t) : rest)
      | budget <= 0 = []
      | otherwise =
          let ls = T.lines t
              keep = minimum [length ls, importedLines, budget]
              cut
                | keep < length ls = T.intercalate "\n" (take keep ls ++ [langComment lang <> " \8230 (" <> T.pack (show (length ls - keep)) <> " more lines)"])
                | otherwise = t
           in (p, cut) : capped (budget - keep) rest

-- | The modules a file's import lines name, in order, by RESOLVES (the
-- first that matches a stripped line): its `module` group's modules
-- (comma-separated, each less its alias, brackets and comment), then each
-- of its `names` joined to the module by @.@ (a name may be a module).
-- Python's: @import a.b, c@ names @a.b@ and @c@; @from a.b import x, y@
-- names @a.b@, then @a.b.x@ and @a.b.y@; a relative import names none.
importedModules :: Language -> [Resolve] -> Text -> [Text]
importedModules lang = importedModulesWith (langComment lang) "."

-- | 'importedModules' with the language's comment and module separator.
importedModulesWith :: Text -> Text -> [Resolve] -> Text -> [Text]
importedModulesWith comment sep resolves t = concatMap named (map T.strip (T.lines t))
  where
    compiled = [(rx, r) | r <- resolves, Just (rx :: Regex) <- [makeRegexM (T.unpack (rsPattern r))]]
    named l = case [(groups, r) | (rx, r) <- compiled, Just (AllTextSubmatches groups) <- [matchM rx (T.unpack l) :: Maybe (AllTextSubmatches [] String)]] of
      (groups, r) : _ ->
        let grp n = maybe "" T.pack (listToMaybe (drop n groups))
            mods = items (grp (rsModule r))
         in mods ++ [m <> sep <> n | m <- mods, Just g <- [rsNames r], n <- items (grp g)]
      [] -> []
    items = filter (not . T.null) . map nameOf . T.splitOn ","
    -- a name less its alias, brackets and comment
    nameOf = T.takeWhile (\c -> ident c || c == '.' || c == ':') . T.dropWhile (`elem` (" (" :: String)) . fst . T.breakOn comment

-- | A conjure's imported modules: how many files, lines each, lines in all.
importedFiles, importedLines, importedTotal :: Int
importedFiles = 8
importedLines = 120
importedTotal = 600

-- | Tests (ids, or test files) run over the project's files as the editor has them.
runOver :: ServerState -> Workspace IO -> Language -> [TestId] -> IO (Either Text TestRun)
runOver s ws lang ids = do
  (_, files) <- projectFiles ws (M.empty :: Map FilePath Text) lang (map (T.unpack . fileOf) ids)
  runTestsIn (pTester (ssPorts s) (ssCancel s)) lang ids files

ident :: Char -> Bool
ident c = isAlphaNum c || c == '_'

-- | A test id's file (`file::name`), and its name.
fileOf, testName :: TestId -> Text
fileOf = fst . T.breakOn "::"
testName = snd . T.breakOnEnd "::"

tshow :: Show a => a -> Text
tshow = T.pack . show
