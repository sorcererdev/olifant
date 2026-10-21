-- | Internal to Olifant.Server: where a request at a cursor stands (the
-- module, its test file, the def at or above the cursor) and the helpers
-- the editor's commands share: the step log, a run over the editor's
-- files, the modules a file imports, a def's name.
module Olifant.Server.Site
  ( Site (..)
  , siteAt
  , siteFile
  , siteTestFile
  , functionAt
  , functionSpanAfter
  , importOf
  , importedContext
  , importedModules
  , appendStepLine
  , runOver
  , testBody
  , defLineAbove
  , defName
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
import Data.List (sort, stripPrefix)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Anchor (locateContainers)
import Olifant.App (Ports (..), Project (..))
import Olifant.Catalog (catalogLanguages)
import Olifant.Generation.FillPhase (projectFiles)
import Olifant.Language (Language (..), Syntax, TestId, TestRun, languageFor, runTestsIn, testsForFile)
import Olifant.Server.State (ServerState (..), appendStepLine, projectFor)
import Olifant.Workspace (Workspace (..), normalisePath)
import System.FilePath (dropExtension, makeRelative, splitDirectories, (</>))

-- | Where a command at a cursor stands: the module and its test
-- file (the cursor in either), their texts, and the def at or above the
-- cursor.
data Site = Site
  { siteRoot :: FilePath
  , siteProject :: Project
  , siteLang :: Language
  , siteModuleRel :: FilePath
  , siteTestRel :: FilePath
  , siteOnTest :: Bool
  , siteModuleText :: Text
  , siteTestText :: Maybe Text
  , siteAtCursor :: Maybe Text
  , -- | the line (1-based) of that def
    siteDefLine :: Maybe Int
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
      let above = defLineAbove line (if onTest then fromMaybe "" testText0 else moduleText)
      pure (Site root project lang moduleRel testRel onTest moduleText testText0 (snd <$> above) (fst <$> above))

-- | The function a cursor (1-based line) is in, in a module: the def at or
-- above it (its name, its line) while its body reaches the cursor, and that
-- body. The def is the one at that line, whatever else shares its name.
functionAt :: ServerState -> Site -> Int -> IO (Maybe (Text, Int, (Int, Int)))
functionAt s site line = case (siteOnTest site, siteAtCursor site, siteDefLine site) of
  (False, Just n, Just d) ->
    functionSpanAfter (pSyntax (ssPorts s)) (siteLang site) n d (siteModuleText site) <&> \case
      Just sp@(_, z) | line <= z -> Just (n, d, sp)
      _ -> Nothing
  _ -> pure Nothing

-- | The line importing a function into a new test file (the class it is a
-- method of, if any): Python's `from pkg.module import Name`, the module's
-- path less its `src/` folder; none for another language.
importOf :: Language -> FilePath -> Text -> Text -> Maybe Int -> Maybe Text
importOf lang moduleRel moduleText fn defLine
  | langName lang /= "python" = Nothing
  | otherwise = Just ("from " <> dotted <> " import " <> owner)
  where
    dotted = T.intercalate "." (map T.pack (splitDirectories (dropExtension (fromMaybe moduleRel (stripPrefix "src/" moduleRel)))))
    ls = T.lines moduleText
    -- the def at its line (another class may define the name too), else the first of the name
    isDef l = ("def " <> fn <> "(") `T.isPrefixOf` T.strip l
    (above, at) = case defLine of
      Just d | maybe False isDef (listToMaybe (drop (d - 1) ls)) -> splitAt (d - 1) ls
      _ -> break isDef ls
    owner = case at of
      d : _ | T.isPrefixOf " " d || T.isPrefixOf "\t" d -> case [l | l <- reverse above, not (T.null (T.strip l)), not (" " `T.isPrefixOf` l || "\t" `T.isPrefixOf` l)] of
        l : _ | Just c <- T.stripPrefix "class " l -> T.takeWhile ident c
        _ -> fn
      _ -> fn

-- | The project modules a draft sees beside its file and the file beside it
-- (KNOWN, never repeated): the Python files their TEXTS import, an absolute
-- import's module (or a name imported from it that is a module) found as
-- @src/a/b/c.py@ or @src/a/b/c/__init__.py@, then what those import (one
-- level, so the cap cuts these first); in import order, at most
-- 'importedFiles' of them, each at most 'importedLines' lines (cut with a
-- line saying how many more) and 'importedTotal' in all. The standard
-- library and third-party packages are no file of the project: skipped.
importedContext :: Workspace IO -> Language -> [FilePath] -> [Text] -> IO [(FilePath, Text)]
importedContext ws lang known texts
  | langName lang /= "python" = pure []
  | otherwise = do
      direct <- resolved known texts
      -- one level further: what those import (a port's models)
      further <- resolved (known ++ map fst direct) (map snd direct)
      pure (capped importedTotal (take importedFiles (direct ++ further)))
  where
    resolved seen ts = do
      found <- forM (concatMap importedModules ts) $ \m ->
        let path = T.unpack (T.intercalate "/" (T.splitOn "." m))
         in firstFile ["src" </> path <> ".py", "src" </> path </> "__init__.py"]
      pure (dedupe [(p, t) | Just (p, t) <- found, p `notElem` seen, not (T.null (T.strip t))])
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
                | keep < length ls = T.intercalate "\n" (take keep ls ++ ["# \8230 (" <> T.pack (show (length ls - keep)) <> " more lines)"])
                | otherwise = t
           in (p, cut) : capped (budget - keep) rest

-- | The modules a Python file's absolute imports name, in order: @import
-- a.b, c@ names @a.b@ and @c@; @from a.b import x, y@ names @a.b@, then
-- @a.b.x@ and @a.b.y@ (a name may be a module). Relative imports are not
-- followed.
importedModules :: Text -> [Text]
importedModules t = concatMap named (map T.strip (T.lines t))
  where
    named l
      | Just rest <- T.stripPrefix "from " l
      , (m, names) <- T.breakOn " import " rest
      , not (T.null names)
      , not ("." `T.isPrefixOf` m) =
          m : [m <> "." <> n | n <- map nameOf (T.splitOn "," (T.drop (T.length " import ") names)), not (T.null n)]
      | Just rest <- T.stripPrefix "import " l = filter (not . T.null) (map nameOf (T.splitOn "," rest))
      | otherwise = []
    -- a name less its alias, brackets and comment
    nameOf = T.takeWhile (\c -> ident c || c == '.') . T.dropWhile (`elem` (" (" :: String)) . T.takeWhile (/= '#')

-- | A draft's imported modules: how many files, lines each, lines in all.
importedFiles, importedLines, importedTotal :: Int
importedFiles = 8
importedLines = 120
importedTotal = 600

-- | The body (1-based) of the function NAME whose def is on line D: the
-- first of the name's bodies that starts at or after it.
functionSpanAfter :: Syntax IO -> Language -> Text -> Int -> Text -> IO (Maybe (Int, Int))
functionSpanAfter syntax lang name d text = either (const Nothing) (\cs -> listToMaybe (sort [(a + 1, z + 1) | (a, z) <- cs, a + 1 >= d])) <$> locateContainers syntax lang ["function", name] text

-- | Tests (ids, or test files) run over the project's files as the editor has them.
runOver :: ServerState -> Workspace IO -> Language -> [TestId] -> IO (Either Text TestRun)
runOver s ws lang ids = do
  (_, files) <- projectFiles ws (M.empty :: Map FilePath Text) lang (map (T.unpack . fileOf) ids)
  runTestsIn (pTester (ssPorts s) (ssCancel s)) lang ids files

-- | A test's lines: its def to the next line at or left of the def's indentation.
testBody :: Text -> Text -> [Text]
testBody name text = case dropWhile (not . isDef) (T.lines text) of
  [] -> []
  d : rest -> takeWhile (\l -> T.null (T.strip l) || indentOf l > indentOf d) rest
  where
    isDef l = ("def " <> name <> "(") `T.isPrefixOf` T.strip l
    indentOf = T.length . T.takeWhile (== ' ')

-- | The def at or above a line (1-based): its line and name.
defLineAbove :: Int -> Text -> Maybe (Int, Text)
defLineAbove line text = listToMaybe [(i, n) | (i, Just n) <- reverse (take line (zip [1 ..] (map defName (T.lines text))))]

defName :: Text -> Maybe Text
defName l = do
  rest <- T.stripPrefix "def " (fromMaybe stripped (T.stripPrefix "async " stripped))
  let n = T.takeWhile ident rest
  if not (T.null n) && "(" `T.isPrefixOf` T.drop (T.length n) rest then Just n else Nothing
  where
    stripped = T.strip l

ident :: Char -> Bool
ident c = isAlphaNum c || c == '_'

-- | A test id's file (`file::name`), and its name.
fileOf, testName :: TestId -> Text
fileOf = fst . T.breakOn "::"
testName = snd . T.breakOnEnd "::"

tshow :: Show a => a -> Text
tshow = T.pack . show
