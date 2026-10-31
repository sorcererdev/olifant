-- | Conjure holds no Python (milestone 4a): its modules name no Python
-- syntax, file name or message outside comments. Each literal here is one
-- the shapes moved to the language, the pack or the toolchain. The same
-- holds for the strangling, stub, fill-prompt, anticipation and doctor
-- modules (no-pack-in-migrate). And the engine holds no pack
-- (no-pack-in-engine, no-pack-in-migrate): no source names a shipped pack,
-- one of their generators or rules, or a name, file or convention a pack
-- declares (a strangler pack's migrate.toml).
module Olifant.ConjureNeutralSpec (spec) where

import Control.Monad (filterM, forM)
import Data.Char (isAlphaNum)
import Data.List (isSuffixOf, nub, sort)
import Data.Map.Strict qualified as M
import Olifant.Toml (Tree (..), parseToml)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Functor ((<&>))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

-- | The modules conjure runs through.
modules :: [FilePath]
modules = ["src/Olifant/Server/Conjure.hs", "src/Olifant/Server/ConjureScan.hs", "src/Olifant/Server/ConjureNext.hs", "src/Olifant/Server/Site.hs", "src/Olifant/Server/Unit.hs", "src/Olifant/Language/Test.hs"]

literals :: [Text]
literals = ["\"def ", "\"class ", "__init__", "Protocol", "\".py\"", "__test.py", "\"assert ", "\"assert\"", "\"test_\"", "lambda", "\"self", "NameError", "AssertionError", "pyright", "pytest", "\"#\""]
  -- python-htmx's service layout, once the port method's inputs
  ++ ["\"app\"", "\"service\""]

-- | Lines that still hold one, each with why.
allowed :: [(FilePath, Text)]
allowed = []

-- | The modules strangling, stubs, the fill prompt, anticipation,
-- recognition and doctor's rows run through.
neutralModules :: [FilePath]
neutralModules =
  [ "src/Olifant/App.hs", "src/Olifant/Migration.hs", "src/Olifant/Server/Actions.hs", "src/Olifant/Server/Preview.hs", "src/Olifant/Server/Fixes.hs"
  , "src/Olifant/Capabilities.hs", "src/Olifant/TestName.hs", "src/Olifant/Server/Stub.hs", "src/Olifant/Model.hs", "src/Olifant/Generation/FillPhase.hs"
  , "src/Olifant/Server/Anticipate.hs", "src/Olifant/Server/Recognize.hs", "src/Olifant/Cli/Options.hs"
  ]

-- | Python literals none of them names: conjure's (less @self@, a word
-- Rust shares) and those this branch moved into python-strangler, the
-- python language file and the pytest toolchain.
neutralLiterals :: [Text]
neutralLiterals =
  filter (/= "\"self") literals
    ++ ["\"import ", "\"from ", "\"async ", "datetime", "SWITCHES", "switches.on", "switches.py", ".commit(", ".save(", "db.rows", "db.row(", "\"global ", "\"True\"", "python"]

-- | A line less its comment (@--@ outside a string), and @{- … -}@ blocks dropped.
code :: Text -> [Text]
code = go False . T.lines
  where
    go _ [] = []
    go True (l : ls)
      | "-}" `T.isInfixOf` l = go False ls
      | otherwise = go True ls
    go False (l : ls)
      | "{-" `T.isPrefixOf` T.stripStart l, not ("-}" `T.isInfixOf` l) = go True ls
      | otherwise = lineCode l : go False ls
    lineCode = T.pack . strip False . T.unpack
    strip _ [] = []
    strip False ('-' : '-' : _) = []
    strip inS ('\\' : c : r) = '\\' : c : strip inS r
    strip inS ('"' : r) = '"' : strip (not inS) r
    strip inS (c : r) = c : strip inS r

-- | What no engine source may name, from the shipped packs themselves: a pack (in a string
-- literal, alone or before @/@; a hyphenated name anywhere), a generator id with a hyphen
-- (one word ids, @port@ or @query@, are the engine's own words too), and the conventions this
-- branch moved into python-htmx's language file.
packLiterals :: IO [Text]
packLiterals = do
  packs <- sort <$> (listDirectory "packs" >>= filterM (doesDirectoryExist . ("packs" </>)))
  gens <- fmap concat . forM packs $ \p -> do
    let dir = "packs" </> p </> "generators"
    there <- doesDirectoryExist dir
    if there then listDirectory dir else pure []
  -- each pack's rule ids (`[rules.ID]`) and its migrate.toml's values
  rules <- fmap concat . forM packs $ \p -> fileLines ("packs" </> p </> "rules.toml") <&> \ls ->
    [rid | l <- ls, Just rest <- [T.stripPrefix "[rules." (T.strip l)], Just rid <- [T.stripSuffix "]" rest], not ("." `T.isInfixOf` rid)]
  declared <- fmap concat . forM packs $ \p -> do
    let path = "packs" </> p </> "migrate.toml"
    there <- doesFileExist path
    if there then either (const []) leaves . parseToml <$> TIO.readFile path else pure []
  let named p = if '-' `elem` p then [T.pack p] else ["\"" <> T.pack p <> "\"", "\"" <> T.pack p <> "/"]
  pure . nub $
    concatMap named packs
      ++ [T.pack g | g <- sort gens, '-' `elem` g]
      ++ [r | r <- rules, "-" `T.isInfixOf` r]
      ++ concatMap distinctive declared
      ++ ["def command(", "def query(", "booking_repository"]
  where
    fileLines f = doesFileExist f >>= \there -> if there then T.lines <$> TIO.readFile f else pure []
    leaves = concatMap leaf . M.elems
    leaf = \case
      TText t -> [t]
      TList ts -> concatMap leaf ts
      TTable t -> leaves t
      _ -> []
    -- a value that is more than one plain word (a word, `command`, `file`
    -- or `True`, is the engine's too), and a glob's file name
    distinctive v
      | T.all (\c -> isAlphaNum c || c == '_') v = []
      | otherwise = v : [f | let f = last (T.splitOn "/" v), f /= v, not (T.any (`elem` ("*{" :: String)) f), "." `T.isInfixOf` f]

-- | The engine's sources: every module under DIR but specs and test data.
sources :: FilePath -> IO [FilePath]
sources dir = do
  entries <- map (dir </>) <$> listDirectory dir
  dirs <- filterM doesDirectoryExist entries
  nested <- concat <$> mapM sources [d | d <- dirs, not ("testdata" `isSuffixOf` d)]
  pure ([f | f <- entries, f `notElem` dirs, ".hs" `isSuffixOf` f, not ("Spec.hs" `isSuffixOf` f)] ++ nested)

spec :: Spec
spec = do
  describe "conjure holds no Python" $
    it "its modules name no Python literal outside comments" $ do
      found <- concat <$> forM modules (\f -> (\t -> [(f, lit) | l <- code t, lit <- literals, lit `T.isInfixOf` l]) <$> TIO.readFile f)
      found `shouldBe` allowed
  describe "strangling, stubs, the fill prompt, anticipation and doctor hold no Python" $
    it "their modules name no Python literal outside comments" $ do
      found <- concat <$> forM neutralModules (\f -> (\t -> [(f, lit) | l <- code t, lit <- neutralLiterals, lit `T.isInfixOf` l]) <$> TIO.readFile f)
      found `shouldBe` []
  describe "the engine holds no pack" $
    it "no source names a shipped pack, its generators or its conventions outside comments" $ do
      lits <- packLiterals
      files <- (++) <$> sources "src" <*> sources "app"
      found <- concat <$> forM (sort files) (\f -> (\t -> [(f, lit) | l <- code t, lit <- lits, lit `T.isInfixOf` l]) <$> TIO.readFile f)
      found `shouldBe` []
