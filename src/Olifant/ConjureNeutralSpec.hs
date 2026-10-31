-- | Conjure holds no Python (milestone 4a): its modules name no Python
-- syntax, file name or message outside comments. Each literal here is one
-- the shapes moved to the language, the pack or the toolchain. And the
-- engine holds no pack (no-pack-in-engine): no source names a shipped
-- pack, one of their generators, or a convention one pack declares.
module Olifant.ConjureNeutralSpec (spec) where

import Control.Monad (filterM, forM)
import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesDirectoryExist, listDirectory)
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
  let named p = if '-' `elem` p then [T.pack p] else ["\"" <> T.pack p <> "\"", "\"" <> T.pack p <> "/"]
  pure (concatMap named packs ++ [T.pack g | g <- sort gens, '-' `elem` g] ++ ["def command(", "def query(", "booking_repository"])

-- | Engine lines that still name one, each with the change that removes it.
packAllowed :: [(FilePath, Text)]
packAllowed =
  -- the migrate (strangler) subsystem: its own follow-up, see the no-pack-in-engine report
  [ ("src/Olifant/App.hs", "python-strangler")
  , ("src/Olifant/Cli.hs", "python-strangler")
  , ("src/Olifant/Cli.hs", "python-strangler")
  , ("src/Olifant/Cli.hs", "python-strangler")
  , ("src/Olifant/Cli.hs", "python-strangler")
  , ("src/Olifant/Cli/Options.hs", "python-htmx")
  , ("src/Olifant/Cli/Options.hs", "python-htmx")
  , ("src/Olifant/Server/Actions.hs", "extract-command")
  , ("src/Olifant/Server/Actions.hs", "extract-command")
  , ("src/Olifant/Server/Actions.hs", "extract-query")
  , ("src/Olifant/Server/Actions.hs", "extract-query")
  , ("src/Olifant/Server/Preview.hs", "extract-command")
  , ("src/Olifant/Server/Preview.hs", "extract-query")
  ]

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
  describe "the engine holds no pack" $
    it "no source names a shipped pack, its generators or its conventions outside comments" $ do
      lits <- packLiterals
      files <- (++) <$> sources "src" <*> sources "app"
      found <- concat <$> forM (sort files) (\f -> (\t -> [(f, lit) | l <- code t, lit <- lits, lit `T.isInfixOf` l]) <$> TIO.readFile f)
      sort found `shouldBe` sort packAllowed
