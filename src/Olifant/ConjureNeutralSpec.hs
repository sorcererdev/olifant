-- | Conjure holds no Python (milestone 4a): its modules name no Python
-- syntax, file name or message outside comments. Each literal here is one
-- the shapes moved to the language, the pack or the toolchain.
module Olifant.ConjureNeutralSpec (spec) where

import Control.Monad (forM)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Test.Hspec

-- | The modules conjure runs through.
modules :: [FilePath]
modules = ["src/Olifant/Server/Conjure.hs", "src/Olifant/Server/ConjureScan.hs", "src/Olifant/Server/ConjureNext.hs", "src/Olifant/Server/Site.hs", "src/Olifant/Server/Unit.hs", "src/Olifant/Language/Test.hs"]

literals :: [Text]
literals = ["\"def ", "\"class ", "__init__", "Protocol", "\".py\"", "__test.py", "\"assert ", "\"assert\"", "\"test_\"", "lambda", "\"self", "NameError", "AssertionError", "pyright", "pytest"]

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

spec :: Spec
spec = describe "conjure holds no Python" $
  it "its modules name no Python literal outside comments" $ do
    found <- concat <$> forM modules (\f -> (\t -> [(f, lit) | l <- code t, lit <- literals, lit `T.isInfixOf` l]) <$> TIO.readFile f)
    found `shouldBe` allowed
