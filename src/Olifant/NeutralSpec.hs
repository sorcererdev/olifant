-- | The engine branches on no language (design: "no language names in
-- logic"): a language's name appears in messages only. A line comparing
-- `langName` with a literal, or `python ||`, outside the allowlist fails.
module Olifant.NeutralSpec (spec) where

import Control.Monad (filterM, forM)
import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Text.Regex.TDFA ((=~))

-- | Lines that still branch on a language, each with the milestone that
-- removes it.
allowed :: [(FilePath, Text)]
allowed =
  [ -- milestone 3: [test] shapes (a fill that answers a whole file)
    ("src/Olifant/Generation/FillPhase.hs", "answersAFile s f = linesKind s && fmap langName (langOf (holeFile s)) == Just \"python\" && looksLikeFile (M.findWithDefault \"\" (holeFile s) base) (snd (splitImports (importsOf (holeFile s)) f))")
  ]

branches :: Text -> Bool
branches l = T.unpack l =~ ("langName[^\n]*(==|/=)[[:space:]]*(Just[[:space:]]+)?\"" :: String) || T.unpack l =~ ("python[[:space:]]*\\|\\|" :: String)

sources :: FilePath -> IO [FilePath]
sources dir = do
  entries <- map (dir </>) <$> listDirectory dir
  dirs <- filterM doesDirectoryExist entries
  nested <- concat <$> mapM sources [d | d <- dirs, not ("testdata" `isSuffixOf` d)]
  pure ([f | f <- entries, f `notElem` dirs, ".hs" `isSuffixOf` f, not ("Spec.hs" `isSuffixOf` f)] ++ nested)

spec :: Spec
spec = describe "no language branches in the engine" $
  it "only the allowlisted lines compare langName with a literal" $ do
    files <- sources "src"
    found <- concat <$> forM files (\f -> (\t -> [(f, T.strip l) | l <- T.lines t, branches l]) <$> TIO.readFile f)
    sort found `shouldBe` sort allowed
