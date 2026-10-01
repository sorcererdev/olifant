-- | GHC specifics for `complete` edits (spec §5.6): `-fdiagnostics-as-json`
-- output, where warning 62161 lists the patterns a `case` lacks.
module Olifant.Language.Ghc (ghcSupport, parseGhcReport, typeOf) where

import Data.Aeson (Value, eitherDecodeStrict, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language.Diagnostics

-- | GHC prints names as the site has them in scope, so patterns go in as
-- reported; a `complete` edit names its constructor `Type.Ctor`, visible
-- only where GHC reports that type.
ghcSupport :: CompilerSupport
ghcSupport =
  CompilerSupport
    { supportFormat = "ghc-json"
    , supportParse = parseGhcReport
    , supportPattern = \_ _ p -> p
    , supportVisible = \d _ constructor -> case (T.breakOnEnd "." constructor, missingType d) of
        (("", _), _) -> True
        (_, Nothing) -> True
        ((withDot, _), Just ty) -> snd (T.breakOnEnd "." (T.dropEnd 1 withDot)) == ty
    , supportFunction = declarationAt
    }

data Diag = Diag {dFile :: FilePath, dStart :: Int, dEnd :: Int, dSeverity :: Text, dCode :: Maybe Int, dMessage :: Text}

-- | Every JSON line of GHC's output; other lines (cabal's progress) are skipped.
parseGhcReport :: ByteString -> Either Text CheckReport
parseGhcReport raw = Right (foldr add (CheckReport [] [] []) diags)
  where
    diags = mapMaybe decodeLine (BC.lines raw)
    decodeLine l = case eitherDecodeStrict l of
      Right v -> parseMaybe diag v
      Left _ -> Nothing
    diag :: Value -> Parser Diag
    diag = withObject "diagnostic" $ \o -> do
      sp <- o .: "span"
      file <- sp .: "file"
      s <- sp .: "start" >>= (.: "line")
      e <- sp .: "end" >>= (.: "line")
      sev <- o .: "severity"
      code <- o .:? "code"
      msg <- o .: "message" :: Parser [Text]
      pure (Diag file s e sev code (T.intercalate "\n" msg))
    add d r@(CheckReport ms bs us)
      | dSeverity d == "Error" = CheckReport ms ((dFile d, firstLine (dMessage d)) : bs) us
      | dCode d == Just 62161, "not matched:" `T.isInfixOf` dMessage d =
          let site = MissingPatterns (dFile d) (dStart d - 1, dEnd d - 1) (patternsOf (dMessage d)) (typeOf (dMessage d))
           in case equationName (dMessage d) of
                Just f -> CheckReport ms bs ((site, f) : us)
                Nothing -> CheckReport (site : ms) bs us
      | otherwise = r
    firstLine t = case T.lines t of
      (x : _) -> T.strip x
      [] -> ""

-- | The patterns after "not matched:", split on top-level commas, without
-- GHC's truncation marker.
patternsOf :: Text -> [Text]
patternsOf msg =
  let after = T.unwords (map T.strip (T.lines (snd (T.breakOnEnd "not matched:" msg))))
   in filter (\p -> not (T.null p) && p /= "...") (map T.strip (splitTop after))

splitTop :: Text -> [Text]
splitTop = map T.pack . go (0 :: Int) "" . T.unpack
  where
    go _ acc [] = [reverse acc]
    go d acc (c : cs)
      | c == ',' && d == 0 = reverse acc : go d "" cs
      | c `elem` ("([{" :: String) = go (d + 1) (c : acc) cs
      | c `elem` (")]}" :: String) = go (max 0 (d - 1)) (c : acc) cs
      | otherwise = go d (c : acc) cs

-- | The scrutinee's type when the message names exactly one.
typeOf :: Text -> Maybe Text
typeOf msg = case T.breakOn " not matched" (snd (T.breakOnEnd "Patterns of type " msg)) of
  (ty, rest)
    | T.null rest || T.null ty -> Nothing
    | length (splitTop ty) > 1 -> Nothing
    | otherwise -> case T.words (T.filter (`notElem` ("`'\8216\8217" :: String)) ty) of
        (w : _) -> Just (snd (T.breakOnEnd "." w))
        [] -> Nothing

-- | "In an equation for `f':" → f (sites the loop cannot extend).
equationName :: Text -> Maybe Text
equationName msg = case T.breakOnEnd "In an equation for " msg of
  ("", _) -> Nothing
  (_, rest) -> Just (T.takeWhile (`notElem` ("'\8217:" :: String)) (T.dropWhile (`elem` ("`\8216" :: String)) rest))
