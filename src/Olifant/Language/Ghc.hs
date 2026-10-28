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
-- where that type appears anywhere in the type GHC reports (`T`, `Maybe T`,
-- `(T, Bool)`).
ghcSupport :: CompilerSupport
ghcSupport =
  CompilerSupport
    { supportFormat = "ghc-json"
    , supportParse = parseGhcReport
    , supportPattern = \_ _ _ p -> p
    , supportVisible = \d _ constructor -> case (T.breakOnEnd "." constructor, missingType d) of
        (("", _), _) -> True
        (_, Nothing) -> True
        ((withDot, _), Just ty) -> snd (T.breakOnEnd "." (T.dropEnd 1 withDot)) `elem` typeNames ty
    , supportUndefined = const Nothing
    , supportFunction = declarationAt
    , supportReadsStdout = False
    , supportSpans = Nothing
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
    -- 62161 first: under -Werror it arrives with severity Error.
    add d r@(CheckReport ms bs us)
      | dCode d == Just 62161, "not matched:" `T.isInfixOf` dMessage d =
          let site = MissingPatterns (dFile d) (dStart d - 1, dEnd d - 1) (patternsOf (dMessage d)) (typeOf (dMessage d))
           in case contextOf (dMessage d) of
                Nothing -> CheckReport (site : ms) bs us
                Just described -> CheckReport ms bs ((site, described) : us)
      | dSeverity d == "Error" = CheckReport ms ((dFile d, firstLine (dMessage d)) : bs) us
      | otherwise = r
    firstLine t = case T.lines t of
      (x : _) -> T.strip x
      [] -> ""

-- | The patterns GHC lists after "not matched:": the rest of that line when
-- there is one, otherwise one per following line (GHC 9.10 prints them
-- stacked, never comma-separated); its truncation marker `...` is dropped.
patternsOf :: Text -> [Text]
patternsOf msg =
  let ls = drop 1 (dropWhile (not . ("not matched:" `T.isInfixOf`)) (T.lines msg))
      sameLine = T.strip (snd (T.breakOnEnd "not matched:" (head' (filter ("not matched:" `T.isInfixOf`) (T.lines msg)))))
      stacked = takeWhile (not . T.null) (map T.strip ls)
   in filter (/= "...") (if T.null sameLine then stacked else [sameLine])
  where
    head' = \case (x : _) -> x; [] -> ""

-- | The scrutinee's type as GHC prints it, without quotes (`Maybe T`,
-- `(T, Bool)`, `M.T`).
typeOf :: Text -> Maybe Text
typeOf msg = case T.breakOn " not matched" (snd (T.breakOnEnd "Patterns of type " msg)) of
  (ty, rest)
    | T.null rest || T.null (T.strip ty) -> Nothing
    | otherwise -> Just (T.strip (T.filter (`notElem` ("`'\8216\8217" :: String)) ty))


-- | Where GHC found the gap, when it is not a `case` the loop can extend:
-- "an equation for `f`", "a lambda abstraction", "a pattern binding", ….
-- Nothing for case and \case alternatives.
contextOf :: Text -> Maybe Text
contextOf msg = case [T.strip l | l <- T.lines msg, "In " `T.isPrefixOf` T.strip l] of
  (l : _)
    | any (`T.isPrefixOf` l) ["In a case alternative", "In a \\case alternative", "In a \\cases alternative"] -> Nothing
    | otherwise -> Just (T.map ascii (T.dropWhileEnd (== ':') (T.drop 3 l)))
  [] -> Nothing
  where
    ascii c = if c `elem` ("'\8216\8217" :: String) then '`' else c
