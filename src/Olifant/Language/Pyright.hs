-- | pyright specifics for `complete` edits and checks (Python): the JSON of
-- `pyright --outputjson` on stdout. `reportMatchNotExhaustive` is a site at
-- its `match` line (the extent comes from the source, see 'supportSpans');
-- any other error is a broken file — except a function's "must return value
-- on all code paths", which the missing case itself causes.
module Olifant.Language.Pyright (pyrightSupport, parsePyrightReport, pythonPattern) where

import Data.Aeson (Value, eitherDecodeStrict, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum, isUpper)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language.Diagnostics

pyrightSupport :: CompilerSupport
pyrightSupport =
  CompilerSupport
    { supportFormat = "pyright-json"
    , supportParse = parsePyrightReport
    -- qualified as the site's own `case`s are (python review I2)
    , supportPattern = \d content _ p -> qualifyLike (siteLines d content) p
    , supportVisible = \_ _ _ -> True
    , supportFunction = pythonFunctionAt
    , supportArmSeparator = ""
    , supportReadsStdout = True
    , -- pyright points at the `match` line; the arms are the block under it
      supportSpans = Just extendSites
    }

-- | Each site from its `match` line to its last arm, read from the source.
extendSites :: Map FilePath Text -> CheckReport -> CheckReport
extendSites files r = r {reportMissing = map extend (reportMissing r)}
  where
    extend m = case M.lookup (missingFile m) files of
      Just src -> m {missingCase = (fst (missingCase m), indentBlockEnd (T.splitOn "\n" src) (fst (missingCase m)))}
      Nothing -> m

data Diag = Diag {dFile :: FilePath, dLine :: Int, dSeverity :: Text, dRule :: Maybe Text, dMessage :: Text}

parsePyrightReport :: ByteString -> Either Text CheckReport
parsePyrightReport raw = case eitherDecodeStrict raw >>= parseEither report of
  Left why -> Left ("pyright did not print its JSON report: " <> T.pack why)
  Right ds ->
    let sites = [MissingPatterns (dFile d) (dLine d, dLine d) (unhandled (dMessage d)) Nothing | d <- ds, dRule d == Just "reportMatchNotExhaustive"]
        withSites = map missingFile sites
        consequence d = dRule d == Just "reportReturnType" && "must return value on all code paths" `T.isInfixOf` dMessage d && dFile d `elem` withSites
        broken = [(dFile d, dMessage d) | d <- ds, dSeverity d == "error", dRule d /= Just "reportMatchNotExhaustive", not (consequence d)]
     in Right (CheckReport sites broken [])
  where
    report :: Value -> Parser [Diag]
    report = withObject "report" $ \o -> o .: "generalDiagnostics" >>= traverse diag
    diag = withObject "diagnostic" $ \d -> do
      start <- d .: "range" >>= withObject "range" (.: "start") >>= withObject "position" (.: "line")
      Diag <$> d .: "file" <*> pure start <*> d .: "severity" <*> d .:? "rule" <*> d .: "message"

-- | The patterns in `Unhandled type: "A | Literal[E.X, E.Y]"`.
unhandled :: Text -> [Text]
unhandled msg = case T.breakOn "Unhandled type: \"" (T.replace "\160" " " msg) of
  (_, rest) | not (T.null rest) -> concatMap pythonPatterns (T.splitOn " | " (T.takeWhile (/= '"') (T.drop (T.length "Unhandled type: \"") rest)))
  _ -> []
  where
    pythonPatterns t = case T.stripPrefix "Literal[" t of
      Just inner -> map T.strip (T.splitOn "," (T.dropWhileEnd (== ']') inner))
      Nothing -> [pythonPattern t]

-- | A type as a class pattern: `PartyTooLarge` → `PartyTooLarge()`; type
-- arguments dropped (`Ok[int]` → `Ok()`).
pythonPattern :: Text -> Text
pythonPattern t = T.takeWhile (/= '[') (T.strip t) <> "()"

-- | The lines of a site's `match`, from its first to its last arm.
siteLines :: MissingPatterns -> Text -> Text
siteLines d content =
  let (start, end) = missingCase d
   in T.unlines (take (end - start + 1) (drop start (T.lines content)))

-- | Written as the site writes the others: `case fx.SlotTaken():` there makes
-- `PartyTooLarge()` → `fx.PartyTooLarge()`.
qualifyLike :: Text -> Text -> Text
qualifyLike site p
  | "." `T.isInfixOf` T.takeWhile (/= '(') p = p
  | otherwise = maybe p (\q -> q <> "." <> p) (listToMaybe (mapMaybe qualifier (T.lines site)))
  where
    qualifier l = case T.stripPrefix "case " (T.stripStart l) of
      Just rest ->
        let name = T.takeWhile (\c -> isAlphaNum c || c == '_' || c == '.') rest
            (q, cls) = T.breakOnEnd "." name
         in if T.null q || T.null cls || not (isUpper (T.head cls)) then Nothing else Just (T.dropEnd 1 q)
      Nothing -> Nothing

-- | The nearest `def NAME` at or above a 0-based line.
pythonFunctionAt :: Text -> Int -> Text
pythonFunctionAt content line =
  fromMaybe "?" (listToMaybe [n | l <- reverse (take (line + 1) (T.lines content)), Just rest <- [T.stripPrefix "def " (T.stripStart l) `orElse` T.stripPrefix "async def " (T.stripStart l)], let n = T.takeWhile (\c -> isAlphaNum c || c == '_') rest, not (T.null n)])
  where
    orElse a b = maybe b Just a
