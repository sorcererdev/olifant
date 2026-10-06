-- | rustc specifics for `complete` edits and checks (spec §5.6): cargo's
-- `--message-format=json` lines on stdout. Error E0004 ("non-exhaustive
-- patterns") is a site: from the `match`'s scrutinee to its last arm (the
-- line of rustc's own "add a match arm" suggestion).
module Olifant.Language.Rustc (rustcSupport, parseRustcReport, rustFunctionAt) where

import Control.Applicative ((<|>))
import Data.Aeson (Value, eitherDecodeStrict, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language.Diagnostics

-- | Patterns go in without the reference rustc prints for a `&T` scrutinee
-- (match ergonomics); a `complete` edit names its variant `Type::Variant`
-- (or `Type.Variant`), visible where that type is matched.
rustcSupport :: CompilerSupport
rustcSupport =
  CompilerSupport
    { supportFormat = "rustc-json"
    , supportParse = parseRustcReport
    , supportPattern = \_ _ _ p -> fromMaybe p (T.stripPrefix "&mut " p <|> T.stripPrefix "&" p)
    , supportVisible = \d _ constructor -> case (qualifier constructor, missingType d) of
        (Nothing, _) -> True
        (_, Nothing) -> True
        (Just ty, Just t) -> ty `elem` typeNames t
    , supportUndefined = const Nothing
    , supportFunction = rustFunctionAt
    , supportArmSeparator = ","
    , supportReadsStdout = True
    , supportLoneArmSpaced = False
    , supportSpans = Nothing
    }

data Diag = Diag
  { dFile :: FilePath
  , dLine :: Int
  , dLevel :: Text
  , dCode :: Maybe Text
  , dMessage :: Text
  , dLabel :: Text
  , dLastArm :: Maybe Int
  , dType :: Maybe Text
  }

-- | Every `compiler-message` line; other lines (artifacts, build-finished)
-- are skipped. E0004 is a site, E0005 (a refutable `let`) a site olifant
-- cannot extend, any other error a broken file.
parseRustcReport :: ByteString -> Either Text CheckReport
parseRustcReport raw = Right (foldr add (CheckReport [] [] []) (mapMaybe decodeLine (BC.lines raw)))
  where
    decodeLine l = either (const Nothing) (parseMaybe diag) (eitherDecodeStrict l)
    diag :: Value -> Parser Diag
    diag = withObject "line" $ \o -> do
      reason <- o .: "reason"
      if reason /= ("compiler-message" :: Text) then fail "not a diagnostic" else pure ()
      m <- o .: "message"
      level <- m .: "level"
      code <- m .:? "code" >>= maybe (pure Nothing) (withObject "code" (.:? "code"))
      msg <- m .: "message"
      spans <- m .: "spans" :: Parser [Value]
      (file, line, label) <- case [sp | sp <- spans, parseMaybe (withObject "span" (.: "is_primary")) sp == Just True] of
        (sp : _) -> withObject "span" (\s -> (,,) <$> s .: "file_name" <*> s .: "line_start" <*> (fromMaybe "" <$> s .:? "label")) sp
        [] -> fail "no primary span"
      children <- m .: "children" :: Parser [Value]
      let suggestions = concat (mapMaybe (parseMaybe (withObject "child" (.: "spans"))) children) :: [Value]
          lastArm = listToMaybe [l | sp <- suggestions, Just (l, Just r) <- [parseMaybe (withObject "span" (\s -> (,) <$> s .: "line_start" <*> s .:? "suggested_replacement")) sp], ("," :: Text) `T.isPrefixOf` r]
          ty = listToMaybe [t | c <- children, Just cm <- [parseMaybe (withObject "child" (.: "message")) c], Just t <- [matchedType cm]]
      pure (Diag file line level code msg label lastArm ty)
    add d (CheckReport ms bs us) = case dCode d of
      Just "E0004" | Just lastArm <- dLastArm d ->
        CheckReport (MissingPatterns (dFile d) (dLine d - 1, lastArm - 1) (patternsIn (dMessage d)) (dType d) : ms) bs us
      Just "E0005" ->
        CheckReport ms bs ((MissingPatterns (dFile d) (dLine d - 1, dLine d - 1) (patternsIn (dLabel d)) (dType d), "a `let` binding") : us)
      _ | dLevel d == "error" -> CheckReport ms ((dFile d, dMessage d) : bs) us
      _ -> CheckReport ms bs us

-- | "the matched value is of type `&Failure`" → &Failure.
matchedType :: Text -> Maybe Text
matchedType t = case T.breakOn "the matched value is of type `" t of
  (_, rest) | not (T.null rest) -> Just (T.takeWhile (/= '`') (T.drop (T.length "the matched value is of type `") rest))
  _ -> Nothing

-- | The backquoted patterns in "non-exhaustive patterns: `A`, `B` and 2 more
-- not covered". rustc lists three; once those are placed, the check loop's
-- next round lists the rest (spec §5.6).
patternsIn :: Text -> [Text]
patternsIn t = [p | (i, p) <- zip [0 :: Int ..] (T.splitOn "`" t), odd i]

-- | `Type::Variant` or `Type.Variant` → Type.
qualifier :: Text -> Maybe Text
qualifier c = case T.splitOn "::" (T.replace "." "::" c) of
  parts@(_ : _ : _) -> Just (last (init parts))
  _ -> Nothing

-- | The nearest `fn NAME` at or above a 0-based line.
rustFunctionAt :: Text -> Int -> Text
rustFunctionAt content line =
  case [n | l <- reverse (take (line + 1) (T.lines content)), let (_, rest) = T.breakOn "fn " l, not (T.null rest), let n = T.takeWhile (\ch -> isAlphaNum ch || ch == '_') (T.drop 3 rest), not (T.null n)] of
    (n : _) -> n
    [] -> "?"
