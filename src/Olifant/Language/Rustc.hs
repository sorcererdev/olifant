-- | rustc specifics for `complete` edits and checks (spec §5.6): cargo's
-- `--message-format=json` lines on stdout.
module Olifant.Language.Rustc (rustcSupport, parseRustcReport) where

import Data.Aeson (Value, eitherDecodeStrict, withObject, (.:))
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Olifant.Language.Diagnostics

rustcSupport :: CompilerSupport
rustcSupport =
  CompilerSupport
    { supportFormat = "rustc-json"
    , supportParse = parseRustcReport
    , supportPattern = \_ _ p -> p
    , supportVisible = \_ _ _ -> True
    , supportFunction = declarationAt
    , supportReadsStdout = True
    }

data Diag = Diag {dFile :: FilePath, dLevel :: Text, dMessage :: Text}

-- | Every `compiler-message` line; other lines (artifacts, build-finished)
-- are skipped. Errors are broken files.
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
      msg <- m .: "message"
      spans <- m .: "spans" :: Parser [Value]
      file <- case [sp | sp <- spans, parseMaybe (withObject "span" (.: "is_primary")) sp == Just True] of
        (sp : _) -> withObject "span" (.: "file_name") sp
        [] -> fail "no primary span"
      pure (Diag file level msg)
    add d r@(CheckReport ms bs us)
      | dLevel d == "error" = CheckReport ms ((dFile d, dMessage d) : bs) us
      | otherwise = r
