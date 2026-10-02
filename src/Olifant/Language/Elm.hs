-- | Elm specifics for `complete` edits (spec §5.6): `elm make --report=json`
-- diagnostics, and how a constructor is written at a site.
module Olifant.Language.Elm (elmSupport, parseElmReport, qualifyPattern, visibleAt) where

import Control.Applicative ((<|>))
import Data.Aeson (Value (..), eitherDecodeStrict, withObject, (.:))
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, isSpace)
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Language.Diagnostics (CheckReport (..), CompilerSupport (..), declarationAt, missing)

-- | The `MISSING PATTERNS` problems in a report (stderr of `elm make
-- --report=json`), with the paths as printed. Empty input means a clean
-- compile; a non-compile error (bad elm.json, …) is a failure.
parseElmReport :: ByteString -> Either Text CheckReport
parseElmReport raw
  | BS.all (`elem` [9, 10, 13, 32]) raw = Right (CheckReport [] [] [])
  | otherwise = case eitherDecodeStrict raw of
      Left err -> Left ("unreadable elm report: " <> T.pack err)
      Right v -> either (Left . T.pack) id (parseEither report v)
  where
    report = withObject "report" $ \o -> do
      ty <- o .: "type" :: Parser Text
      case ty of
        "compile-errors" -> Right . collect . concat <$> ((o .: "errors" :: Parser [Value]) >>= traverse errorsIn)
        _ -> do
          title <- o .: "title"
          msg <- o .: "message"
          pure (Left (title <> ": " <> messageText msg))
    errorsIn = withObject "error" $ \e -> do
      path <- e .: "path"
      problems <- e .: "problems" :: Parser [Value]
      traverse (problem path) problems
    collect found = CheckReport [m | Right m <- found] [b | Left b <- found] []
    problem path = withObject "problem" $ \p -> do
      title <- p .: "title" :: Parser Text
      if title /= "MISSING PATTERNS"
        then pure (Left (path, title))
        else do
          region <- p .: "region"
          s <- region .: "start" >>= (.: "line")
          e <- region .: "end" >>= (.: "line")
          msg <- p .: "message"
          pure (Right (missing path (s, e) (patternsIn (messageText msg))))
    patternsIn t =
      let after = drop 1 (dropWhile (not . ("Missing possibilities include:" `T.isInfixOf`)) (T.lines t))
       in map T.strip (takeWhile (not . T.null . T.strip) (dropWhile (T.null . T.strip) after))

-- | Elm messages are a list of strings and styled `{ "string": … }` chunks.
messageText :: Value -> Text
messageText = \case
  Array xs -> T.concat (map piece (toList xs))
  String s -> s
  _ -> ""
  where
    piece = \case
      String s -> s
      Object o -> maybe "" id (parseMaybe (.: "string") o)
      _ -> ""

-- | `elm make --report=json`: unqualified patterns, qualified per site; a
-- constructor named `Module.Ctor` is visible where that module is (imported).
elmSupport :: CompilerSupport
elmSupport =
  CompilerSupport
    { supportFormat = "elm-json"
    , supportParse = parseElmReport
    , supportPattern = qualifyPattern
    , supportVisible = \_ site constructor -> visibleAt site constructor
    , supportFunction = declarationAt
    , supportArmSeparator = ""
    , supportReadsStdout = False
    }

-- | @qualifyPattern site constructor pattern@: write the constructor (as
-- given in the `complete` edit, e.g. @Route.Settings@) the way the site's
-- module refers to it, inside the compiler's unqualified pattern.
qualifyPattern :: Text -> Text -> Text -> Text
qualifyPattern site constructor pattern = case T.breakOnEnd "." constructor of
  ("", bare) -> replaceWord bare bare pattern
  (withDot, bare) -> replaceWord bare (qualified (T.dropEnd 1 withDot) bare) pattern
  where
    ls = T.lines site
    siteModule = listToMaybe [T.takeWhile (not . isSpace) (T.strip rest) | l <- ls, Just rest <- [T.stripPrefix "module " l <|> T.stripPrefix "port module " l]]
    qualified m bare
      | siteModule == Just m = bare
      | otherwise = case [T.words rest | l <- ls, Just rest <- [T.stripPrefix ("import " <> m) l], T.null rest || " " `T.isPrefixOf` rest] of
          (ws : _)
            | "(..)" `elem` ws -> bare
            | ("as" : alias : _) <- dropWhile (/= "as") ws -> alias <> "." <> bare
          _ -> m <> "." <> bare

-- | Can a constructor written @M.C@ appear at this site? Only in module @M@
-- or a module importing it (Elm has no cyclic imports, so `Route` never sees
-- `Main`'s constructors). An unqualified constructor could be anywhere.
visibleAt :: Text -> Text -> Bool
visibleAt site constructor = case T.breakOnEnd "." constructor of
  ("", _) -> True
  (withDot, _) ->
    let m = T.dropEnd 1 withDot
        ls = T.lines site
        isModule = any (\l -> any (\p -> maybe False ((== m) . T.takeWhile (not . isSpace) . T.strip) (T.stripPrefix p l)) ["module ", "port module "]) ls
        imports = any (\l -> maybe False (\r -> T.null r || " " `T.isPrefixOf` r) (T.stripPrefix ("import " <> m) l)) ls
     in isModule || imports

-- | Replace whole identifier runs equal to @old@ (dots count as part of a run).
replaceWord :: Text -> Text -> Text -> Text
replaceWord old new = T.concat . map swap . T.groupBy (\a b -> ident a == ident b)
  where
    ident c = isAlphaNum c || c == '_' || c == '.'
    swap w = if w == old then new else w
