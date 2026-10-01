{-# LANGUAGE MultiWayIf #-}

module Olifant.Template.Parse (parseTemplate) where

import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Olifant.Naming (allCases, caseName)
import Olifant.Suggest (closest)
import Olifant.Template.Error
import Olifant.Template.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Piece = TLit Text | TOut Int Text | TTag Int Text

data Tag = TIf Expr | TElif Expr | TElse | TFor Text Expr | TEnd

parseTemplate :: Text -> Either TemplateError Template
parseTemplate src = first hint $ do
  pieces <- lexTemplate src
  (segs, stop, _) <- block pieces
  case stop of
    Nothing -> Right (Template src segs)
    Just (line, tag) -> Left (SyntaxError line ("unexpected `" <> tagKeyword tag <> "`"))
  where
    -- Filters as function calls (`pascalCase(name)`) or after a colon
    -- (`name:pascal`) are the usual guesses; say how they are written.
    hint = \case
      SyntaxError line msg
        | Just (f, v) <- callLike src -> SyntaxError line (msg <> "\nfilters are written after a bar: {= " <> v <> "|" <> f <> " =}")
      other -> other

-- | The first @{= f(x) =}@ or @{= x:f =}@ in a template, as (filter, value),
-- with the filter name mapped to a real one when it is a known alias.
callLike :: Text -> Maybe (Text, Text)
callLike src = case [e | piece <- drop 1 (T.splitOn "{=" src), let e = T.strip (fst (T.breakOn "=}" piece))] of
  es -> case [r | e <- es, Just r <- [call e, colon e]] of
    r : _ -> Just r
    [] -> Nothing
  where
    call e = case T.breakOn "(" e of
      (f, rest) | not (T.null rest), T.isSuffixOf ")" rest, isWord f -> Just (filterFor f, T.strip (T.dropEnd 1 (T.drop 1 rest)))
      _ -> Nothing
    colon e = case T.splitOn ":" e of
      [v, f] | isWord (T.strip v), isWord (T.strip f) -> Just (filterFor (T.strip f), T.strip v)
      _ -> Nothing
    isWord w = not (T.null w) && T.all (\c -> isAlphaNum c || c == '_') w
    filterFor f = fromMaybe f (suggestFilter f)

-- | The real filter a made-up name probably means: a known alias
-- (`capitalize` → `pascal`) or the nearest filter by spelling.
suggestFilter :: Text -> Maybe Text
suggestFilter f = case lookup (T.toLower (T.filter (/= '_') f)) aliases of
  Just real -> Just real
  Nothing -> closest f knownFilters
  where
    aliases =
      [ ("capitalize", "pascal"), ("capitalized", "pascal"), ("ucfirst", "pascal"), ("pascalcase", "pascal"), ("classify", "pascal")
      , ("camelcase", "camel"), ("camelize", "camel")
      , ("kebabcase", "kebab"), ("dasherize", "kebab"), ("slug", "kebab"), ("slugify", "kebab")
      , ("snakecase", "snake"), ("underscore", "snake"), ("lower", "snake"), ("lowercase", "snake")
      , ("upper", "constant"), ("uppercase", "constant"), ("screamingsnake", "constant")
      , ("titleize", "title"), ("titlecase", "title")
      , ("pluralize", "plural"), ("singularize", "singular")
      ]

-- Lexing ------------------------------------------------------------------

-- | Everything between @{=@ and @=}@ is one piece. A leading @-@ / trailing
-- @-@ trims whitespace outside it; a first word of if/elif/else/for/end
-- makes it a tag; a leading @?@ makes it a hole; anything else is output.
lexTemplate :: Text -> Either TemplateError [Piece]
lexTemplate = go 1 False []
  where
    go :: Int -> Bool -> [Piece] -> Text -> Either TemplateError [Piece]
    go line trimNext acc src = case T.breakOn "{=" src of
      (rest, "") -> Right (reverse (addLit trimNext rest acc))
      (before, opened) -> do
        let acc' = addLit trimNext before acc
            line' = line + T.count "\n" before
        (inner, rest) <- closeWith line' (T.drop 2 opened)
        let trimBefore = "-" `T.isPrefixOf` inner
            trimAfter = "-" `T.isSuffixOf` inner && T.length inner > 1
            body = T.strip (dropTrims inner)
            acc'' = if trimBefore then trimLastLit acc' else acc'
            next = go (line' + T.count "\n" inner) trimAfter
        if
          | "?" `T.isPrefixOf` body -> Left (HolesNotYetSupported line')
          | isTagKeyword (T.takeWhile isIdentChar body) -> next (TTag line' body : acc'') rest
          | otherwise -> next (TOut line' body : acc'') rest

    dropTrims t =
      let t' = if "-" `T.isPrefixOf` t then T.drop 1 t else t
       in if "-" `T.isSuffixOf` t' then T.dropEnd 1 t' else t'

    isTagKeyword w = w `elem` ["if", "elif", "else", "for", "end"]

    addLit trim t acc =
      let t' = if trim then T.stripStart t else t
       in if T.null t' then acc else TLit t' : acc

    trimLastLit (TLit t : rest) =
      let t' = T.stripEnd t in if T.null t' then rest else TLit t' : rest
    trimLastLit acc = acc

    closeWith line t = case T.breakOn "=}" t of
      (inner, rest)
        | T.null rest -> Left (SyntaxError line "unclosed `{=`: expected `=}`")
        | otherwise -> Right (inner, T.drop 2 rest)

-- Structure ---------------------------------------------------------------

-- | Parse segments until the end of input or a closing tag, which is returned.
block :: [Piece] -> Either TemplateError ([Segment], Maybe (Int, Tag), [Piece])
block [] = Right ([], Nothing, [])
block (tok : rest) = case tok of
  TLit t -> prepend (Lit t) (block rest)
  TOut line src -> do
    e <- runAt line (spaceP *> expr <* eof) src
    prepend (Out e) (block rest)
  TTag line src -> do
    tag <- runAt line (spaceP *> tagP <* eof) src
    case tag of
      TIf cond -> do
        (branches, els, rest') <- ifChain line cond rest
        prepend (If branches els) (block rest')
      TFor v e -> do
        (body, stop, rest') <- block rest
        case stop of
          Just (_, TEnd) -> prepend (For v e body) (block rest')
          _ -> Left (SyntaxError line "`for` without a matching `end`")
      closing -> Right ([], Just (line, closing), rest)
  where
    prepend s r = do
      (segs, stop, rest') <- r
      Right (s : segs, stop, rest')

ifChain :: Int -> Expr -> [Piece] -> Either TemplateError ([(Expr, [Segment])], [Segment], [Piece])
ifChain line cond toks = do
  (body, stop, rest) <- block toks
  case stop of
    Just (_, TElif c) -> do
      (branches, els, rest') <- ifChain line c rest
      Right ((cond, body) : branches, els, rest')
    Just (_, TElse) -> do
      (els, stop', rest') <- block rest
      case stop' of
        Just (_, TEnd) -> Right ([(cond, body)], els, rest')
        _ -> Left (SyntaxError line "`else` without a matching `end`")
    Just (_, TEnd) -> Right ([(cond, body)], [], rest)
    _ -> Left (SyntaxError line "`if` without a matching `end`")

tagKeyword :: Tag -> Text
tagKeyword = \case
  TIf _ -> "if"
  TElif _ -> "elif"
  TElse -> "else"
  TFor _ _ -> "for"
  TEnd -> "end"

-- Expressions -------------------------------------------------------------

type Parser = Parsec Void Text

runAt :: Int -> Parser a -> Text -> Either TemplateError a
runAt line p src = first (SyntaxError line . T.strip . T.pack . errorBundlePretty) (parse p "" src)

spaceP :: Parser ()
spaceP = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceP

symbol :: Text -> Parser Text
symbol = L.symbol spaceP

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

keyword :: Text -> Parser ()
keyword w = lexeme (try (string w *> notFollowedBy (satisfy isIdentChar)))

reserved :: [Text]
reserved = ["and", "or", "not", "true", "false", "in", "if", "elif", "else", "for", "end"]

identifier :: Parser Text
identifier = lexeme . try $ do
  c <- satisfy (\x -> isAsciiLower x || isAsciiUpper x || x == '_')
  cs <- takeWhileP Nothing isIdentChar
  let w = T.cons c cs
  if w `elem` reserved then fail ("`" <> T.unpack w <> "` is a keyword") else pure w

knownFilters :: [Text]
knownFilters = map caseName allCases ++ ["plural", "singular", "timestamp", "parens"]

tagP :: Parser Tag
tagP =
  choice
    [ TIf <$> (keyword "if" *> expr)
    , TElif <$> (keyword "elif" *> expr)
    , TElse <$ keyword "else"
    , TFor <$> (keyword "for" *> identifier) <*> (keyword "in" *> expr)
    , TEnd <$ keyword "end"
    ]

expr :: Parser Expr
expr = chainLeft andExpr (BinOp Or <$ keyword "or")
  where
    andExpr = chainLeft notExpr (BinOp And <$ keyword "and")
    notExpr = (Not <$> (keyword "not" *> notExpr)) <|> comparison
    comparison = do
      l <- filtered
      option l $ do
        op <- (Equal <$ symbol "==") <|> (NotEqual <$ symbol "!=")
        BinOp op l <$> filtered
    filtered = do
      base <- atom
      fs <- many (symbol "|" *> filterName)
      pure (foldl Filter base fs)
    filterName = do
      n <- identifier
      if n `elem` knownFilters then pure n else fail ("unknown filter `" <> T.unpack n <> "`" <> maybe "" (\c -> "; did you mean `" <> T.unpack c <> "`?") (suggestFilter n))
    atom =
      choice
        [ StrLit <$> lexeme (quoted '"' <|> quoted '\'')
        , IntLit <$> lexeme L.decimal
        , BoolLit True <$ keyword "true"
        , BoolLit False <$ keyword "false"
        , between (symbol "(") (symbol ")") expr
        , do
            v <- identifier
            fs <- many (char '.' *> identifier)
            pure (foldl Field (Var v) fs)
        ]
    quoted q = char q *> takeWhileP Nothing (/= q) <* char q
    chainLeft p op = p >>= rest
      where
        rest l = (do f <- op; r <- p; rest (f l r)) <|> pure l
