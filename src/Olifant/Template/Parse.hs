module Olifant.Template.Parse (parseTemplate) where

import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Olifant.Naming (allCases, caseName)
import Olifant.Template.Error
import Olifant.Template.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Piece = TLit Text | TOut Int Text | TTag Int Text

data Tag = TIf Expr | TElif Expr | TElse | TEndif | TFor Text Expr | TEndfor

parseTemplate :: Text -> Either TemplateError Template
parseTemplate src = do
  tokens <- lexTemplate src
  (segs, stop, _) <- block tokens
  case stop of
    Nothing -> Right (Template src segs)
    Just (line, tag) -> Left (SyntaxError line ("unexpected `" <> tagKeyword tag <> "`"))

-- Lexing ------------------------------------------------------------------

lexTemplate :: Text -> Either TemplateError [Piece]
lexTemplate = go 1 False []
  where
    go :: Int -> Bool -> [Piece] -> Text -> Either TemplateError [Piece]
    go line trimNext acc src = case nextOpener src of
      Nothing -> Right (reverse (addLit trimNext src acc))
      Just (before, opener, afterOpener) -> do
        let acc' = addLit trimNext before acc
            line' = line + T.count "\n" before
        case opener of
          "<$" -> Left (HolesNotYetSupported line')
          "{{" -> do
            (inner, rest) <- closeWith line' "}}" afterOpener
            go (line' + T.count "\n" inner) False (TOut line' (T.strip inner) : acc') rest
          _ -> do
            (inner, rest) <- closeWith line' "%}" afterOpener
            let trimBefore = "-" `T.isPrefixOf` inner
                trimAfter = "-" `T.isSuffixOf` inner
                body = T.strip (T.dropWhileEnd (== '-') (T.dropWhile (== '-') inner))
                acc'' = if trimBefore then trimLastLit acc' else acc'
            go (line' + T.count "\n" inner) trimAfter (TTag line' body : acc'') rest

    addLit trim t acc =
      let t' = if trim then T.stripStart t else t
       in if T.null t' then acc else TLit t' : acc

    trimLastLit (TLit t : rest) =
      let t' = T.stripEnd t in if T.null t' then rest else TLit t' : rest
    trimLastLit acc = acc

    closeWith line closer t = case T.breakOn closer t of
      (inner, rest)
        | T.null rest -> Left (SyntaxError line ("unclosed tag: expected `" <> closer <> "`"))
        | otherwise -> Right (inner, T.drop (T.length closer) rest)

    nextOpener t =
      case sortOn
        (\(b, _, _) -> T.length b)
        [(b, o, T.drop 2 a) | o <- ["{{", "{%", "<$"], let (b, a) = T.breakOn o t, not (T.null a)] of
        [] -> Nothing
        (x : _) -> Just x

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
          Just (_, TEndfor) -> prepend (For v e body) (block rest')
          _ -> Left (SyntaxError line "`for` without a matching `endfor`")
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
        Just (_, TEndif) -> Right ([(cond, body)], els, rest')
        _ -> Left (SyntaxError line "`else` without a matching `endif`")
    Just (_, TEndif) -> Right ([(cond, body)], [], rest)
    _ -> Left (SyntaxError line "`if` without a matching `endif`")

tagKeyword :: Tag -> Text
tagKeyword = \case
  TIf _ -> "if"
  TElif _ -> "elif"
  TElse -> "else"
  TEndif -> "endif"
  TFor _ _ -> "for"
  TEndfor -> "endfor"

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
reserved = ["and", "or", "not", "true", "false", "in", "if", "elif", "else", "endif", "for", "endfor"]

identifier :: Parser Text
identifier = lexeme . try $ do
  c <- satisfy (\x -> isAsciiLower x || isAsciiUpper x || x == '_')
  cs <- takeWhileP Nothing isIdentChar
  let w = T.cons c cs
  if w `elem` reserved then fail ("`" <> T.unpack w <> "` is a keyword") else pure w

knownFilters :: [Text]
knownFilters = map caseName allCases ++ ["plural", "singular", "timestamp"]

tagP :: Parser Tag
tagP =
  choice
    [ TIf <$> (keyword "if" *> expr)
    , TElif <$> (keyword "elif" *> expr)
    , TEndif <$ keyword "endif"
    , TElse <$ keyword "else"
    , TFor <$> (keyword "for" *> identifier) <*> (keyword "in" *> expr)
    , TEndfor <$ keyword "endfor"
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
      if n `elem` knownFilters then pure n else fail ("unknown filter `" <> T.unpack n <> "`")
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
