module Olifant.Template.Render (render, evaluate) where

import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Olifant.Naming (Case (Snake), Name, caseFromText, parseName, pluralize, singularize)
import Olifant.Naming qualified as Naming
import Olifant.Template.Error
import Olifant.Template.Syntax
import Olifant.Template.Value

render :: Env -> Template -> Either TemplateError Text
render env = renderSegments env . templateSegments

-- | Like 'render', but a template that is exactly one @{{ expr }}@ keeps the
-- expression's value (so derived values can be bools, names or lists).
evaluate :: Env -> Template -> Either TemplateError Value
evaluate env t = case templateSegments t of
  [Out e] -> eval env e
  _ -> VText <$> render env t

renderSegments :: Env -> [Segment] -> Either TemplateError Text
renderSegments env segs = T.concat <$> traverse (renderSegment env) segs

renderSegment :: Env -> Segment -> Either TemplateError Text
renderSegment env = \case
  Lit t -> Right t
  Out e -> eval env e >>= display
  If branches els -> pick branches
    where
      pick [] = renderSegments env els
      pick ((cond, body) : more) = do
        v <- eval env cond
        if truthy v then renderSegments env body else pick more
  For var e body -> do
    items <-
      eval env e >>= \case
        VList xs -> Right xs
        VNull -> Right []
        other -> Left (TypeMismatch ("`for` needs a list, got " <> describeValue other))
    let n = length items
        loopVars i =
          VRecord (M.fromList [("first", VBool (i == 0)), ("last", VBool (i == n - 1)), ("index", VInt (fromIntegral i + 1))])
    T.concat
      <$> sequence
        [ renderSegments (M.insert var item (M.insert "loop" (loopVars i) env)) body
        | (i, item) <- zip [0 :: Int ..] items
        ]

display :: Value -> Either TemplateError Text
display = \case
  VText t -> Right t
  VName n -> Right (Naming.render Snake n)
  VBool b -> Right (if b then "true" else "false")
  VInt i -> Right (T.pack (show i))
  VTime t -> Right (T.pack (iso8601Show t))
  VNull -> Right ""
  other -> Left (TypeMismatch ("cannot print " <> describeValue other))

eval :: Env -> Expr -> Either TemplateError Value
eval env = \case
  Var x -> maybe (Left (UndefinedVariable x)) Right (M.lookup x env)
  Field e f ->
    eval env e >>= \case
      VRecord m -> maybe (Left (UndefinedVariable f)) Right (M.lookup f m)
      other -> Left (TypeMismatch ("cannot read `." <> f <> "` from " <> describeValue other))
  StrLit t -> Right (VText t)
  IntLit i -> Right (VInt i)
  BoolLit b -> Right (VBool b)
  Not e -> VBool . not . truthy <$> eval env e
  BinOp op a b -> do
    va <- eval env a
    vb <- eval env b
    Right . VBool $ case op of
      Equal -> valuesEqual va vb
      NotEqual -> not (valuesEqual va vb)
      And -> truthy va && truthy vb
      Or -> truthy va || truthy vb
  Filter e f -> eval env e >>= applyFilter f

applyFilter :: Text -> Value -> Either TemplateError Value
applyFilter f v = case caseFromText f of
  Just c -> VText . Naming.render c <$> asName
  Nothing -> case f of
    "plural" -> VName . pluralize <$> asName
    "singular" -> VName . singularize <$> asName
    "timestamp" -> case v of
      VTime t -> Right (VText (T.pack (formatTime defaultTimeLocale "%Y%m%d%H%M%S" t)))
      other -> Left (TypeMismatch ("`timestamp` needs a time, got " <> describeValue other))
    _ -> Left (TypeMismatch ("unknown filter `" <> f <> "`"))
  where
    asName :: Either TemplateError Name
    asName = case v of
      VName n -> Right n
      VText t -> either (Left . TypeMismatch . (("`" <> f <> "`: ") <>)) Right (parseName t)
      other -> Left (TypeMismatch ("`" <> f <> "` needs a name, got " <> describeValue other))
