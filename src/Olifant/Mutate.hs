-- | Mutation testing's makings: operators (an ast-grep pattern and its fix,
-- or a Haskell transform of the matched text), the sites they fire on within
-- the lines in scope, and the waivers a line can carry. Running the mutants
-- is `Olifant.Block`'s judge, not this module's.
module Olifant.Mutate (Mutant (..), Operator (..), mutantsOf, waiverOn, waiverLine, capMutants) where

import Data.Aeson (Value)
import Data.ByteString qualified as BS
import Data.List (sortOn)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Olifant.Language (Language (..), Match (..), Rewrite (..), Syntax (..), SyntaxFailure)

-- | One mutation of a file: the line, the operator that made it, what it
-- changed from and to, and the whole mutated file. `muWaived` holds the
-- reason a surviving mutant was accepted; `muDim` marks one Jev thinks
-- equivalent to the original.
data Mutant = Mutant
  { muLine :: Int,
    muOperator :: Text,
    muFrom :: Text,
    muTo :: Text,
    muText :: Text,
    muWaived :: Maybe Text,
    muDim :: Bool
  }
  deriving stock (Eq, Show)

-- | A way to break code: an ast-grep rule with a fix that rewrites each match,
-- or one whose matched text a Haskell function transforms (`Nothing`: no mutant).
data Operator
  = Rewriting {opName :: Text, opRule :: Value, opFix :: Text}
  | Transforming {opName :: Text, opRule :: Value, opTransform :: Text -> Maybe Text}

-- | The file's text and the lines in scope (1-based) → one mutant per (site,
-- operator), in file order. A rewrite that changes nothing is no mutant, and
-- a line's waiver for an operator rides on that operator's mutant there.
mutantsOf :: Monad m => Syntax m -> Language -> [Operator] -> Text -> Set Int -> m (Either SyntaxFailure [Mutant])
mutantsOf syntax lang operators file scope = go operators []
  where
    go [] acc = pure (Right (sortOn (\m -> (muLine m, muOperator m)) (concat (reverse acc))))
    go (op : rest) acc =
      sitesOf op >>= \case
        Left failure -> pure (Left failure)
        Right sites -> go rest ([mutant op s | s@(Site (line, _) _ _ _ _) <- sites, S.member (line + 1) scope, rw s /= txt s] : acc)
    sitesOf op = case op of
      Rewriting {} -> fmap (map (\r -> Site (rwStart r) (rwEnd r) (rwText r) (rwReplacement r) (rwBytes r))) <$> rewriteSyntax syntax lang (opRule op) (opFix op) file
      Transforming {} ->
        fmap (concatMap (\m -> [Site (matchStart m) (matchEnd m) (matchText m) to (matchBytes m) | Just to <- [opTransform op (matchText m)]]))
          <$> querySyntax syntax lang (opRule op) file
    mutant op (Site (line, _) _ from to bytes) =
      Mutant
        { muLine = line + 1,
          muOperator = opName op,
          muFrom = fst (difference from to),
          muTo = snd (difference from to),
          muText = spliceBytes bytes to file,
          muWaived = waiverOn lang file (line + 1) (opName op),
          muDim = False
        }
    rw (Site _ _ _ to _) = to
    txt (Site _ _ from _ _) = from

-- | What changed between a site's text and its replacement: the words left
-- once those they share at both ends are dropped (`>` and `>=` for
-- `a > b` and `a >= b`); the whole of both when one side would be left empty.
difference :: Text -> Text -> (Text, Text)
difference from to
  | null a || null b = (from, to)
  | otherwise = (T.unwords a, T.unwords b)
  where
    (ws, vs) = (T.words from, T.words to)
    lead = length (takeWhile id (zipWith (==) ws vs))
    (ws', vs') = (drop lead ws, drop lead vs)
    trail = length (takeWhile id (zipWith (==) (reverse ws') (reverse vs')))
    (a, b) = (take (length ws' - trail) ws', take (length vs' - trail) vs')

-- | A matched site: start, end, matched text, its replacement, and byte range.
data Site = Site (Int, Int) (Int, Int) Text Text (Int, Int)

spliceBytes :: (Int, Int) -> Text -> Text -> Text
spliceBytes (from, to) replacement file =
  let bytes = TE.encodeUtf8 file
   in TE.decodeUtf8With lenientDecode (BS.take from bytes <> TE.encodeUtf8 replacement <> BS.drop to bytes)

-- | The reason on a line's waiver for an operator: the file, a 1-based line
-- and an operator → `COMMENT olifant: OP ok: REASON` at the line's end.
waiverOn :: Language -> Text -> Int -> Text -> Maybe Text
waiverOn lang file line op = case drop (line - 1) (T.lines file) of
  (text : _) | (_ : _ : _) <- parts text -> do
    rest <- T.stripPrefix "olifant:" (T.stripStart (last (parts text)))
    let (name, afterName) = T.break (== ' ') (T.stripStart rest)
    reason <- T.strip <$> T.stripPrefix "ok:" (T.stripStart afterName)
    if name == op && T.stripStart afterName /= afterName && not (T.null reason) then Just reason else Nothing
  _ -> Nothing
  where
    parts = T.splitOn (langComment lang)

-- | The file with `  # olifant: OP ok: REASON` appended to that (1-based) line.
waiverLine :: Language -> Text -> Int -> Text -> Text -> Text
waiverLine lang file line op reason =
  T.unlines [if i == line then l <> "  " <> langComment lang <> " olifant: " <> op <> " ok: " <> reason else l | (i, l) <- zip [1 ..] (T.lines file)]

-- | The first `n` mutants (they are already in file order).
capMutants :: Int -> [Mutant] -> [Mutant]
capMutants = take
