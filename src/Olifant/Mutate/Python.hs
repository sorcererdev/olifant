-- | The Python mutation operators: what a test of Python code must notice.
module Olifant.Mutate.Python (pythonOperators) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Mutate (Operator (..))
import Text.Read (readMaybe)

-- | Boundaries, equality, negation, and/or, arithmetic, a return's value, a
-- call statement, strings and integers. A string in a statement of its own
-- (a docstring) is not a site.
pythonOperators :: [Operator]
pythonOperators =
  [ rw "boundary" "$A < $B" "$A <= $B",
    rw "boundary" "$A <= $B" "$A < $B",
    rw "boundary" "$A > $B" "$A >= $B",
    rw "boundary" "$A >= $B" "$A > $B",
    rw "equality" "$A == $B" "$A != $B",
    rw "equality" "$A != $B" "$A == $B",
    rw "negate" "$A is None" "$A is not None",
    rw "negate" "$A is not None" "$A is None",
    rw "negate" "not $A" "$A",
    rw "and-or" "$A and $B" "$A or $B",
    rw "and-or" "$A or $B" "$A and $B",
    rw "arith" "$A + $B" "$A - $B",
    rw "arith" "$A - $B" "$A + $B",
    rw "return-none" "return $X" "return None",
    Rewriting "delete-call" (object ["kind" .= ("expression_statement" :: Text), "has" .= object ["kind" .= ("call" :: Text)]]) "pass",
    Transforming "string" (object ["kind" .= ("string" :: Text), "not" .= object ["inside" .= object ["kind" .= ("expression_statement" :: Text)]]]) (const (Just "\"\"")),
    Transforming "number" (object ["kind" .= ("integer" :: Text)]) (\t -> T.pack . show . (+ (1 :: Integer)) <$> readMaybe (T.unpack t))
  ]
  where
    rw :: Text -> Text -> Text -> Operator
    rw n p = Rewriting n (object ["pattern" .= p] :: Value)
