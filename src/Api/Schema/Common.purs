module Api.Schema.Common where

import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Foreign (ForeignError(JSONError), fail)
import Data.Foreign.Class (class IsForeign, read)
import Data.Tuple (Tuple(Tuple))
import Prelude

newtype Pair a b = Pair (Tuple a b)

getPair :: forall a b. Pair a b -> Tuple a b
getPair (Pair t) = t

instance isForeignPair :: (IsForeign a, IsForeign b) => IsForeign (Pair a b) where
  read json = do
    list <- read json
    case list of
      [a, b] -> Pair <$> (Tuple <$> read a <*> read b)
      _ -> fail $ JSONError "expected list of two elements"

instance encodeJsonPair :: (EncodeJson a, EncodeJson b) => EncodeJson (Pair a b) where
  encodeJson (Pair (Tuple a b)) = encodeJson
    [ encodeJson a
    , encodeJson b
    ]
