module Api.Schema where

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Either (Either(Right, Left))
import Data.Foreign (ForeignError(JSONError), fail)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Prelude
import Types (ErrorDetail)

data ServerResponse a
  = ServerSuccess a
  | ServerError ErrorDetail

instance isForeignServerResponse :: (IsForeign a) => IsForeign (ServerResponse a) where
  read json = do
    success <- readProp "success" json
    if success
      then ServerSuccess <$> readProp "object" json
      else do title <- readProp "title" json
              body  <- readProp "message" json
              pure $ ServerError { title: title, body: body }

newtype JsonEither a b = JsonEither (Either a b)

runJsonEither :: forall a b. JsonEither a b -> Either a b
runJsonEither (JsonEither x) = x

instance isForeignJsonEither :: (IsForeign a, IsForeign b) => IsForeign (JsonEither a b) where
  read json = do
    l <- readProp "Left" json
    r <- readProp "Right" json
    case Tuple (unNullOrUndefined l) (unNullOrUndefined r) of
      Tuple (Just l') Nothing -> pure $ JsonEither $ Left l'
      Tuple Nothing (Just r') -> pure $ JsonEither $ Right r'
      _ -> fail $ JSONError "expected `Left` or `Right` property"

newtype Name = Name String

instance encodeJsonName :: EncodeJson Name where
  encodeJson (Name name) = "name" := name
                        ~> jsonEmptyObject
