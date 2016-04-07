module Api.Schema where

import Prelude

import Control.Monad.Error.Class (throwError)

import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.NullOrUndefined

import Types

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
    case Tuple (runNullOrUndefined l) (runNullOrUndefined r) of
      Tuple (Just l') Nothing -> pure $ JsonEither $ Left l'
      Tuple Nothing (Just r') -> pure $ JsonEither $ Right r'
      _ -> throwError $ JSONError "expected `Left` or `Right` property"
