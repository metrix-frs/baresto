module Api.Schema where

import Prelude

import Data.Foreign
import Data.Foreign.Class

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
              body  <- readProp "body"  json
              pure $ ServerError { title: title, body: body }
