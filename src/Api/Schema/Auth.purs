module Api.Schema.Auth where

import Prelude

import           Data.Maybe
import           Data.Tuple
import           Data.Foreign
import           Data.Foreign.Class
import           Data.Foreign.NullOrUndefined

import Optic.Core

import Types


newtype LoginResponse = LoginResponse
  { lrSuccess :: Boolean
  , lrMessage :: String
  }

instance isForeignLoginResponse :: IsForeign LoginResponse where
  read json = do
    suc <- readProp "success" json
    msg <- readProp "msg" json
    pure $ LoginResponse { lrSuccess: suc, lrMessage: msg }

newtype LoginStatus = LoginStatus (Maybe String)

instance isForeignLoginStatus :: IsForeign LoginStatus where
  read json = LoginStatus <<< runNullOrUndefined <$> readProp "customerId" json
