module Api.Schema.Auth where

import Prelude

import Data.Maybe
import Data.Tuple
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.NullOrUndefined

import Types

newtype AuthInfo = AuthInfo
  { authUserName           :: String
  , authContractBegin      :: UTCTime
  , authContractEnd        :: UTCTime
  , authContractIsTrial    :: Boolean
  , authContractInvalidMsg :: Maybe String
  }

instance isForeignAuthInfo :: IsForeign AuthInfo where
  read json = do
    status <- { authUserName: _
              , authContractBegin: _
              , authContractEnd: _
              , authContractIsTrial: _
              , authContractInvalidMsg: _
              }
      <$> readProp "userName"      json
      <*> readProp "contractBegin" json
      <*> readProp "contractEnd"   json
      <*> readProp "isTrial"       json
      <*> (runNullOrUndefined <$> readProp "invalidMsg" json)
    pure $ AuthInfo status
