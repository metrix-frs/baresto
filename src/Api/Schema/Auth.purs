module Api.Schema.Auth where

import Prelude (pure, ($), (<$>), (<*>), bind)

import Data.Maybe (Maybe)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign.NullOrUndefined (runNullOrUndefined)

import Types (UTCTime)

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
