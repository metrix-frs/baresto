module Api.Schema.Import where

import Prelude

import Data.Array
import Data.Foreign
import Data.Foreign.Class

import Types

newtype XbrlImportWarning = XbrlImportWarning
  { message :: String
  , context :: String
  }

instance isForeignXbrlImportWarning :: IsForeign XbrlImportWarning where
  read json = do
    msg <- readProp "message" json
    ctx <- readProp "context" json
    pure $ XbrlImportWarning { message: msg, context: ctx }

newtype XbrlImportConf = XbrlImportConf
  { warnings :: Array XbrlImportWarning
  , updateId :: UpdateId
  , moduleId :: ModuleId
  }

instance isForeignXbrlImportConf :: IsForeign XbrlImportConf where
  read json = do
    conf <- { warnings: _
            , updateId: _
            , moduleId: _
            }
      <$> readProp "warnings" json
      <*> readProp "updateId" json
      <*> readProp "moduleId" json
    pure $ XbrlImportConf conf
