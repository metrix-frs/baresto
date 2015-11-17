module Api.Schema.Import where

import Prelude

import Data.Array
import Data.Foreign
import Data.Foreign.Class

import Api.Schema.BusinessData

import Types

newtype Warning = Warning
  { message :: String
  , context :: String
  }

instance isForeignWarning :: IsForeign Warning where
  read json = do
    msg <- readProp "message" json
    ctx <- readProp "context" json
    pure $ Warning { message: msg, context: ctx }

newtype XbrlImportConf = XbrlImportConf
  { warnings :: Array Warning
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

newtype CsvImportConf = CsvImportConf
  { warnings :: Array Warning
  , changes :: UpdateGet
  }

instance isForeignCsvImportConf :: IsForeign CsvImportConf where
  read json = do
    conf <- { warnings: _
            , changes: _
            }
      <$> readProp "warnings" json
      <*> readProp "changes" json
    pure $ CsvImportConf conf
