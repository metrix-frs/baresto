module Api.Schema.Import where

import Prelude
import Data.Foreign.Class (class IsForeign, readProp)
import Api.Schema.BusinessData (UpdateGet)
import Types (ModuleId, UpdateId)

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
  , update   :: UpdateGet
  }

instance isForeignCsvImportConf :: IsForeign CsvImportConf where
  read json = do
    conf <- { warnings: _
            , update: _
            }
      <$> readProp "warnings" json
      <*> readProp "update" json
    pure $ CsvImportConf conf
