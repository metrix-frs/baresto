module Api.Schema.File where

import Data.Foreign.Class (class IsForeign, readProp)
import Data.Lens (Lens', lens)
import Prelude
import Types (UpdateId, UTCTime, ModuleId, FileId, Label)

newtype File = File
  { fileId           :: FileId
  , fileModuleId     :: ModuleId
  , fileLabel        :: String
  , fileCreated      :: UTCTime
  , fileChanged      :: UTCTime
  , fileLastUpdateId :: UpdateId
  }

_File :: Lens' File _
_File = lens (\(File r) -> r) (\_ r -> File r)

_fileId :: Lens' File FileId
_fileId = _File <<< lens _.fileId _{ fileId = _ }

_fileModuleId :: Lens' File ModuleId
_fileModuleId = _File <<< lens _.fileModuleId _{ fileModuleId = _ }

_fileLabel :: Lens' File Label
_fileLabel = _File <<< lens _.fileLabel _{ fileLabel = _ }

_fileCreated :: Lens' File UTCTime
_fileCreated = _File <<< lens _.fileCreated _{ fileCreated = _ }

_fileChanged :: Lens' File UTCTime
_fileChanged = _File <<< lens _.fileChanged _{ fileChanged = _ }

_fileLastUpdateId :: Lens' File UpdateId
_fileLastUpdateId = _File <<< lens _.fileLastUpdateId _{ fileLastUpdateId = _ }

instance isForeignFile :: IsForeign File where
  read json = do
    file <- { fileId: _
            , fileModuleId: _
            , fileLabel: _
            , fileCreated: _
            , fileChanged: _
            , fileLastUpdateId: _
            }
      <$> readProp "id" json
      <*> readProp "moduleId" json
      <*> readProp "label" json
      <*> readProp "created" json
      <*> readProp "changed" json
      <*> readProp "lastUpdateId" json
    pure $ File file

newtype FileDesc = FileDesc
  { fileDescId           :: FileId
  , fileDescLabel        :: String
  , fileDescModId        :: ModuleId
  , fileDescModLabel     :: String
  , fileDescTaxLabel     :: String
  , fileDescCreated      :: UTCTime
  , fileDescLastUpdateId :: UpdateId
  }

instance isForeignFileDesc :: IsForeign FileDesc where
  read json = do
    file <- { fileDescId: _
            , fileDescLabel: _
            , fileDescModId: _
            , fileDescModLabel: _
            , fileDescTaxLabel: _
            , fileDescCreated: _
            , fileDescLastUpdateId: _
            }
      <$> readProp "id" json
      <*> readProp "label" json
      <*> readProp "moduleId" json
      <*> readProp "moduleLabel" json
      <*> readProp "taxonomyLabel" json
      <*> readProp "created" json
      <*> readProp "lastUpdateId" json
    pure $ FileDesc file
