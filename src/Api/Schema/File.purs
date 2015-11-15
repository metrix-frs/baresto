module Api.Schema.File where

import Prelude

import Data.Foreign
import Data.Foreign.Class

import Optic.Core

import Types

newtype File = File
  { fileId :: FileId
  , fileModuleId :: ModuleId
  , fileLabel :: String
  , fileCreated :: UTCTime
  , fileLastUpdateId :: UpdateId
  }

_File :: LensP File _
_File = lens (\(File r) -> r) (\_ r -> File r)

_fileId :: LensP File FileId
_fileId = _File .. lens _.fileId _{ fileId = _ }

_fileModuleId :: LensP File ModuleId
_fileModuleId = _File .. lens _.fileModuleId _{ fileModuleId = _ }

_fileLabel :: LensP File Label
_fileLabel = _File .. lens _.fileLabel _{ fileLabel = _ }

instance isForeignFile :: IsForeign File where
  read json = do
    file <- { fileId: _
            , fileModuleId: _
            , fileLabel: _
            , fileCreated: _
            , fileLastUpdateId: _
            }
      <$> readProp "id" json
      <*> readProp "moduleId" json
      <*> readProp "label" json
      <*> readProp "created" json
      <*> readProp "lastUpdateId" json
    pure $ File file

newtype Tag = Tag
  { tagId :: TagId
  }
