module Api.Schema.Module where

import Prelude

import Data.Foreign
import Data.Foreign.Class

import Optic.Core

import Types

newtype Module = Module
  { moduleId :: ModuleId
  , moduleLabel     :: String
  , templateGroups :: Array TemplateGroup
  }

_Module :: LensP Module _
_Module = lens (\(Module r) -> r) (\_ r -> Module r)

_moduleId :: LensP Module ModuleId
_moduleId = _Module .. lens _.moduleId _{moduleId = _ }

_moduleLabel :: LensP Module String
_moduleLabel = _Module .. lens _.moduleLabel _{ moduleLabel = _ }

_templateGroups :: LensP Module (Array TemplateGroup)
_templateGroups = _Module .. lens _.templateGroups _{ templateGroups = _ }

instance isForeignModule :: IsForeign Module where
  read json = do
    mod <- { moduleId: _, moduleLabel: _, templateGroups: _ }
      <$> readProp "id"             json
      <*> readProp "label"          json
      <*> readProp "templateGroups" json
    pure $ Module mod

newtype TemplateGroup = TemplateGroup
  { templateGroupId :: TemplateGroupId
  , templateGroupLabel :: String
  , templates :: Array TemplateEntry
  }

_TemplateGroup :: LensP TemplateGroup _
_TemplateGroup = lens (\(TemplateGroup r) -> r) (\_ r -> TemplateGroup r)

_templateGroupId :: LensP TemplateGroup TemplateGroupId
_templateGroupId = _TemplateGroup .. lens _.templateGroupId _{templateGroupId = _ }

_templateGroupLabel :: LensP TemplateGroup String
_templateGroupLabel = _TemplateGroup .. lens _.templateGroupLabel _{ templateGroupLabel = _ }

_templates :: LensP TemplateGroup (Array TemplateEntry)
_templates = _TemplateGroup .. lens _.templates _{ templates = _ }

instance isForeignTemplateGroup :: IsForeign TemplateGroup where
  read json = do
    grp <- { templateGroupId: _, templateGroupLabel: _, templates: _ }
      <$> readProp "id"        json
      <*> readProp "label"     json
      <*> readProp "templates" json
    pure $ TemplateGroup grp

newtype TemplateEntry = TemplateEntry
  { templateEntryId    :: TemplateId
  , templateEntryCode  :: String
  , templateEntryLabel :: String
  }

_templateEntry :: LensP TemplateEntry _
_templateEntry = lens (\(TemplateEntry r) -> r) (\_ r -> TemplateEntry r)

_templateEntryId :: LensP TemplateEntry TemplateId
_templateEntryId = _templateEntry .. lens _.templateEntryId _{templateEntryId = _ }

_templateEntryCode :: LensP TemplateEntry String
_templateEntryCode = _templateEntry .. lens _.templateEntryCode _{ templateEntryCode = _ }

_templateEntryLabel :: LensP TemplateEntry String
_templateEntryLabel = _templateEntry .. lens _.templateEntryLabel _{ templateEntryLabel = _ }

instance isForeignTemplate :: IsForeign TemplateEntry where
  read json = do
    frm <- { templateEntryId: _, templateEntryCode: _, templateEntryLabel: _ }
      <$> readProp "id"    json
      <*> readProp "code"  json
      <*> readProp "label" json
    pure $ TemplateEntry frm
