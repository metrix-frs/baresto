module Api.Schema.Module where

import Data.Foreign.Class (class IsForeign, readProp)
import Data.Lens (Lens', lens)
import Prelude
import Types (TableId, TemplateId, TemplateGroupId, ModuleId)

newtype Module = Module
  { moduleId :: ModuleId
  , moduleLabel     :: String
  , templateGroups :: Array TemplateGroup
  }

_Module :: Lens' Module _
_Module = lens (\(Module r) -> r) (\_ r -> Module r)

_moduleId :: Lens' Module ModuleId
_moduleId = _Module <<< lens _.moduleId _{moduleId = _ }

_moduleLabel :: Lens' Module String
_moduleLabel = _Module <<< lens _.moduleLabel _{ moduleLabel = _ }

_templateGroups :: Lens' Module (Array TemplateGroup)
_templateGroups = _Module <<< lens _.templateGroups _{ templateGroups = _ }

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
  , templates :: Array Template
  }

_TemplateGroup :: Lens' TemplateGroup _
_TemplateGroup = lens (\(TemplateGroup r) -> r) (\_ r -> TemplateGroup r)

_templateGroupId :: Lens' TemplateGroup TemplateGroupId
_templateGroupId = _TemplateGroup <<< lens _.templateGroupId _{templateGroupId = _ }

_templateGroupLabel :: Lens' TemplateGroup String
_templateGroupLabel = _TemplateGroup <<< lens _.templateGroupLabel _{ templateGroupLabel = _ }

_templates :: Lens' TemplateGroup (Array Template)
_templates = _TemplateGroup <<< lens _.templates _{ templates = _ }

instance isForeignTemplateGroup :: IsForeign TemplateGroup where
  read json = do
    grp <- { templateGroupId: _, templateGroupLabel: _, templates: _ }
      <$> readProp "id"        json
      <*> readProp "label"     json
      <*> readProp "templates" json
    pure $ TemplateGroup grp

newtype Template = Template
  { templateId     :: TemplateId
  , templateCode   :: String
  , templateLabel  :: String
  , templateTables :: Array TableEntry
  }

_Template :: Lens' Template _
_Template = lens (\(Template r) -> r) (\_ r -> Template r)

_templateId :: Lens' Template TemplateId
_templateId = _Template <<< lens _.templateId _{ templateId = _ }

_templateCode :: Lens' Template String
_templateCode = _Template <<< lens _.templateCode _{ templateCode = _ }

_templateLabel :: Lens' Template String
_templateLabel = _Template <<< lens _.templateLabel _{ templateLabel = _ }

_templateTables :: Lens' Template (Array TableEntry)
_templateTables = _Template <<< lens _.templateTables _{ templateTables = _ }

instance isForeignTemplate :: IsForeign Template where
  read json = do
    tpl <- { templateId: _
           , templateCode: _
           , templateLabel: _
           , templateTables: _
           }
      <$> readProp "id"     json
      <*> readProp "code"   json
      <*> readProp "label"  json
      <*> readProp "tables" json
    pure $ Template tpl

newtype TableEntry = TableEntry
  { tableEntryId :: TableId
  , tableEntryCode :: String
  }

_TableEntry :: Lens' TableEntry _
_TableEntry = lens (\(TableEntry r) -> r) (\_ r -> TableEntry r)

_tableEntryId :: Lens' TableEntry TableId
_tableEntryId = _TableEntry <<< lens _.tableEntryId _{ tableEntryId = _ }

_tableEntryCode :: Lens' TableEntry String
_tableEntryCode = _TableEntry <<< lens _.tableEntryCode _{ tableEntryCode = _ }

instance isForeignTableEntry :: IsForeign TableEntry where
  read json = do
    tbl <- { tableEntryId: _
           , tableEntryCode: _
           }
        <$> readProp "id" json
        <*> readProp "code" json
    pure $ TableEntry tbl
