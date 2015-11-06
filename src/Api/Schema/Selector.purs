module Api.Schema.Selector where

import Prelude

import Data.Foreign
import Data.Foreign.Class

import Optic.Core

import Types

newtype File = File
  { fileId :: FileId
  , fileModuleId :: ModuleId
  }

_File :: LensP File _
_File = lens (\(File r) -> r) (\_ r -> File r)

_fileId :: LensP File FileId
_fileId = _File .. lens _.fileId _{ fileId = _ }

_fileModuleId :: LensP File ModuleId
_fileModuleId = _File .. lens _.fileModuleId _{ fileModuleId = _ }

instance isForeignFile :: IsForeign File where
  read json = do
    file <- { fileId: _
            , fileModuleId: _
            }
      <$> readProp "id" json
      <*> readProp "moduleId" json
    pure $ File file

--

newtype Framework = Framework
  { frameworkId :: FrameworkId
  , frameworkLabel :: String
  , taxonomies :: Array Taxonomy
  }

_Framework :: LensP Framework _
_Framework = lens (\(Framework r) -> r) (\_ r -> Framework r)

_frameworkId :: LensP Framework FrameworkId
_frameworkId = _Framework .. lens _.frameworkId _{ frameworkId = _ }

_frameworkLabel :: LensP Framework String
_frameworkLabel = _Framework .. lens _.frameworkLabel _{ frameworkLabel = _ }

_taxonomies :: LensP Framework (Array Taxonomy)
_taxonomies = _Framework .. lens _.taxonomies _{ taxonomies = _ }

instance isForeignFramework :: IsForeign Framework where
  read json = do
    frm <- { frameworkId: _, frameworkLabel: _, taxonomies: _ }
      <$> readProp "id"         json
      <*> readProp "label"      json
      <*> readProp "taxonomies" json
    pure $ Framework frm

newtype Taxonomy = Taxonomy
  { taxonomyId :: TaxonomyId
  , taxonomyLabel :: String
  , conceptualModules :: Array ConceptualModule
  }

_Taxonomy :: LensP Taxonomy _
_Taxonomy = lens (\(Taxonomy r) -> r) (\_ r -> Taxonomy r)

_taxonomyId :: LensP Taxonomy TaxonomyId
_taxonomyId = _Taxonomy .. lens _.taxonomyId _{taxonomyId = _ }

_taxonomyLabel :: LensP Taxonomy String
_taxonomyLabel = _Taxonomy .. lens _.taxonomyLabel _{ taxonomyLabel = _ }

_conceptualModules :: LensP Taxonomy (Array ConceptualModule)
_conceptualModules = _Taxonomy .. lens _.conceptualModules _{ conceptualModules = _ }

instance isForeignTaxonomy :: IsForeign Taxonomy where
  read json = do
    tax <- { taxonomyId: _, taxonomyLabel: _, conceptualModules: _ }
      <$> readProp "id"       json
      <*> readProp "label"    json
      <*> readProp "concepts" json
    pure $ Taxonomy tax

newtype ConceptualModule = ConceptualModule
  { conceptId :: ConceptualModuleId
  , conceptLabel :: String
  , modules :: Array ModuleId
  }

_ConceptualModule :: LensP ConceptualModule _
_ConceptualModule = lens (\(ConceptualModule r) -> r) (\_ r -> ConceptualModule r)

_conceptId :: LensP ConceptualModule ConceptualModuleId
_conceptId = _ConceptualModule .. lens _.conceptId _{conceptId = _ }

_conceptLabel :: LensP ConceptualModule String
_conceptLabel = _ConceptualModule .. lens _.conceptLabel _{ conceptLabel = _ }

_modules :: LensP ConceptualModule (Array ModuleId)
_modules = _ConceptualModule .. lens _.modules _{ modules = _ }

instance isForeignConceptualModule :: IsForeign ConceptualModule where
  read json = do
    con <- { conceptId: _, conceptLabel: _, modules: _ }
      <$> readProp "id"      json
      <*> readProp "label"   json
      <*> readProp "modules" json
    pure $ ConceptualModule con
