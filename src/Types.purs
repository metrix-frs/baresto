module Types where

import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Random (RANDOM())

import Control.Monad.Aff (Aff())
import Control.Monad.Aff.AVar (AVAR())

import Network.HTTP.Affjax (AJAX())

import Handsontable.Types (HOT())

import Data.Tuple

import DOM

type Effects eff =
  ( dom :: DOM
  , avar :: AVAR
  , err :: EXCEPTION
  , console :: CONSOLE
  , ajax :: AJAX
  , hot :: HOT
  , random :: RANDOM
  | eff )

type Metrix = Aff (Effects ())

--

type TemplateId         = Int
type TableId            = Int
type AxisId             = Int
type OrdinateId         = Int
type MemberId           = Int
type CellId             = Int

type FrameworkId        = Int
type TaxonomyId         = Int
type ConceptualModuleId = Int
type ModuleId           = Int
type TemplateGroupId    = Int

type CustomMemberId     = String
type SubsetMemberId     = Int
type UpdateId           = Int

type XBRLCode           = String
type Label              = String
type XBRLCodeSet        = Array (Tuple XBRLCode Label)
