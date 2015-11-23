module Types where

import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Random (RANDOM())
import Control.Monad.Eff.Ref (REF())

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
  , ref :: REF
  | eff )

type Metrix = Aff (Effects ())

--

type FrameworkId        = Int
type TaxonomyId         = Int
type ConceptualModuleId = Int
type ModuleId           = Int
type TemplateGroupId    = Int
type TemplateId         = Int

type FileId             = Int
type UpdateId           = Int
type TagId              = Int

type TableId            = Int
type AxisId             = Int
type OrdinateId         = Int
type MemberId           = Int
type CellId             = Int

type CustomMemberId     = String
type SubsetMemberId     = Int
type RowKey             = String

type XBRLCode           = String
type Label              = String
type XBRLCodeSet        = Array (Tuple XBRLCode Label)

type UTCTime            = String
