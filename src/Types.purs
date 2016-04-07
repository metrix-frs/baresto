module Types where

import Prelude

import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Random (RANDOM())
import Control.Monad.Eff.Ref (REF())

import Control.Monad.Aff (Aff())
import Control.Monad.Aff.AVar (AVAR())

import Data.Date
import Data.Date.UTC
import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class

import Network.HTTP.Affjax (AJAX())

import Handsontable.Types (HOT())

import Data.Tuple

import DOM

type Effects =
  ( dom :: DOM
  , avar :: AVAR
  , err :: EXCEPTION
  , console :: CONSOLE
  , ajax :: AJAX
  , hot :: HOT
  , random :: RANDOM
  , ref :: REF
  )

type Metrix = Aff Effects

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

newtype UTCTime = UTCTime Date

instance isForeignUTCTime :: IsForeign UTCTime where
  read json = do
    mDate <- fromString <$> read json
    case mDate of
      Just date -> pure $ UTCTime date
      Nothing -> Left $ JSONError "Could not read date."

-- TODO: think about way of running this effectful function in eff monad
foreign import showDate :: Date -> String
foreign import showDayImpl :: Date -> String

showDay :: UTCTime -> String
showDay (UTCTime date) = showDayImpl date

instance showUTCTime :: Show UTCTime where
  show (UTCTime date) = showDate date

--

type ErrorDetail =
  { title :: String
  , body :: String
  }
