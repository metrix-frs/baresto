module Types where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Data.Date (Date)
import Data.Foreign (ForeignError(JSONError), fail)
import Data.Foreign.Class (class IsForeign, read)
import Data.JSDate (toDate)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple)
import Handsontable.Types (HOT)
import Network.HTTP.Affjax (AJAX)
import Prelude

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
    mDate <- toDate <$> read json
    case mDate of
      Just date -> pure $ UTCTime date
      Nothing -> fail $ JSONError "Could not read date."

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
