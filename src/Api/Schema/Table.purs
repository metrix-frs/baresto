module Api.Schema.Table where

import Api.Schema.Common (getPair)
import Data.Foreign (ForeignError(JSONError), fail)
import Data.Foreign.Class (class IsForeign, readProp, read)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Maybe (Maybe)
import Prelude
import Types (OrdinateId, MemberId, XBRLCodeSet, AxisId, CellId, TableId)

type XHeader = Array (Array XHeaderCell)
type Ordinates = Array Ordinate

newtype Table = Table
  { tableId         :: TableId
  , tableName       :: String
  , tableGrid       :: Grid
  , tableIsHeader   :: Boolean
  , tableXHeader    :: XHeader
  , tableXOrdinates :: Ordinates
  , tableZAxis      :: ZAxis
  , tableYAxis      :: YAxis
  }

instance isForeignTable :: IsForeign Table where
  read json = do
    tbl <- { tableId: _
           , tableName: _
           , tableGrid: _
           , tableIsHeader: _
           , tableXHeader: _
           , tableXOrdinates: _
           , tableZAxis: _
           , tableYAxis: _
           }
      <$> readProp "id"         json
      <*> readProp "name"       json
      <*> readProp "grid"       json
      <*> readProp "isHeader"   json
      <*> readProp "xHeader"    json
      <*> readProp "xOrdinates" json
      <*> readProp "zAxis"      json
      <*> readProp "yAxis"      json
    pure $ Table tbl

data Grid = Grid (Array Sheet)

instance isForeignGrid :: IsForeign Grid where
  read json = Grid <$> read json

data Sheet = Sheet (Array Row)

instance isForeignSheet :: IsForeign Sheet where
  read json = Sheet <$> read json

data Row = Row (Array Cell)

instance isForeignRow :: IsForeign Row where
  read json = Row <$> read json

data Cell
  = ShadedCell
  | FactCell CellId DataType
  | YMemberCell CellId
  | NoCell

instance isForeignCell :: IsForeign Cell where
  read json = do
    cellType <- readProp "type" json
    case cellType of
      "shaded"  -> pure ShadedCell
      "fact"    -> FactCell <$> readProp "id" json <*> readProp "dataType" json
      "yMember" -> YMemberCell <$> readProp "id" json
      "noCell"  -> pure NoCell
      _         -> fail $ JSONError "expected `shaded`, `fact`, `yMember` or `noCell`"

newtype XHeaderCell = XHeaderCell
  { colspan  :: Int
  , ordinate :: Maybe Ordinate
  }

instance isForeignXHeaderCell :: IsForeign XHeaderCell where
  read json = do
    cell <- { colspan: _, ordinate: _ }
      <$> readProp "colspan" json
      <*> (unNullOrUndefined <$> readProp "ordinate" json)
    pure $ XHeaderCell cell

data ZAxis
  = ZAxisSingleton
  | ZAxisClosed AxisId Ordinates
  | ZAxisCustom AxisId String
  | ZAxisSubset AxisId String (Array SubsetMemberOption)

instance isForeignZAxis :: IsForeign ZAxis where
  read json = do
    axisType <- readProp "type" json
    case axisType of
      "singleton" -> pure ZAxisSingleton
      "closed"    -> ZAxisClosed <$> readProp "id" json
                                 <*> readProp "ordinates" json
      "custom"    -> ZAxisCustom <$> readProp "id" json
                                 <*> readProp "label" json
      "subset"    -> ZAxisSubset <$> readProp "id" json
                                 <*> readProp "label" json
                                 <*> readProp "options" json
      _           -> fail $ JSONError "expected `singleton`, `closed`, `custom` or `subset`"

data YAxis
  = YAxisClosed AxisId Ordinates
  | YAxisCustom AxisId String

instance isForeignYAxis :: IsForeign YAxis where
  read json = do
    axisType <- readProp "type" json
    case axisType of
      "closed" -> YAxisClosed <$> readProp "id" json
                              <*> readProp "ordinates" json
      "custom" -> YAxisCustom <$> readProp "id" json
                              <*> readProp "label" json
      _        -> fail $ JSONError "expected `closed` or `custom`"

data DataType
  = BooleanData
  | DateData
  | IntegerData
  | MonetaryData
  | PercentageData
  | CodeData XBRLCodeSet
  | StringData
  | NumberData

instance isForeignDataType :: IsForeign DataType where
  read json = do
    dataType <- readProp "type" json
    case dataType of
      "boolean"    -> pure BooleanData
      "date"       -> pure DateData
      "integer"    -> pure IntegerData
      "monetary"   -> pure MonetaryData
      "percentage" -> pure PercentageData
      "code"       -> CodeData <<< map getPair <$> readProp "xbrlCodeSet" json
      "string"     -> pure StringData
      "number"     -> pure NumberData
      _            -> fail $ JSONError "unexpected data type"

newtype SubsetMemberOption = SubsetMemberOption
  { memberId    :: MemberId
  , memberLabel :: String
  , memberLevel :: Int
  }

instance isForeignMember :: IsForeign SubsetMemberOption where
  read json = do
    m <- { memberId: _, memberLabel: _, memberLevel: _ }
      <$> readProp "id"    json
      <*> readProp "label" json
      <*> readProp "level" json
    pure $ SubsetMemberOption m

newtype Ordinate = Ordinate
  { ordinateLabel      :: String
  , ordinateId         :: OrdinateId
  , ordinateCode       :: String
  , ordinateIsAbstract :: Boolean
  , ordinateLevel      :: Int
  }

instance isForeignOrdinate :: IsForeign Ordinate where
  read json = do
    ord <- { ordinateLabel: _
           , ordinateId: _
           , ordinateCode: _
           , ordinateIsAbstract: _
           , ordinateLevel: _
           }
      <$> readProp "label"    json
      <*> readProp "id"       json
      <*> readProp "code"     json
      <*> readProp "abstract" json
      <*> readProp "level"    json
    pure $ Ordinate ord
