module Lib.BusinessData
( CustomYMemberStore()
, CustomZMemberStore()
, SubsetZMemberStore()
, SubsetMember()
, CustomMember()
, BusinessData()
, emptyBusinessData
, _BusinessData
, _snapshot
, _customYMembers
, _customZMembers
, _subsetZMembers
, Edit(..)
, editToUpdate
, applyUpdate
, doesSheetExist
, getMaxSheet
, sheetToZLocation
, gridHeight
, getCellTable
, getFactTable
, getCustomYMembers
, getCustomYMembersBySheet
, getCustomZMembers
, getSubsetZMembers
, isSubsetZMemberSelected
) where

import Data.Array as Array
import Data.Map as M
import Api.Schema.BusinessData (Update(Update))
import Api.Schema.BusinessData.Key (IsRowKey(RowKey, NoRowKey), Key(KeySubsetZSelected, KeyHeaderFact, KeyFact, KeyCustomZMember, KeyCustomRow), YLocation(YLocCustom, YLocClosed), ZLocation(ZLocSubset, ZLocCustom, ZLocClosed, ZLocSingle))
import Api.Schema.BusinessData.Value (Value(Value), updateValue, UpdateValue(UpdateValueData))
import Api.Schema.Table (Cell(YMemberCell, FactCell, NoCell), DataType(BooleanData, CodeData, PercentageData, MonetaryData), Ordinate(Ordinate), Table(Table), YAxis(YAxisCustom, YAxisClosed), ZAxis(ZAxisSubset, ZAxisCustom, ZAxisClosed, ZAxisSingleton))
import Control.Bind (join)
import Control.Monad.State (execState)
import Data.Array ((!!), length, filter, snoc)
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe, Maybe(Just, Nothing), isJust)
import Data.Tuple (Tuple(Tuple), lookup)
import Lib.Table (lookupBySnd, C(C), Coord(Coord), R(R), S(S), cellLookup, boolValueMap)
import Optic.At (at)
import Optic.Core (LensP, (.~), (..), (^.), lens)
import Optic.Iso (non)
import Optic.Monad.Getter (use)
import Optic.Monad.Setter ((%=), (.=))
import Prelude ((#), ($), pure, bind, (<$>), (-), (==), unit, (/=), flip, id, map)
import Types (SubsetMemberId, AxisId, CellId, CustomMemberId)
import Utils (maxInt, getIndices)

type CustomMember = Tuple CustomMemberId String
type SubsetMember = Tuple SubsetMemberId String

type CustomYMemberStore = M.Map (Tuple AxisId ZLocation) (Array CustomMember)
type CustomZMemberStore = M.Map AxisId (Array CustomMember)
type SubsetZMemberStore = M.Map AxisId (Array SubsetMember)

newtype BusinessData = BusinessData
  { snapshot       :: M.Map Key String
  , customYMembers :: CustomYMemberStore
  , customZMembers :: CustomZMemberStore
  , subsetZMembers :: SubsetZMemberStore
  }

_BusinessData :: LensP BusinessData _
_BusinessData = lens (\(BusinessData r) -> r) (\_ r -> BusinessData r)

_snapshot :: LensP BusinessData (M.Map Key String)
_snapshot = _BusinessData .. lens _.snapshot _{ snapshot = _ }

_customYMembers :: LensP BusinessData CustomYMemberStore
_customYMembers = _BusinessData .. lens _.customYMembers _{ customYMembers = _ }

_customZMembers :: LensP BusinessData CustomZMemberStore
_customZMembers = _BusinessData .. lens _.customZMembers _{ customZMembers = _ }

_subsetZMembers :: LensP BusinessData SubsetZMemberStore
_subsetZMembers = _BusinessData .. lens _.subsetZMembers _{ subsetZMembers = _ }

emptyBusinessData :: BusinessData
emptyBusinessData = BusinessData
  { snapshot:       M.empty
  , customYMembers: M.empty
  , customZMembers: M.empty
  , subsetZMembers: M.empty
  }

-- Update

data Edit
  = SetFacts              Table (Array (Tuple Coord String))
  | NewCustomRow          AxisId ZLocation CustomMemberId
  | NewCustomZMember      AxisId           CustomMemberId
  | RenameCustomZMember   AxisId           Int            String
  | DeleteCustomRow       AxisId ZLocation Int
  | DeleteCustomZMember   AxisId           Int
  | SelectSubsetZMember   AxisId           SubsetMemberId
  | DeselectSubsetZMember AxisId           SubsetMemberId

foreign import stripDecimals :: String -> Int -> String

editToUpdate :: Edit -> BusinessData -> Maybe Update
editToUpdate bde bd = case bde of
  SetFacts table changes ->
    let list = join $ (go table) <$> changes
    in  if Array.null list
          then Nothing
          else Just $ Update list
  NewCustomRow axId zLoc cm ->
    single (KeyCustomRow axId zLoc cm) $ Just "customY"
  NewCustomZMember axId cm ->
    single (KeyCustomZMember axId cm) $ Just ""
  RenameCustomZMember axId index name -> do
    (Tuple cm _) <- getCustomZMembers axId bd !! index
    single (KeyCustomZMember axId cm) $ Just name
  DeleteCustomRow axId zLoc index -> do
    (Tuple cm _) <- getCustomYMembers axId zLoc bd !! index
    single (KeyCustomRow axId zLoc cm) Nothing
  DeleteCustomZMember axId index -> do
    (Tuple cm _) <- getCustomZMembers axId bd !! index
    single (KeyCustomZMember axId cm) Nothing
  SelectSubsetZMember axId sm ->
    single (KeySubsetZSelected axId sm) $ Just "selected"
  DeselectSubsetZMember axId sm ->
    single (KeySubsetZSelected axId sm) Nothing
  where
    go :: Table -> Tuple Coord String -> Array (Tuple Key UpdateValue)
    go table (Tuple coord new) = case getKey coord table bd of
        Just key ->
          let old = getBDValue key bd
              convNew = Just $ conv new
          in  if  old == convNew
                then []
                else [Tuple key (UpdateValueData convNew)]
        Nothing -> []
      where
        conv v = case cellLookup coord table of
          Just (FactCell _ dType) -> case dType of
            CodeData pairs ->
              fromMaybe v $ lookupBySnd v pairs
            BooleanData ->
              fromMaybe v $ lookupBySnd v boolValueMap
            MonetaryData -> v
            PercentageData -> v
            _ -> v
          Just _ -> v
          Nothing -> v

    single :: Key -> Maybe String -> Maybe Update
    single key new =
      let old = getBDValue key bd in
      if  old == new
        then Nothing
        else Just $ Update [Tuple key (UpdateValueData new)]

applyUpdate :: Update -> BusinessData -> BusinessData
applyUpdate (Update list) bd' = foldl go bd' list
  where
    go bd (Tuple key upd) = bd # execState do
      old <- use $ _snapshot .. at key
      let new = case updateValue upd (Value { valueData: old, valuePrecision: Nothing }) of
                  Value v -> v.valueData
      _snapshot .. at key .= new
      case key of
        KeyCustomRow axId zLoc cm ->
          _customYMembers .. at (Tuple axId zLoc) .. non [] %= case upd of
            UpdateValueData (Just new') -> flip snoc (Tuple cm new')
            UpdateValueData Nothing     -> filter (\(Tuple cm' (_ :: String)) -> cm /= cm')
            _                           -> id
        KeyCustomZMember axId cm ->
          _customZMembers .. at axId .. non [] %= case Tuple old upd of
            Tuple (Just _) (UpdateValueData (Just new')) ->
              map (\(Tuple cm' val) -> if cm == cm' then (Tuple cm' new') else (Tuple cm' val))
            Tuple Nothing (UpdateValueData (Just new')) ->
              flip snoc (Tuple cm new')
            Tuple (Just _) (UpdateValueData Nothing) ->
              filter (\(Tuple cm' (_ :: String)) -> cm /= cm')
            _ ->
              id
        KeySubsetZSelected axId sm ->
          _subsetZMembers .. at axId .. non [] %= case upd of
            UpdateValueData (Just new') -> flip snoc (Tuple sm new')
            UpdateValueData Nothing     -> filter (\(Tuple sm' (_ :: String)) -> sm /= sm')
            _                           -> id
        _ -> pure unit

-- Interface

doesSheetExist :: S -> Table -> BusinessData -> Boolean
doesSheetExist (S s) (Table tbl) bd =
  case tbl.tableZAxis of
    ZAxisCustom axisId _   -> isJust $ (getCustomZMembers axisId bd) !! s
    ZAxisSubset axisId _ _ -> isJust $ (getSubsetZMembers axisId bd) !! s
    ZAxisClosed _ ords     -> isJust $ ords !! s
    ZAxisSingleton         -> s == 0

getMaxSheet :: Table -> BusinessData -> S
getMaxSheet (Table tbl) bd = S $ maxInt 0 $
  case tbl.tableZAxis of
    ZAxisCustom axisId _   -> length (getCustomZMembers axisId bd) - 1
    ZAxisSubset axisId _ _ -> length (getSubsetZMembers axisId bd) - 1
    ZAxisClosed _ ords     -> length ords - 1
    ZAxisSingleton         -> 0

sheetToZLocation :: S -> Table -> BusinessData -> Maybe ZLocation
sheetToZLocation (S s) (Table tbl) bd = case tbl.tableZAxis of
  ZAxisSingleton       -> pure ZLocSingle
  ZAxisClosed _ ords   -> do
    (Ordinate ord) <- ords !! s
    pure $ ZLocClosed (ord.ordinateId)
  ZAxisCustom axId _   -> do
    (Tuple cmId _) <- getCustomZMembers axId bd !! s
    pure $ ZLocCustom axId cmId
  ZAxisSubset axId _ _ -> do
    (Tuple smId _) <- getSubsetZMembers axId bd !! s
    pure $ ZLocSubset axId smId

gridHeight :: S -> Table -> BusinessData -> Maybe Int
gridHeight s table@(Table tbl) bd = do
  zLoc <- sheetToZLocation s table bd
  case tbl.tableYAxis of
    YAxisClosed _ ords -> pure $ length ords
    YAxisCustom axId _ -> pure $ length $ getCustomYMembers axId zLoc bd

getCellTable :: S -> Table -> BusinessData -> Maybe (Array (Array Cell))
getCellTable s table@(Table tbl) bd = do
    zLoc <- sheetToZLocation s table bd
    pure $ case tbl.tableYAxis of
      YAxisClosed _ ords -> row <$> getIndices ords
      YAxisCustom axId _ -> row <$> getIndices (getCustomYMembers axId zLoc bd)
  where
    row r = cell r <$> getIndices tbl.tableXOrdinates
    cell r c = fromMaybe NoCell $ cellLookup (Coord (C c) (R r) s) table

getFactTable :: S -> Table -> BusinessData -> Maybe (Array (Array String))
getFactTable s table@(Table tbl) bd = do
    zLoc <- sheetToZLocation s table bd
    pure $ case tbl.tableYAxis of
      YAxisClosed _ ords -> row <$> getIndices ords
      YAxisCustom axId _ -> row <$> getIndices (getCustomYMembers axId zLoc bd)
  where
    row r = cell r <$> getIndices tbl.tableXOrdinates
    cell r c = getFact (Coord (C c) (R r) s) table bd

getFact :: Coord -> Table -> BusinessData -> String
getFact coord table@(Table tbl) bd = case getKey coord table bd of
    Nothing -> ""
    Just key -> conv $ fromMaybe "" $ getBDValue key bd
  where
    conv k = case cellLookup coord table of
      Just (FactCell _ (CodeData pairs)) ->
        fromMaybe k $ lookup k pairs
      Just (FactCell _ BooleanData) ->
        fromMaybe k $ lookup k boolValueMap
      Just _ -> k
      Nothing -> k

getKey :: Coord -> Table -> BusinessData -> Maybe Key
getKey coord@(Coord _ (R r) (S s)) table@(Table tbl) bd =
    case cellLookup coord table of
      Just (FactCell cellId _)  -> if tbl.tableIsHeader
                                     then Just $ KeyHeaderFact cellId
                                     else go cellId NoRowKey
      Just (YMemberCell cellId) -> go cellId RowKey
      _                         -> Nothing
  where
    go :: CellId -> IsRowKey -> Maybe Key
    go cellId isRowKey = do
      zLoc <- sheetToZLocation (S s) table bd
      yLoc <- case tbl.tableYAxis of
        YAxisClosed _ _      -> pure YLocClosed
        YAxisCustom axId _   -> do
          (Tuple cmId _) <- getCustomYMembers axId zLoc bd !! r
          pure $ YLocCustom axId cmId
      pure $ KeyFact cellId isRowKey yLoc zLoc

getCustomYMembers :: AxisId -> ZLocation -> BusinessData -> Array CustomMember
getCustomYMembers axId zLoc bd =
  fromMaybe [] $ bd ^. _customYMembers .. at (Tuple axId zLoc)

getCustomYMembersBySheet :: AxisId -> S -> Table -> BusinessData -> Array CustomMember
getCustomYMembersBySheet axId s table bd = case sheetToZLocation s table bd of
  Just zLoc -> getCustomYMembers axId zLoc bd
  Nothing   -> []

getCustomZMembers :: AxisId -> BusinessData -> Array CustomMember
getCustomZMembers axId bd =
  fromMaybe [] $ bd ^. _customZMembers .. at axId

getSubsetZMembers :: AxisId -> BusinessData -> Array SubsetMember
getSubsetZMembers axId bd =
  fromMaybe [] $ bd ^. _subsetZMembers .. at axId

isSubsetZMemberSelected :: AxisId -> SubsetMemberId -> BusinessData -> Boolean
isSubsetZMemberSelected axId memId bd =
  isJust $ getBDValue (KeySubsetZSelected axId memId) bd

-- Internal helper

getBDValue :: Key -> BusinessData -> Maybe String
getBDValue key bd = bd ^. _snapshot .. at key

setBDValue :: Key -> String -> BusinessData -> BusinessData
setBDValue key value bd = bd # _snapshot .. at key .~ Just value
