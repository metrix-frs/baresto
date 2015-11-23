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
, invertUpdate
, editToUpdate
, applyUpdate
, doesSheetExist
, sheetToZLocation
, gridHeight
, getCellTable
, getFactTable
, getCustomYMembers
, getCustomZMembers
, getSubsetZMembers
, isSubsetZMemberSelected
) where

import Prelude

import           Data.Int (fromNumber)
import           Data.Array hiding ((..))
import           Data.Maybe
import           Data.Tuple
import qualified Data.Map as M
import           Data.Foldable

import           Control.Apply
import           Control.Monad.State
import           Control.Monad.State.Class

import Optic.At (at)
import Optic.Core
import Optic.Refractor.Prism (_Just)
import Optic.Fold ((^?))
import Optic.Monad.Setter
import Optic.Monad.Getter
import Optic.Iso (non)

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode
import Data.Argonaut.Combinators ((:=), (~>))

import Types

import Lib.Table

import Api.Schema.Table
import Api.Schema.BusinessData
import Api.Schema.BusinessData.Key

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
  | NewCustomZMember      AxisId           CustomMemberId String
  | RenameCustomZMember   AxisId           Int            String
  | DeleteCustomRow       AxisId ZLocation Int
  | DeleteCustomZMember   AxisId           Int
  | SelectSubsetZMember   AxisId           SubsetMemberId
  | DeselectSubsetZMember AxisId           SubsetMemberId

invertUpdate :: Update -> Update
invertUpdate (Update m) = Update $ foldl invert M.empty $ M.toList m
  where
    invert m' (Tuple key (Tuple old new)) = M.insert key (Tuple new old) m'

foreign import stripDecimals :: String -> Int -> String

editToUpdate :: Edit -> BusinessData -> Maybe Update
editToUpdate bde bd = case bde of
  SetFacts table changes ->
    let m = foldl (go table) M.empty changes
    in  if m == M.empty
          then Nothing
          else Just $ Update m
  NewCustomRow axId zLoc cm ->
    single (KeyCustomRow axId zLoc cm) (Just "customY")
  NewCustomZMember axId cm name ->
    single (KeyCustomZMember axId cm) (Just name)
  RenameCustomZMember axId index name -> do
    (Tuple cm _) <- getCustomZMembers axId bd !! index
    single (KeyCustomZMember axId cm) (Just name)
  DeleteCustomRow axId zLoc index -> do
    (Tuple cm _) <- getCustomYMembers axId zLoc bd !! index
    single (KeyCustomRow axId zLoc cm) Nothing
  DeleteCustomZMember axId index -> do
    (Tuple cm _) <- getCustomZMembers axId bd !! index
    single (KeyCustomZMember axId cm) Nothing
  SelectSubsetZMember axId sm ->
    single (KeySubsetZSelected axId sm) (Just "selected")
  DeselectSubsetZMember axId sm ->
    single (KeySubsetZSelected axId sm) Nothing
  where
    go table m (Tuple coord val) = case getKey coord table bd of
        Just key ->
          let old = getBDValue key bd
              new = if val == "" then Nothing else Just (conv val)
          in  if  old == new
                then m
                else M.insert key (Tuple old new) m
        Nothing -> m
      where
        conv v = case cellLookup coord table of
          Just (FactCell _ dType) -> case dType of
            CodeData pairs ->
              fromMaybe v $ lookupBySnd v pairs
            BooleanData ->
              fromMaybe v $ lookupBySnd v boolValueMap
            MonetaryData ->
              stripDecimals v 2
            PercentageData ->
              stripDecimals v 4
            _ -> v
          Just _ -> v
          Nothing -> v
        lookupBySnd v pairs = fst <$> find (\(Tuple a b) -> b == v) pairs

    single key new =
      let old = getBDValue key bd in
      if  old == new
        then Nothing
        else Just $ Update $ M.singleton key (Tuple old new)

applyUpdate :: Update -> BusinessData -> BusinessData
applyUpdate (Update m) bd' = foldl go bd' $ M.toList m
  where
    go bd (Tuple key (Tuple old new)) = bd # execState do
      _snapshot .. at key .= new
      case key of
        KeyCustomRow axId zLoc cm ->
          _customYMembers .. at (Tuple axId zLoc) .. non [] %= case new of
            Just newVal ->
              flip snoc (Tuple cm newVal)
            Nothing ->
              filter (\(Tuple cm' (_ :: String)) -> cm /= cm')
        KeyCustomZMember axId cm ->
          _customZMembers .. at axId .. non [] %= case Tuple old new of
            Tuple (Just _) (Just newVal) ->
              map (\(Tuple cm' val) -> if cm == cm' then (Tuple cm' newVal) else (Tuple cm' val))
            Tuple Nothing (Just newVal) ->
              flip snoc (Tuple cm newVal)
            Tuple (Just _) Nothing ->
              filter (\(Tuple cm' (_ :: String)) -> cm /= cm')
            Tuple Nothing Nothing ->
              id
        KeySubsetZSelected axId sm ->
          _subsetZMembers .. at axId .. non [] %= case new of
            Just newVal ->
              flip snoc (Tuple sm newVal)
            Nothing ->
              filter (\(Tuple sm' (_ :: String)) -> sm /= sm')
        _ -> pure unit

-- Interface

doesSheetExist :: S -> Table -> BusinessData -> Boolean
doesSheetExist (S s) (Table tbl) bd =
  case tbl.tableZAxis of
    ZAxisCustom axisId _   -> isJust $ (getCustomZMembers axisId bd) !! s
    ZAxisSubset axisId _ _ -> isJust $ (getSubsetZMembers axisId bd) !! s
    ZAxisClosed _ ords     -> isJust $ ords !! s
    ZAxisSingleton         -> s == 0

sheetToZLocation :: S -> Table -> BusinessData -> Maybe ZLocation
sheetToZLocation (S s) (Table tbl) bd = case tbl.tableZAxis of
  ZAxisSingleton       -> pure ZLocClosed
  ZAxisClosed _ _      -> pure ZLocClosed
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
    cell r c = fromMaybe "" $ getFact (Coord (C c) (R r) s) table bd

getFact :: Coord -> Table -> BusinessData -> Maybe String
getFact coord table@(Table tbl) bd = do
    key <- getKey coord table bd
    conv <$> getBDValue key bd
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
      zLoc <- case tbl.tableZAxis of
        ZAxisSingleton       -> pure ZLocClosed
        ZAxisClosed _ _      -> pure ZLocClosed
        ZAxisCustom axId _   -> do
          (Tuple cmId _) <- getCustomZMembers axId bd !! s
          pure $ ZLocCustom axId cmId
        ZAxisSubset axId _ _ -> do
          (Tuple smId _) <- getSubsetZMembers axId bd !! s
          pure $ ZLocSubset axId smId
      yLoc <- case tbl.tableYAxis of
        YAxisClosed _ _      -> pure YLocClosed
        YAxisCustom axId _   -> do
          (Tuple cmId _) <- getCustomYMembers axId zLoc bd !! s
          pure $ YLocCustom axId cmId
      pure $ KeyFact cellId isRowKey yLoc zLoc

getCustomYMembers :: AxisId -> ZLocation -> BusinessData -> Array CustomMember
getCustomYMembers axId zLoc bd =
  fromMaybe [] $ bd ^. _customYMembers .. at (Tuple axId zLoc)

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
