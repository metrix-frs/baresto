module Lib.BusinessData
( CustomMemberStore()
, SubsetMemberStore()
, SubsetMember()
, CustomMember()
, BusinessData()
, emptyBusinessData
, _BusinessData
, _snapshot
, _customMembers
, _subsetMembers
, Edit(..)
, invertUpdate
, editToUpdate
, applyUpdate
, doesSheetExist
, gridHeight
, getCellTable
, getFactTable
, getCustomMembers
, getSubsetMembers
, isSubsetMemberSelected
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

type CustomMemberStore = M.Map AxisId (Array CustomMember)
type SubsetMemberStore = M.Map AxisId (Array SubsetMember)

newtype BusinessData = BusinessData
  { snapshot          :: M.Map Key String
  , customMembers     :: CustomMemberStore
  , subsetMembers     :: SubsetMemberStore
  }

_BusinessData :: LensP BusinessData _
_BusinessData = lens (\(BusinessData r) -> r) (\_ r -> BusinessData r)

_snapshot :: LensP BusinessData (M.Map Key String)
_snapshot = _BusinessData .. lens _.snapshot _{ snapshot = _ }

_customMembers :: LensP BusinessData CustomMemberStore
_customMembers = _BusinessData .. lens _.customMembers _{ customMembers = _ }

_subsetMembers :: LensP BusinessData SubsetMemberStore
_subsetMembers = _BusinessData .. lens _.subsetMembers _{ subsetMembers = _ }

emptyBusinessData :: BusinessData
emptyBusinessData = BusinessData
  { snapshot:      M.empty
  , customMembers: M.empty
  , subsetMembers: M.empty
  }

-- Update

data Edit
  = SetFacts              Table (Array (Tuple Coord String))
  | NewCustomYOrdinate    AxisId CustomMemberId
  | NewCustomZMember      AxisId CustomMemberId String
  | RenameCustomZMember   AxisId Int            String
  | DeleteCustomYOrdinate AxisId Int
  | DeleteCustomZMember   AxisId Int
  | SelectSubsetZMember   AxisId SubsetMemberId
  | DeselectSubsetZMember AxisId SubsetMemberId

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
  NewCustomYOrdinate axId cm ->
    single (KeyCustomYOrdinate axId cm) (Just "customY")
  NewCustomZMember axId cm name ->
    single (KeyCustomZMember axId cm) (Just name)
  RenameCustomZMember axId index name -> do
    (Tuple cm _) <- getCustomMembers axId bd !! index
    single (KeyCustomZMember axId cm) (Just name)
  DeleteCustomYOrdinate axId index -> do
    (Tuple cm _) <- getCustomMembers axId bd !! index
    single (KeyCustomYOrdinate axId cm) Nothing
  DeleteCustomZMember axId index -> do
    (Tuple cm _) <- getCustomMembers axId bd !! index
    single (KeyCustomZMember axId cm) Nothing
  SelectSubsetZMember axId sm ->
    single (KeySubsetZSelected axId sm) (Just "selected")
  DeselectSubsetZMember axId sm ->
    single (KeySubsetZSelected axId sm) Nothing
  where
    go table m (Tuple coord val) = case getKey coord table bd of
        Just key ->
          let old = getBDValue key bd
              new = if val == "" then Nothing else Just val
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
        KeyCustomYOrdinate axId cm ->
          _customMembers .. at axId .. non [] %= case new of
            Just newVal ->
              flip snoc (Tuple cm newVal)
            Nothing ->
              filter (\(Tuple cm' (_ :: String)) -> cm /= cm')
        KeyCustomZMember axId cm ->
          _customMembers .. at axId .. non [] %= case Tuple old new of
            Tuple (Just _) (Just newVal) ->
              map (\(Tuple cm' val) -> if cm == cm' then (Tuple cm' newVal) else (Tuple cm' val))
            Tuple Nothing (Just newVal) ->
              flip snoc (Tuple cm newVal)
            Tuple (Just _) Nothing ->
              filter (\(Tuple cm' (_ :: String)) -> cm /= cm')
            Tuple Nothing Nothing ->
              id
        KeySubsetZSelected axId sm ->
          _subsetMembers .. at axId .. non [] %= case new of
            Just newVal ->
              flip snoc (Tuple sm newVal)
            Nothing ->
              filter (\(Tuple sm' (_ :: String)) -> sm /= sm')
        _ -> pure unit

-- Interface

doesSheetExist :: Table -> BusinessData -> S -> Boolean
doesSheetExist (Table tbl) bd (S s) =
  case tbl.tableZAxis of
    ZAxisCustom axisId _   -> isJust $ (getCustomMembers axisId bd) !! s
    ZAxisSubset axisId _ _ -> isJust $ (getSubsetMembers axisId bd) !! s
    ZAxisClosed _ ords     -> isJust $ ords !! s
    ZAxisSingleton         -> s == 0

gridHeight :: Table -> BusinessData -> Int
gridHeight (Table tbl) bd =
  case tbl.tableYAxis of
    YAxisClosed _ ords -> length ords
    YAxisCustom axId _ -> length $ getCustomMembers axId bd

getCellTable :: S -> Table -> BusinessData -> Maybe (Array (Array Cell))
getCellTable s table@(Table tbl) bd =
    if doesSheetExist table bd s
      then Just $ case tbl.tableYAxis of
        YAxisClosed _ ords -> row <$> getIndices ords
        YAxisCustom axId _ -> row <$> getIndices (getCustomMembers axId bd)
      else Nothing
  where
    row r = cell r <$> getIndices tbl.tableXOrdinates
    cell r c = fromMaybe NoCell $ cellLookup (Coord (C c) (R r) s) table

getFactTable :: S -> Table -> BusinessData -> Maybe (Array (Array String))
getFactTable s table@(Table tbl) bd =
    if doesSheetExist table bd s
      then Just $ case tbl.tableYAxis of
        YAxisClosed _ ords -> row <$> getIndices ords
        YAxisCustom axId _ -> row <$> getIndices (getCustomMembers axId bd)
      else Nothing
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
                                     then go cellId false
                                     else Just $ KeyHeaderFact cellId
      Just (YMemberCell cellId) -> go cellId true
      _                         -> Nothing
  where
    go :: CellId -> Boolean -> Maybe Key
    go cellId isMemberFact = case Tuple tbl.tableYAxis tbl.tableZAxis of
        Tuple (YAxisClosed _ _) (ZAxisCustom axisId _) ->
          (getCustomMembers axisId bd) !! s
            <#> \(Tuple memId _) -> KeyCustomZFact cellId axisId memId
        Tuple (YAxisClosed _ _) (ZAxisSubset axisId _ _) ->
          (getSubsetMembers axisId bd) !! s
            <#> \(Tuple memId _) -> KeySubsetZFact cellId axisId memId
        Tuple (YAxisClosed _ _) (ZAxisSingleton)
          -> Just $ KeyPlainFact cellId
        Tuple (YAxisClosed _ _) (ZAxisClosed _ _)
          -> Just $ KeyPlainFact cellId
        Tuple (YAxisCustom axId _) (ZAxisSingleton) ->
          customY axId
        Tuple (YAxisCustom axId _) (ZAxisClosed _ _) ->
          customY axId
        Tuple (YAxisCustom axYId _) (ZAxisCustom axZId _) -> do
          (Tuple memYId _) <- getCustomMembers axYId bd !! r
          (Tuple memZId _) <- getCustomMembers axZId bd !! s
          Just $ if isMemberFact
            then KeyMemberYZFact cellId axYId memYId axZId memZId
            else KeyCustomYZFact cellId axYId memYId axZId memZId
        _ ->
          -- unsupported combination of y and z axis
          Nothing
      where
        customY axId = (getCustomMembers axId bd) !! r
          <#> \(Tuple memId _) -> if isMemberFact
                                    then KeyMemberYFact cellId axId memId
                                    else KeyCustomYFact cellId axId memId

getCustomMembers :: AxisId -> BusinessData -> Array CustomMember
getCustomMembers axId bd =
  fromMaybe [] $ bd ^. _customMembers .. at axId

getSubsetMembers :: AxisId -> BusinessData -> Array SubsetMember
getSubsetMembers axId bd =
  fromMaybe [] $ bd ^. _subsetMembers .. at axId

isSubsetMemberSelected :: AxisId -> SubsetMemberId -> BusinessData -> Boolean
isSubsetMemberSelected axId memId bd =
  isJust $ getBDValue (KeySubsetZSelected axId memId) bd

-- Internal helper

getBDValue :: Key -> BusinessData -> Maybe String
getBDValue key bd = bd ^. _snapshot .. at key

setBDValue :: Key -> String -> BusinessData -> BusinessData
setBDValue key value bd = bd # _snapshot .. at key .~ Just value
