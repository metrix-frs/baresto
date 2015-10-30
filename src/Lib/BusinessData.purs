module Lib.BusinessData
( BusinessData()
, emptyBusinessData
, _BusinessData
, _serverState
, _snapshot
, _customMembers
, CustomMemberStore()
, SubsetMemberStore()
, _subsetMembers
, _updateId
, generateDiff
, doesSheetExist
, gridHeight
, getCellTable
, getFactTable
, getFact
, setFact
, SubsetMember()
, CustomMember()
, getCustomMembers
, getSubsetMembers
, isSubsetMemberSelected
) where

import Prelude

import           Data.Int (fromNumber)
import           Data.Array hiding ((..), filter)
import           Data.List (fromList, toList, filter)
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

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode
import Data.Argonaut.Combinators ((:=), (~>))

import Types

import Lib.Template

import Api.Schema.Template
import Api.Schema.BusinessData
import Api.Schema.BusinessData.Key

import Utils (maxInt, getIndices)


type CustomMember = Tuple CustomMemberId String
type SubsetMember = Tuple SubsetMemberId String

type CustomMemberStore = M.Map (Tuple ModuleId AxisId) (Array CustomMember)
type SubsetMemberStore = M.Map (Tuple ModuleId AxisId) (Array SubsetMember)

newtype BusinessData = BusinessData
  { serverState       :: BDSnapshot
  , snapshot          :: BDSnapshot
  , customMembers     :: CustomMemberStore
  , subsetMembers     :: SubsetMemberStore
  , updateId          :: Maybe (Tuple UpdateId String)
  }

_BusinessData :: LensP BusinessData _
_BusinessData = lens (\(BusinessData r) -> r) (\_ r -> BusinessData r)

_serverState :: LensP BusinessData BDSnapshot
_serverState = _BusinessData .. lens _.serverState _{ serverState = _ }

_snapshot :: LensP BusinessData BDSnapshot
_snapshot = _BusinessData .. lens _.snapshot _{ snapshot = _ }

_customMembers :: LensP BusinessData CustomMemberStore
_customMembers = _BusinessData .. lens _.customMembers _{ customMembers = _ }

_subsetMembers :: LensP BusinessData SubsetMemberStore
_subsetMembers = _BusinessData .. lens _.subsetMembers _{ subsetMembers = _ }

_updateId :: LensP BusinessData (Maybe (Tuple UpdateId String))
_updateId = _BusinessData .. lens _.updateId _{ updateId = _ }

emptyBusinessData :: BusinessData
emptyBusinessData = BusinessData
  { serverState: emptyBDSnapshot
  , snapshot: emptyBDSnapshot
  , customMembers: M.empty
  , subsetMembers: M.empty
  , updateId: Nothing
  }

-- Diff

stateUpdate :: BDSnapshot -> BDUpdate -> BDSnapshot
stateUpdate (BDSnapshot old) (BDUpdate upd) = BDSnapshot $ foldl f old $ M.toList upd
  where f m (Tuple key mVal) = case mVal of
          Just val -> M.insert key val m
          Nothing  -> M.delete key m

stateDiff :: BDSnapshot -> BDSnapshot -> BDUpdate
stateDiff (BDSnapshot old) (BDSnapshot new) = BDUpdate $ forOld $ forNew M.empty
  where
    forNew m = foldl fNew m $ M.toList new
    fNew m (Tuple key newVal) = case M.lookup key old of
      Nothing     -> if newVal == ""
                       then m
                       else M.insert key (Just newVal) m
      Just oldVal -> if oldVal == newVal
                       then if newVal == ""
                              then M.insert key Nothing m
                              else m
                       else if newVal == ""
                              then M.insert key Nothing m
                              else M.insert key (Just newVal) m
    forOld m = foldl fOld m $ M.keys old
    fOld m key = case M.lookup key new of
      Nothing -> M.insert key Nothing m
      Just _ -> m

-- Interface

genBDUpdateMsg :: BusinessData -> Maybe BDUpdateMsg
genBDUpdateMsg bd = diff <$> bd ^. _updateId
  where diff (Tuple fv _) = BDUpdateMsg
          { parentUpdateId: fv
          , values: generateDiff bd
          }

generateDiff :: BusinessData -> BDUpdate
generateDiff bd = stateDiff (bd ^. _serverState) (bd ^. _snapshot)

doesSheetExist :: ModuleId -> Table -> BusinessData -> S -> Boolean
doesSheetExist modId (Table tbl) bd (S s) =
  case tbl.tableZAxis of
    ZAxisCustom axisId _   -> isJust $ (getCustomMembers modId axisId bd) !! s
    ZAxisSubset axisId _ _ -> isJust $ (getSubsetMembers modId axisId bd) !! s
    ZAxisClosed _ ords     -> isJust $ ords !! s
    ZAxisSingleton         -> s == 0

gridHeight :: ModuleId -> Table -> BusinessData -> Int
gridHeight modId (Table tbl) bd =
  case tbl.tableYAxis of
    YAxisClosed _ ords -> length ords
    YAxisCustom axId _ -> length $ getCustomMembers modId axId bd

getCellTable :: ModuleId -> S -> Table -> BusinessData -> Maybe (Array (Array Cell))
getCellTable modId s table@(Table tbl) bd =
    if doesSheetExist modId table bd s
      then Just $ case tbl.tableYAxis of
        YAxisClosed _ ords -> row <$> getIndices ords
        YAxisCustom axId _ -> row <$> getIndices (getCustomMembers modId axId bd)
      else Nothing
  where
    row r = cell r <$> getIndices tbl.tableXOrdinates
    cell r c = fromMaybe NoCell $ cellLookup (Coord (C c) (R r) s) table

getFactTable :: ModuleId -> S -> Table -> BusinessData -> Maybe (Array (Array String))
getFactTable modId s table@(Table tbl) bd =
    if doesSheetExist modId table bd s
      then Just $ case tbl.tableYAxis of
        YAxisClosed _ ords -> row <$> getIndices ords
        YAxisCustom axId _ -> row <$> getIndices (getCustomMembers modId axId bd)
      else Nothing
  where
    row r = cell r <$> getIndices tbl.tableXOrdinates
    cell r c = fromMaybe "" $ getFact modId (Coord (C c) (R r) s) table bd

getFact :: ModuleId -> Coord -> Table -> BusinessData -> Maybe String
getFact modId coord table@(Table tbl) bd = do
    (Tuple cellId factType) <- getKeyInfo modId coord table bd
    conv <$> if tbl.tableIsHeader
      then getBDValue (HeaderKey modId cellId) bd
      else getBDValue (FactKey modId factType cellId) bd
  where
    conv k = case cellLookup coord table of
      Just (FactCell _ (CodeData pairs)) ->
        fromMaybe k $ lookup k pairs
      Just (FactCell _ BooleanData) ->
        fromMaybe k $ lookup k boolValueMap
      Just _ -> k
      Nothing -> k
foreign import stripDecimals :: String -> Int -> String

setFact :: ModuleId -> Coord -> Table -> String -> BusinessData -> BusinessData
setFact modId coord table@(Table tbl) val bd =
    case getKeyInfo modId coord table bd of
      Just (Tuple cellId factType) -> if tbl.tableIsHeader
        then setBDValue (HeaderKey modId cellId) (conv val) bd
        else setBDValue (FactKey modId factType cellId) (conv val) bd
      Nothing -> bd
  where
    lookupBySnd v pairs = fst <$> find (\(Tuple a b) -> b == v) pairs
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

getKeyInfo :: ModuleId -> Coord -> Table -> BusinessData -> Maybe (Tuple CellId FactType)
getKeyInfo modId coord@(Coord _ (R r) (S s)) table@(Table tbl) bd =
    case cellLookup coord table of
      Just (FactCell cellId _)  -> Tuple cellId <$> go false
      Just (YMemberCell cellId) -> Tuple cellId <$> go true
      _                         -> Nothing
  where
    go :: Boolean -> Maybe FactType
    go isMemberFact = case tbl.tableZAxis of
      ZAxisCustom axisId _ ->
        (getCustomMembers modId axisId bd) !! s
          <#> \(Tuple memId _) -> CustomZFact axisId memId
      ZAxisSubset axisId _ _ ->
        (getSubsetMembers modId axisId bd) !! s
          <#> \(Tuple memId _) -> SubsetZFact axisId memId
      _ -> case tbl.tableYAxis of
        YAxisClosed _ _ -> Just PlainFact
        YAxisCustom axId _ ->
          (getCustomMembers modId axId bd) !! r
            <#> \(Tuple memId _) -> if isMemberFact
                                      then MemberYFact axId memId
                                      else CustomYFact axId memId

getCustomMembers :: ModuleId -> AxisId -> BusinessData -> Array CustomMember
getCustomMembers modId axId bd =
  fromMaybe [] $ bd ^. _customMembers .. at (Tuple modId axId)

getSubsetMembers :: ModuleId -> AxisId -> BusinessData -> Array SubsetMember
getSubsetMembers modId axId bd =
  fromMaybe [] $ bd ^. _subsetMembers .. at (Tuple modId axId)

isSubsetMemberSelected :: ModuleId -> AxisId -> SubsetMemberId -> BusinessData -> Boolean
isSubsetMemberSelected modId axId memId bd =
  isJust $ getBDValue (SubsetZMemberKey modId axId memId) bd

-- Internal helper

getBDValue :: Key -> BusinessData -> Maybe String
getBDValue key bd = bd ^. _snapshot .. _BDSnapshot .. at key

setBDValue :: Key -> String -> BusinessData -> BusinessData
setBDValue key value bd = bd # _snapshot .. _BDSnapshot .. at key .~ Just value
