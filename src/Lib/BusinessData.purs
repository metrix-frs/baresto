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

type CustomMemberStore = M.Map AxisId (Array CustomMember)
type SubsetMemberStore = M.Map AxisId (Array SubsetMember)

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

foreign import stripDecimals :: String -> Int -> String

setFact :: Coord -> Table -> String -> BusinessData -> BusinessData
setFact coord table@(Table tbl) val bd =
    case getKey coord table bd of
      Just key -> setBDValue key (conv val) bd
      Nothing -> bd
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
getBDValue key bd = bd ^. _snapshot .. _BDSnapshot .. at key

setBDValue :: Key -> String -> BusinessData -> BusinessData
setBDValue key value bd = bd # _snapshot .. _BDSnapshot .. at key .~ Just value
