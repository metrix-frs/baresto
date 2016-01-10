module Api.Schema.Validation where

import Prelude

import Data.Either
import Data.StrMap as SM
import Data.Maybe
import Data.Tuple
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Keys
import Data.Foreign.NullOrUndefined
import Data.Traversable (traverse)
import Data.List (toList)

import Types

newtype ValidationResult = ValidationResult
  { vrDpmFindings    :: SM.StrMap (Array Finding)
  , vrHeaderFindings :: Maybe (Array Finding)
  }

emptyValidationResult :: ValidationResult
emptyValidationResult = ValidationResult
  { vrDpmFindings: SM.empty
  , vrHeaderFindings: Nothing
  }

newtype RuleMap = RuleMap (SM.StrMap (Array Finding))

instance isForeignRuleMap :: IsForeign RuleMap where
  read json = do
    (obj :: SM.StrMap Foreign) <- unsafeReadTagged "Object" json
    -- TODO: report purescript-maps StrMap functions traverse, union, foldM not stack-safe
    pure $ RuleMap $ obj <#> \val -> either (const []) id (read val)

instance isForeignValidationResult :: IsForeign ValidationResult where
  read json = do
    (RuleMap dpm) <- readProp "dpm" json
    header <- runNullOrUndefined <$> readProp "header" json
    pure $ ValidationResult
      { vrDpmFindings: dpm
      , vrHeaderFindings: header
      }

newtype Finding = Finding
  { finCode :: String
  , finMessage :: String
  , finTableBasedFormula :: Maybe String
  , finFormula :: Maybe Formula
  }

instance isForeignFinding :: IsForeign Finding where
  read json = do
    fin <- { finCode: _
           , finMessage: _
           , finTableBasedFormula: _
           , finFormula: _
           }
      <$> readProp "code" json
      <*> readProp "message" json
      <*> (runNullOrUndefined <$> readProp "tableBasedFormula" json)
      <*> (runNullOrUndefined <$> readProp "formula" json)
    pure $ Finding fin

newtype Hole = Hole
  { holeData :: Maybe String
  , holeCoords :: HoleCoords
  , holeTemplate :: String
  }

instance isForeignHole :: IsForeign Hole where
  read json = do
    h <- { holeData: _
         , holeCoords: _
         , holeTemplate: _
         }
      <$> (runNullOrUndefined <$> readProp "data" json)
      <*> readProp "coords" json
      <*> readProp "template" json
    pure $ Hole h

type PlainOrd   = Tuple Int String
type CustomYOrd = Tuple CustomMemberId (Array String)
type CustomZOrd = Tuple CustomMemberId String
type SubsetZOrd = SubsetMemberId


data HoleCoords = HoleCoords HoleCoordX HoleCoordY HoleCoordZ

instance isForeignHoleCoords :: IsForeign HoleCoords where
  read json = HoleCoords <$> readProp "x" json
                         <*> readProp "y" json
                         <*> readProp "z" json

data HoleCoordX = HCX Int String

instance isForeignHoleCoordX :: IsForeign HoleCoordX where
  read json = HCX <$> readProp "i" json
                  <*> readProp "ord" json

data HoleCoordY
  = HCYClosed Int String
  | HCYCustom CustomMemberId (Array RowKey)

instance isForeignHoleCoordY :: IsForeign HoleCoordY where
  read json = do
    typ <- readProp "type" json
    case typ of
      "closed" -> HCYClosed <$> readProp "i" json
                            <*> readProp "ord" json
      "custom" -> HCYCustom <$> readProp "customMemberId" json
                            <*> readProp "rowKeys" json

data HoleCoordZ
  = HCZSingleton
  | HCZClosed Int String
  | HCZCustom CustomMemberId String
  | HCZSubset SubsetMemberId String

instance isForeignHoleCoordZ :: IsForeign HoleCoordZ where
  read json = do
    typ <- readProp "type" json
    case typ of
      "singleton" -> pure HCZSingleton
      "closed"    -> HCZClosed <$> readProp "i" json
                               <*> readProp "ord" json
      "custom"    -> HCZCustom <$> readProp "customMemberId" json
                               <*> readProp "customMember" json
      "subset"    -> HCZSubset <$> readProp "subsetMemberId" json
                               <*> readProp "subsetMember" json

type ModuleParamValue = String

data Formula
  = FHole Hole
  | FSum (Array Hole)
  | FMember String
  | FUnary String Formula
  | FBinary String Formula Formula
  | FIfThenElse Formula Formula Formula
  | FBoolean Boolean
  | FNumber Number
  | FString String
  | FModuleParam String ModuleParamValue
  | FSet (Array Formula)

instance isForeignFormula :: IsForeign Formula where
  read json = do
    typ <- readProp "type" json
    case typ of
      "hole"        -> FHole        <$> readProp "hole" json
      "sum"         -> FSum         <$> readProp "holes" json
      "member"      -> FMember      <$> readProp "name" json
      "unary"       -> FUnary       <$> readProp "op" json    <*> readProp "f" json
      "binary"      -> FBinary      <$> readProp "op" json    <*> readProp "lhs" json  <*> readProp "rhs" json
      "ifThenElse"  -> FIfThenElse  <$> readProp "cond" json  <*> readProp "then" json <*> readProp "else" json
      "boolean"     -> FBoolean     <$> readProp "val" json
      "number"      -> FNumber      <$> readProp "val" json
      "string"      -> FString      <$> readProp "val" json
      "moduleParam" -> FModuleParam <$> readProp "name" json  <*> readProp "val" json
      "set"         -> FSet         <$> readProp "fs" json
