module Api.Schema.Finding where

import Prelude

import           Data.Maybe
import           Data.Tuple
import           Data.Foreign
import           Data.Foreign.Class
import           Data.Foreign.NullOrUndefined

import Types

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

data HoleCoords
  = HCPlain   PlainOrd PlainOrd   PlainOrd
  | HCSubsetZ PlainOrd PlainOrd   SubsetZOrd
  | HCCustomY PlainOrd CustomYOrd
  | HCCustomZ PlainOrd PlainOrd   CustomZOrd

instance isForeignHoleCoords :: IsForeign HoleCoords where
  read json = do
    x <- Tuple <$> readProp "x" json <*> readProp "xOrd" json
    typ <- readProp "type" json
    case typ of
      "plain" -> do
        y <- Tuple <$> readProp "y" json <*> readProp "yOrd" json
        z <- Tuple <$> readProp "x" json <*> readProp "zOrd" json
        pure $ HCPlain x y z
      "subsetZ" -> do
        y <- Tuple <$> readProp "y" json <*> readProp "yOrd" json
        z <- readProp "zSubsetMember" json
        pure $ HCSubsetZ x y z
      "customY" -> do
        y <- Tuple <$> readProp "yCustomMemberId" json <*> readProp "yCustomMemberCodes" json
        pure $ HCCustomY x y
      "customZ" -> do
        y <- Tuple <$> readProp "y" json <*> readProp "yOrd" json
        z <- Tuple <$> readProp "zCustomMemberId" json <*> readProp "zCustomMember" json
        pure $ HCCustomZ x y z

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
      "hole" -> FHole <$> readProp "hole" json
      "sum" -> FSum <$> readProp "holes" json
      "member" -> FMember <$> readProp "name" json
      "unary" -> FUnary <$> readProp "op" json <*> readProp "f" json
      "binary" -> FBinary <$> readProp "op" json <*> readProp "lhs" json <*> readProp "rhs" json
      "ifThenElse" -> FIfThenElse <$> readProp "cond" json <*> readProp "then" json <*> readProp "else" json
      "boolean" -> FBoolean <$> readProp "val" json
      "number" -> FNumber <$> readProp "val" json
      "string" -> FString <$> readProp "val" json
      "moduleParam" -> FModuleParam <$> readProp "name" json <*> readProp "val" json
      "set" -> FSet <$> readProp "fs" json
