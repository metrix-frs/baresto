module Lib.Validation where

import           Prelude

import           Control.Bind (join)

import           Data.Tuple
import           Data.Maybe
import           Data.Foldable
import qualified Data.Map as M
import           Data.List (fromList)

import           Api.Schema.Validation

patchValidationResult :: ValidationResult -> ValidationResult -> ValidationResult
patchValidationResult (ValidationResult patch) (ValidationResult current) =
  ValidationResult
    { vrDpmFindings: foldl (\m (Tuple k v) -> M.insert k v m)
                           current.vrDpmFindings $
                           M.toList patch.vrDpmFindings
    , vrHeaderFindings: case patch.vrHeaderFindings of
        Nothing -> current.vrHeaderFindings
        Just h -> Just h
    }

flattenValidationResult :: ValidationResult -> Array Finding
flattenValidationResult (ValidationResult vr) =
    header <> join (fromList $ M.values vr.vrDpmFindings)
  where
    header = case vr.vrHeaderFindings of
      Just f -> f
      Nothing -> []
