module Lib.Validation where

import Prelude (($), (<>))
import Control.Bind (join)

import Data.Maybe (Maybe(Nothing, Just))
import Data.StrMap as SM
import Data.List (fromList)

import Api.Schema.Validation (Finding, ValidationResult(ValidationResult))

foreign import fastPatch :: forall a. SM.StrMap (Array a) -> SM.StrMap (Array a) -> SM.StrMap (Array a)

patchValidationResult :: ValidationResult -> ValidationResult -> ValidationResult
patchValidationResult (ValidationResult patch) (ValidationResult current) =
  ValidationResult
    { vrDpmFindings: fastPatch patch.vrDpmFindings current.vrDpmFindings
    , vrHeaderFindings: case patch.vrHeaderFindings of
        Nothing -> current.vrHeaderFindings
        Just h -> Just h
    }

flattenValidationResult :: ValidationResult -> Array Finding
flattenValidationResult (ValidationResult vr) =
    header <> join (fromList $ SM.values vr.vrDpmFindings)
  where
    header = case vr.vrHeaderFindings of
      Just f -> f
      Nothing -> []
