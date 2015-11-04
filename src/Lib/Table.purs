module Lib.Table where

import Prelude

import           Data.Either (Either (Left))
import           Data.Maybe
import           Data.Tuple
import           Data.Array hiding ((..))
import           Data.Foldable

import Control.Bind

import Optic.Core

import Utils
import Types
import Api.Schema.Table


mapGrid :: forall a. (Int -> Int -> Int -> Cell -> a) -> Grid -> Array (Array (Array a))
mapGrid f (Grid sheets) = (\(Tuple s sheet) -> mapSheet (f s) sheet) <$> makeIndexed sheets

mapSheet :: forall a. (Int -> Int -> Cell -> a) -> Sheet -> Array (Array a)
mapSheet f (Sheet rows) = (\(Tuple r row) -> mapRow (f r) row) <$> makeIndexed rows

mapRow :: forall a. (Int -> Cell -> a) -> Row -> Array a
mapRow f (Row cells) = (\(Tuple c cell) -> f c cell) <$> makeIndexed cells

boolValueMap :: Array (Tuple String String)
boolValueMap =
  [ Tuple "true" "True"
  , Tuple "false" "False"
  ]

data Coord = Coord C R S

newtype R = R Int
newtype C = C Int
newtype S = S Int

_col :: LensP C Int
_col = lens (\(C c) -> c) (\_ c -> C c)

_row :: LensP R Int
_row = lens (\(R r) -> r) (\_ r -> R r)

instance eqS :: Eq S where
  eq (S a) (S b) = a == b

instance eqR :: Eq R where
  eq (R a) (R b) = a == b

instance eqC :: Eq C where
  eq (C a) (C b) = a == b

instance ordR :: Ord R where
  compare (R a) (R b) = compare a b

instance ordC :: Ord C where
  compare (C a) (C b) = compare a b

instance showS :: Show S where
  show (S s) = show s

cellLookup :: Coord -> Table -> Maybe Cell
cellLookup (Coord (C c) (R r) (S s)) (Table tbl) = do
  Grid sheets <- pure tbl.tableGrid
  Sheet rows <- case tbl.tableZAxis of
    ZAxisSingleton    -> sheets !! s
    ZAxisClosed _ _   -> sheets !! s
    ZAxisCustom _ _   -> sheets !! 0
    ZAxisSubset _ _ _ -> sheets !! 0
  Row cells <- case tbl.tableYAxis of
    YAxisClosed _ _ -> rows !! r
    YAxisCustom _ _ -> rows !! 0
  cells !! c
