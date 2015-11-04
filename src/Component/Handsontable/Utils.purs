module Component.Handsontable.Utils
  ( toHotCoords
  , fromHotCoords
  , attachClickHandler
  ) where

import Prelude

import Control.Monad.Eff

import Data.Array (length)

import Api.Schema.Table

toHotCoords :: Table -> Int -> Int -> {col :: Int, row :: Int}
toHotCoords table c r = { col: c + 2, row: r + (headerHeight table) }

fromHotCoords :: Table -> Int -> Int -> {col :: Int, row :: Int}
fromHotCoords table c r = { col: c - 2, row: r - (headerHeight table) }

headerHeight :: Table -> Int
headerHeight (Table tbl) = length tbl.tableXHeader + case tbl.tableYAxis of
  YAxisCustom _ _ -> 2
  YAxisClosed _ _ -> 1

foreign import attachClickHandler :: forall eff a. String -> Eff eff a -> Eff eff a
