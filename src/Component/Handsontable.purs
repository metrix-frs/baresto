module Component.Handsontable where

import Prelude
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Api.Schema.Table (Table(Table), YAxis(YAxisCustom, YAxisClosed))
import Component.Handsontable.Options (tableOptions)
import Component.Handsontable.Utils (attachClickHandler, forceString, fromHotCoords, toHotCoords)
import Control.Monad.Aff.Free (fromEff)
import DOM.HTML.Types (HTMLElement)
import Data.Array (length)
import Data.Foldable (for_)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(Tuple))
import Halogen (ComponentDSL, ComponentHTML, Component, action, eventSource_, subscribe, eventSource, modify, get, lifecycleComponent)
import Handsontable (populateFromArray, handsontableNode, destroy, render) as Hot
import Handsontable.Hooks (onAfterRender, onAfterChange) as Hot
import Handsontable.Types (Handsontable, ChangeSource(ChangeSpliceRow, ChangeSpliceCol, ChangePaste, ChangeAutofill, ChangeLoadData, ChangePopulateFromArray, ChangeEdit, ChangeEmpty, ChangeAlter), Direction(DirectionDown), PopulateMethod(Overwrite)) as Hot
import Lib.BusinessData (BusinessData, getCustomYMembersBySheet, getFactTable)
import Lib.Table (C(..), Coord(..), R(..), S)
import Types (Metrix)
import Utils (getIndices, initClipboard, cls)

type State =
  { hotInstance :: Maybe (Hot.Handsontable String)
  , hotRoot :: Maybe HTMLElement
  }

initialState :: State
initialState =
  { hotInstance: Nothing
  , hotRoot: Nothing
  }

type Changes = Array (Tuple Coord String)

data Query a
  = Init a
  | SetRoot (Maybe HTMLElement) a
  | Edit Changes a
  | AddRow a
  | DeleteRow Int a
  | Rebuild S Table BusinessData a

handsontable :: S -> Table -> BusinessData -> Component State Query Metrix
handsontable propS propTable propBusinessData = lifecycleComponent
    { render
    , eval
    , initializer: Just (action Init)
    , finalizer: Nothing
    }
  where

    render :: State -> ComponentHTML Query
    render = const $ H.div
      [ cls "hotContainer"
      , P.ref \el -> action (SetRoot el)
      ] []

    eval :: Query ~> ComponentDSL State Query Metrix
    eval (Init next) = do
      build propS propTable propBusinessData
      pure next

    eval (SetRoot el next) = do
      modify _{ hotRoot = el }
      pure next

    eval (Edit changes next) = do
      pure next

    eval (AddRow next) = do
      pure next

    eval (DeleteRow index next) = do
      pure next

    eval (Rebuild s table bd next) = do
      build s table bd
      pure next

build :: S -> Table -> BusinessData -> ComponentDSL State Query Metrix Unit
build s table@(Table tbl) bd = do
  st <- get
  case st.hotRoot of
    Nothing -> pure unit
    Just el -> do
      case st.hotInstance of
        Nothing -> pure unit
        Just hot -> fromEff $ Hot.destroy hot

      hot <- fromEff $ Hot.handsontableNode el (tableOptions s table bd)
      modify _{ hotInstance = Just hot }

      subscribe $ eventSource (\cb -> Hot.onAfterChange hot (\c s' -> cb (Tuple c s'))) \(Tuple changes source) -> do
        let procChange change = let coord = fromHotCoords table change.col change.row
                                in  Tuple (Coord (C coord.col) (R coord.row) s) (forceString change.new)
            go = pure $ action $ Edit $ procChange <$> changes
            no = pure $ action $ Edit []
        case source of
          Hot.ChangeAlter             -> no
          Hot.ChangeEmpty             -> no
          Hot.ChangeEdit              -> go
          Hot.ChangePopulateFromArray -> no
          Hot.ChangeLoadData          -> no
          Hot.ChangeAutofill          -> go
          Hot.ChangePaste             -> go
          Hot.ChangeSpliceCol         -> no
          Hot.ChangeSpliceRow         -> no

      case tbl.tableYAxis of
        YAxisClosed _ _ -> pure unit
        YAxisCustom axId _ -> do
          fromEff $ Hot.onAfterRender hot \_ -> initClipboard ".clipboard"
          subscribe $ eventSource_ (attachClickHandler hot "#newCustomY") do
            pure $ action AddRow
          for_ (getIndices $ getCustomYMembersBySheet axId s table bd) \i ->
            subscribe $ eventSource_ (attachClickHandler hot ("#delCustomY" <> show i)) do
              pure $ action $ DeleteRow i

      case getFactTable s table bd of
        Just vals | length vals > 0 -> do
          fromEff $ Hot.populateFromArray (toHotCoords table 0 0) vals Nothing Nothing Hot.Overwrite Hot.DirectionDown [] hot
          pure unit
        _ -> fromEff $ Hot.render hot

  -- TODO: adjust resize

-- resize :: Eff _ Unit
-- resize = do
--   body <- DOM.document DOM.globalWindow >>= DOM.body
--   w <- DOM.innerWidth DOM.globalWindow
--   h <- DOM.innerHeight DOM.globalWindow
--   els <- DOM.nodeListToArray =<< DOM.querySelectorAll ".hotContainer" body
--   for_ els \el -> do
--     DOM.setStyleAttr "width" (show (w - 20.0) <> "px") el
--     DOM.setStyleAttr "height" (show (h - 300.0) <> "px") el
