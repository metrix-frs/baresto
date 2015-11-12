module Component.Handsontable where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Control.Bind

import           Data.Array hiding ((..))
import           Data.Foldable
import           Data.Tuple
import           Data.Maybe
import qualified Data.Map as M

import DOM.HTML.Types (HTMLElement())

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Optic.Core
import Optic.At

import qualified Handsontable       as Hot
import qualified Handsontable.Types as Hot
import qualified Handsontable.Hooks as Hot

import Utils (getEntropy, getIndices, cls)

import Types
import Api.Schema.Table
import Lib.Table
import Lib.BusinessData

import Component.Handsontable.Options
import Component.Handsontable.Utils

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
  = Init HTMLElement a
  | Edit Changes a
  | AddRow a
  | DeleteRow Int a
  | Rebuild S Table BusinessData a

handsontable :: S -> Table -> BusinessData -> Component State Query Metrix
handsontable propS propTable propBusinessData = component render eval
  where

    render :: Render State Query
    render = const $ H.div
      [ cls "hotContainer"
      , P.initializer \el -> action (Init el)
      ] []

    eval :: Eval Query State Query Metrix
    eval (Init el next) = do
      modify _{ hotRoot = Just el }
      build propS propTable propBusinessData
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
        Just hot -> liftEff' $ Hot.destroy hot

      hot <- liftEff' $ Hot.handsontableNode el (tableOptions s table bd)
      modify _{ hotInstance = Just hot }

      case getFactTable s table bd of
        Just vals | length vals > 0 -> do
          liftEff' $ Hot.populateFromArray (toHotCoords table 0 0) vals Nothing Nothing Hot.Overwrite Hot.DirectionDown [] hot
          pure unit
        _ -> pure unit

      subscribe $ eventSource (\cb -> Hot.onAfterChange hot (\c s -> cb (Tuple c s))) \(Tuple changes source) -> do
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
          subscribe $ eventSource_ (attachClickHandler "#newCustomY") do
            pure $ action AddRow
          for_ (getIndices $ getCustomMembers axId bd) \i ->
            subscribe $ eventSource_ (attachClickHandler ("#delCustomY" <> show i)) do
              pure $ action $ DeleteRow i

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
