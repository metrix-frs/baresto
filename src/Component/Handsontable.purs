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

import Utils (getEntropy, makeIndexed, cls)

import Types
import Api.Schema.Template
import Lib.Template
import Lib.BusinessData

import Component.Handsontable.Options
import Component.Handsontable.Utils

type State =
  { hotInstance :: Maybe (Hot.Handsontable String)
  }

initialState :: State
initialState =
  { hotInstance: Nothing
  }

type Changes = Array (Tuple Coord String)

data Query a
  = Init HTMLElement a
  | Edit Changes a
  | AddRow String a
  | DeleteRow Int String a
  | Rebuild S Table BusinessData a

table :: forall eff. S -> Table -> BusinessData -> Component State Query (Metrix eff)
table initialS initialTable initialBD = component render eval
  where

    render :: Render State Query
    render = const $ H.div
      [ cls "hotcontainer"
      , P.initializer \el -> action (Init el)
      ] []

    eval :: Eval Query State Query (Metrix eff)
    eval (Init el next) = do
      hot <- liftEff' $ Hot.handsontableNode el { data: [] }
      modify _{ hotInstance = Just hot }
      build initialS initialTable initialBD hot
      pure next
    eval (Edit changes next) = do
      pure next
    eval (AddRow name next) = do
      pure next
    eval (DeleteRow index name next) = do
      pure next
    eval (Rebuild s table bd next) = do
      mHot <- gets _.hotInstance
      case mHot of
        Nothing -> pure unit
        Just hot -> build s table bd hot
      pure next

build :: forall eff. S -> Table -> BusinessData -> Hot.Handsontable String -> ComponentDSL State Query (Metrix eff) Unit
build s table@(Table tbl) bd hot = do
  case getFactTable s table bd of
    Just vals | length vals > 0 -> do
      liftEff' $ Hot.populateFromArray (toHotCoords table 0 0) vals Nothing Nothing Hot.Overwrite Hot.DirectionDown [] hot
      pure unit
    _ -> pure unit

  subscribe $ eventSource (\cb -> Hot.onAfterChange hot (\c s -> cb (Tuple c s))) \(Tuple changes source) -> do
    let procChange change = let coord = fromHotCoords table change.col change.row
                            in  Tuple (Coord (C coord.col) (R coord.row) s) change.new
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
        i <- getEntropy 32
        pure $ action $ AddRow i
      for_ (makeIndexed $ getCustomMembers axId bd) \(Tuple i (Tuple memId _)) ->
        subscribe $ eventSource_ (attachClickHandler ("#delCustomY" <> memId)) do
          pure $ action $ DeleteRow i memId

  -- TODO: try to subscribe to button clicks after rendering of handsontable object
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
