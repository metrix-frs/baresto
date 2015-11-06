module Component.ModuleBrowser where

import Prelude

import qualified Data.Map as M
import           Data.Maybe
import           Data.Foldable

import Control.Monad.State (execState)

import Optic.Core
import Optic.At
import Optic.Refractor.Prism
import Optic.Monad.Setter

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Types
import Utils

import Api
import Api.Schema.Module

type State =
  { mod :: Maybe Module
  , open :: Boolean
  , groupOpen :: M.Map TemplateGroupId Boolean
  , selectedTable :: TableId
  }

_groupOpen :: LensP State (M.Map TemplateGroupId Boolean)
_groupOpen = lens _.groupOpen _{ groupOpen = _ }

_mod :: LensP State (Maybe Module)
_mod = lens _.mod _{ mod = _ }

initialState :: State
initialState =
  { mod: Nothing
  , open: true
  , groupOpen: M.empty
  , selectedTable: 0
  }

data Query a
  = Boot ModuleId a
  | SelectTable TableId a
  | ToggleGroupOpen TemplateGroupId a
  | ToggleOpen a

moduleBrowser :: Component State Query Metrix
moduleBrowser = component render eval
  where

    render :: Render State Query
    render st = H.div
      [ cls "moduleBrowser"
      ]
      [ H.text "foo"
      ]

    eval :: Eval Query State Query Metrix
    eval (Boot modId next) = do
      apiCall (getModule modId) \mod -> do
        modify $ execState do
          _mod .= Just mod
          for_ (mod ^. _templateGroups) \g -> do
            _groupOpen .. at (g ^. _templateGroupId) .= Just true
      pure next
    eval (SelectTable tId next) = do
      modify _{ selectedTable = tId }
      pure next
    eval (ToggleGroupOpen gId next) = do
      modify $ _groupOpen .. at gId .. _Just %~ (not :: Boolean -> Boolean)
      pure next
    eval (ToggleOpen next) = do
      modify \st -> st { open = not st.open }
      pure next
