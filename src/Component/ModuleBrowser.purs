module Component.ModuleBrowser where

import Prelude

import qualified Data.Map as M
import           Data.Maybe

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

import Api.Schema.Module

type State =
  { mod :: Maybe Module
  , open :: Boolean
  , groupOpen :: M.Map TemplateGroupId Boolean
  , selectedTemplate :: TemplateId
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
  , selectedTemplate: 0
  }

data Query a
  = Init a
  | SelectTemplate TemplateId a
  | ToggleGroupOpen TemplateGroupId a
  | ToggleOpen a

moduleBrowser :: forall eff. Component State Query (Metrix eff)
moduleBrowser = component render eval
  where

    render :: Render State Query
    render st = H.div
      [ cls "moduleBrowser"
      , P.initializer \_ -> action Init
      ]
      [ H.text "foo"
      ]

    eval :: Eval Query State Query (Metrix eff)
    eval (Init next) = do
      mod <- ajax getModule
      modify $ execState do
        _mod .= Just mod
        for_ (mod ^. _templateGroups) \g -> do
          _groupOpen .. at (g ^. _templateGroupId) .= Just true
      pure next
    eval (SelectTemplate tId next) = do
      modify _{ selectedTemplate = tId }
      pure next
    eval (ToggleGroupOpen gId next) = do
      modify $ _groupOpen .. at gId .. _Just %~ (not :: Boolean -> Boolean)
      pure next
    eval (ToggleOpen next) = do
      modify \st -> st { open = not st.open }
      pure next
