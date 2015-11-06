module Component.Body where

import Prelude

import Data.Maybe
import Data.Either
import Data.Functor.Coproduct (Coproduct(), left)
import Data.Generic (Generic, gEq, gCompare)

import           Halogen
import           Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import qualified Component.FileSelector as FS
import qualified Component.FileViewer as FV

import Types

data SelectorSlot = SelectorSlot

derive instance genericSelectorSlot :: Generic SelectorSlot
instance eqSelectorSlot :: Eq SelectorSlot where eq = gEq
instance ordSelectorSlot :: Ord SelectorSlot where compare = gCompare

data ViewerSlot = ViewerSlot ModuleId FileId

derive instance genericViewerSlot :: Generic ViewerSlot
instance eqViewerSlot :: Eq ViewerSlot where eq = gEq
instance ordViewerSlot :: Ord ViewerSlot where compare = gCompare

type ChildState = Either FS.State FV.StateP
type ChildQuery = Coproduct FS.Query FV.QueryP
type ChildSlot = Either SelectorSlot ViewerSlot

cpSelector :: ChildPath FS.State ChildState FS.Query ChildQuery SelectorSlot ChildSlot
cpSelector = cpL

cpViewer :: ChildPath FV.StateP ChildState FV.QueryP ChildQuery ViewerSlot ChildSlot
cpViewer = cpR

--

data CurrentView
  = FileSelector
  | FileViewer ModuleId FileId

type State =
  { currentView :: CurrentView
  , msg :: String
  }

initialState :: State
initialState =
  { currentView: FileSelector
  , msg: "ss"
  }

type StateP = InstalledState State ChildState Query ChildQuery Metrix ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)

data Query a
  = Foo a

body :: Component StateP QueryP Metrix
body = parentComponent' render eval peek
  where

    render :: RenderParent State ChildState Query ChildQuery Metrix ChildSlot
    render st = H.div_ $ case st.currentView of
      FileSelector ->
        [ H.slot' cpSelector SelectorSlot \_ ->
          { component: FS.selector, initialState: FS.initialState }
        ]
      FileViewer modId fileId ->
        [ H.slot' cpViewer (ViewerSlot modId fileId) \_ ->
          { component: FV.viewer modId fileId, initialState: installedState FV.initialState }
        ]

    eval :: EvalParent Query State ChildState Query ChildQuery Metrix ChildSlot
    eval (Foo next) = do
      pure next

    peek :: Peek (ChildF ChildSlot ChildQuery) State ChildState Query ChildQuery Metrix ChildSlot
    peek child = do
      FV.peek' cpSelector child \s q -> case q of
        FS.SelectFile modId fileId _ ->
          modify _{ currentView = FileViewer modId fileId }
