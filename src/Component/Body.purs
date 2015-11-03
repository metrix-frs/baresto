module Component.Body where

import Prelude

import Data.Either
import Data.Functor.Coproduct (Coproduct())
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

data ViewerSlot = ViewerSlot

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
  | FileViewer

type State = CurrentView

initialState :: State
initialState = FileSelector

type StateP = InstalledState State ChildState Query ChildQuery Metrix ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)

data Query a
  = SetCurrentView CurrentView a

body :: Component StateP QueryP Metrix
body = parentComponent render eval
  where

    render :: RenderParent State ChildState Query ChildQuery Metrix ChildSlot
    render view = H.div_ $ case view of
      FileSelector ->
        [ H.button [ E.onClick (E.input_ $ SetCurrentView FileViewer) ]
          [ H.text "View" ]
        , H.slot' cpSelector SelectorSlot \_ -> { component: FS.selector, initialState: FS.initialState } ]
      FileViewer ->
        [ H.slot' cpViewer ViewerSlot \_ -> { component: FV.viewer, initialState: installedState FV.initialState } ]

    eval :: EvalParent Query State ChildState Query ChildQuery Metrix ChildSlot
    eval (SetCurrentView view next) = do
      modify $ const view
      pure next
