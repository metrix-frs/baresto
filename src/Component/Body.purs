module Component.Body where

import Prelude

import Data.Maybe
import Data.Either
import Data.Functor.Coproduct (Coproduct(), left, coproduct)
import Data.Generic (Generic, gEq, gCompare)

import Halogen
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed as E

import Component.File as F
import Component.FileSelector as FS
import Component.FileViewer as FV

import Types
import Utils (cls, peek')
import Api
import Api.Schema.BusinessData

data SelectorSlot = SelectorSlot

derive instance genericSelectorSlot :: Generic SelectorSlot
instance eqSelectorSlot :: Eq SelectorSlot where eq = gEq
instance ordSelectorSlot :: Ord SelectorSlot where compare = gCompare

data ViewerSlot = ViewerSlot FileId

derive instance genericViewerSlot :: Generic ViewerSlot
instance eqViewerSlot :: Eq ViewerSlot where eq = gEq
instance ordViewerSlot :: Ord ViewerSlot where compare = gCompare

type ChildState = Either FS.StateP FV.StateP
type ChildQuery = Coproduct FS.QueryP FV.QueryP
type ChildSlot = Either SelectorSlot ViewerSlot

cpSelector :: ChildPath FS.StateP ChildState FS.QueryP ChildQuery SelectorSlot ChildSlot
cpSelector = cpL

cpViewer :: ChildPath FV.StateP ChildState FV.QueryP ChildQuery ViewerSlot ChildSlot
cpViewer = cpR

--

data CurrentView
  = FileSelector
  | FileViewer UpdateId

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
    render st = H.div [ cls "body" ] $ case st.currentView of
      FileSelector ->
        [ H.slot' cpSelector SelectorSlot \_ ->
          { component: FS.selector, initialState: installedState FS.initialState }
        ]
      FileViewer updateId ->
        [ H.slot' cpViewer (ViewerSlot updateId) \_ ->
          { component: FV.viewer updateId, initialState: installedState FV.initialState }
        ]

    eval :: EvalParent Query State ChildState Query ChildQuery Metrix ChildSlot
    eval (Foo next) = do
      pure next

    peek :: Peek (ChildF ChildSlot ChildQuery) State ChildState Query ChildQuery Metrix ChildSlot
    peek child = do
        peek' cpSelector child \s q -> coproduct peekSelector peekFile q
        peek' cpViewer   child \s q -> coproduct peekViewer (const $ pure unit) q
      where
        peekSelector q = case q of
          FS.CreateFile modId name _ ->
            apiCallParent (newFile modId name) \(UpdateGet u) ->
              modify _{ currentView = FileViewer u.updateGetId }
          FS.UploadXbrlOpenFile updateId _ ->
            modify _{ currentView = FileViewer updateId }
          _ -> pure unit
        peekFile (ChildF _ q) = case q of
          F.Open updateId _ ->
            modify _{ currentView = FileViewer updateId }
          _ -> pure unit
        peekViewer q = case q of
          FV.CloseFile _ ->
            modify _{ currentView = FileSelector }
          _ -> pure unit
