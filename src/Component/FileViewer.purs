module Component.FileViewer where

import Prelude

import Data.Either
import Data.Maybe
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)

import           Halogen
import           Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import qualified Component.ModuleBrowser as MB
import qualified Component.Handsontable as Hot

import Optic.Core

import Api.Schema.Table
import Lib.Table
import Lib.BusinessData

import Types
import Utils

data ModuleBrowserSlot = ModuleBrowserSlot

derive instance genericModuleBrowserSlot :: Generic ModuleBrowserSlot
instance eqModuleBrowserSlot :: Eq ModuleBrowserSlot where eq = gEq
instance ordModuleBrowserSlot :: Ord ModuleBrowserSlot where compare = gCompare

data HotSlot = HotSlot

derive instance genericHotSlot :: Generic HotSlot
instance eqHotSlot :: Eq HotSlot where eq = gEq
instance ordHotSlot :: Ord HotSlot where compare = gCompare

type ChildState = Either MB.State Hot.State
type ChildQuery = Coproduct MB.Query Hot.Query
type ChildSlot = Either ModuleBrowserSlot HotSlot

cpModuleBrowser :: ChildPath MB.State ChildState MB.Query ChildQuery ModuleBrowserSlot ChildSlot
cpModuleBrowser = cpL

cpHot :: ChildPath Hot.State ChildState Hot.Query ChildQuery HotSlot ChildSlot
cpHot = cpR

--

type State =
  { businessData :: BusinessData
  , selectedSheet :: S
  , table :: Maybe Table
  }

initialState :: State
initialState =
  { businessData: emptyBusinessData
  , selectedSheet: S 0
  , table: Nothing
  }

data Query a
  = SelectSheet S a

type StateP = InstalledState State ChildState Query ChildQuery Metrix ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)

viewer :: Component StateP QueryP Metrix
viewer = parentComponent render eval
  where

    render :: RenderParent State ChildState Query ChildQuery Metrix ChildSlot
    render st = H.div [ cls "viewer" ]
      [ H.div [ cls "vieverBar" ]
        [ H.slot' cpModuleBrowser ModuleBrowserSlot \_ ->
          { component: MB.moduleBrowser 0, initialState: MB.initialState }
        , H.div [ cls "tableTitle" ]
          [ H.h1_
            [ H.text "Title"]
          , H.p_
            [ H.text "subtitle" ]
          ]
        , H.div [ cls "sheetSelector" ]
          [ viewSheetSelector st
          ]
        , H.div [ cls "fileActions" ]
          [ H.text "File Actions"
          ]
        ]
      , H.div [ cls "viewerContent" ]
        [ H.slot' cpHot HotSlot \_ ->
          { component: Hot.handsontable, initialState: Hot.initialState }
        ]
      ]

    eval :: EvalParent Query State ChildState Query ChildQuery Metrix ChildSlot
    eval (SelectSheet s next) = do
      modify _{ selectedSheet = s }
      pure next

viewSheetSelector :: RenderParent State ChildState Query ChildQuery Metrix ChildSlot
viewSheetSelector st = H.div_ []
