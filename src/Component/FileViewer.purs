module Component.FileViewer where

import Prelude

import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)

import           Halogen
import           Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import qualified Component.ModuleBrowser as MB
import qualified Component.Handsontable as H

import Api.Schema.Table
import Lib.BusinessData

import Types

data ModuleBrowserSlot = ModuleBrowserSlot

derive instance genericModuleBrowserSlot :: Generic ModuleBrowserSlot
instance eqModuleBrowserSlot :: Eq ModuleBrowserSlot where eq = gEq
instance ordModuleBrowserSlot :: Ord ModuleBrowserSlot where compare = gCompare

data HotSlot = HotSlot

derive instance genericHotSlot :: Generic HotSlot
instance eqHotSlot :: Eq HotSlot where eq = gEq
instance ordHotSlot :: Ord HotSlot where compare = gCompare

type ChildState = Either MB.State H.State
type ChildQuery = Coproduct MB.Query H.Query
type ChildSlot = Either ModuleBrowserSlot HotSlot

cpModuleBrowser :: ChildPath MB.State ChildState MB.Query ChildQuery ModuleBrowserSlot ChildSlot
cpModuleBrowser = cpL

cpHot :: ChildPath H.State ChildState H.Query ChildQuery HotSlot ChildSlot
cpHot = cpR

--

type State =
  { businessData :: BusinessData
  , selectedSheet :: S

  }

initialState :: State
initialState = State

data Query a
  = Foo a

type StateP = InstalledState State MB.State Query MB.Query Metrix ModuleBrowserSlot
type QueryP = Coproduct Query (ChildF ModuleBrowserSlot MB.Query)

viewer :: Component StateP QueryP Metrix
viewer = parentComponent render eval
  where

    render :: RenderParent State MB.State Query MB.Query Metrix ModuleBrowserSlot
    render _ = H.div_
      [ H.slot ModuleBrowserSlot \_ -> { component: MB.moduleBrowser 0, initialState: MB.initialState }
      ]

    eval :: EvalParent Query State MB.State Query MB.Query Metrix ModuleBrowserSlot
    eval (Foo next) = do
      pure next
