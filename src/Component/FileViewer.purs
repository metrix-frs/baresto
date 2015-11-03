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

import Types

data ModuleBrowserSlot = ModuleBrowserSlot

derive instance genericModuleBrowserSlot :: Generic ModuleBrowserSlot
instance eqModuleBrowserSlot :: Eq ModuleBrowserSlot where eq = gEq
instance ordModuleBrowserSlot :: Ord ModuleBrowserSlot where compare = gCompare

--

data State = State

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
