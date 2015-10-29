module Component.App where

import Prelude

import Control.Plus (Plus)

import Data.Array
import Data.Either
import Data.Tuple
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)

import           Halogen
import           Halogen.Component.ChildPath (ChildPath(), cpL, cpR)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import qualified Component.Spinner as Spinner
import qualified Component.Auth as Auth

import Types

--

data SpinnerSlot = SpinnerSlot

derive instance genericSpinnerSlot :: Generic SpinnerSlot
instance eqSpinnerSlot :: Eq SpinnerSlot where eq = gEq
instance ordSpinnerSlot :: Ord SpinnerSlot where compare = gCompare

data AuthSlot = AuthSlot String

derive instance genericAuthSlot :: Generic AuthSlot
instance eqAuthSlot :: Eq AuthSlot where eq = gEq
instance ordAuthSlot :: Ord AuthSlot where compare = gCompare

type ChildState = Either Auth.State Spinner.State
type ChildQuery = Coproduct Auth.Query Spinner.Query
type ChildSlot = Either AuthSlot SpinnerSlot

cpAuth :: ChildPath Auth.State ChildState Auth.Query ChildQuery AuthSlot ChildSlot
cpAuth = cpL

cpSpinner :: ChildPath Spinner.State ChildState Spinner.Query ChildQuery SpinnerSlot ChildSlot
cpSpinner = cpR

--

data Query a
  = Foo a

type State = Unit

initialState :: State
initialState = unit

type StateP g = InstalledState State ChildState Query ChildQuery g ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)

app :: forall eff. Component (StateP (Metrix eff)) QueryP (Metrix eff)
app = parentComponent render eval
  where
    render :: RenderParent State ChildState Query ChildQuery (Metrix eff) ChildSlot
    render st = H.div_
        [ H.slot' cpSpinner SpinnerSlot \_ -> { component: Spinner.spinner, initialState: Spinner.initialState }
        , H.slot' cpAuth (AuthSlot "A") \_ -> { component: Auth.auth, initialState: Auth.initialState }
        , H.slot' cpAuth (AuthSlot "B") \_ -> { component: Auth.auth, initialState: Auth.initialState }
        ]

    eval :: EvalParent Query State ChildState Query ChildQuery (Metrix eff) ChildSlot
    eval (Foo next) = do
      pure next
