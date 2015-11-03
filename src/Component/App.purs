module Component.App where

import Prelude

import Control.Plus (Plus)

import Data.Array
import Data.Either
import Data.Tuple
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)

import           Halogen
import           Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import qualified Component.Spinner as Spinner
import qualified Component.Auth as Auth
import qualified Component.ErrorBox as ErrorBox
import qualified Component.Body as Body

import Types

--

data AuthSlot = AuthSlot

derive instance genericAuthSlot :: Generic AuthSlot
instance eqAuthSlot :: Eq AuthSlot where eq = gEq
instance ordAuthSlot :: Ord AuthSlot where compare = gCompare

data SpinnerSlot = SpinnerSlot

derive instance genericSpinnerSlot :: Generic SpinnerSlot
instance eqSpinnerSlot :: Eq SpinnerSlot where eq = gEq
instance ordSpinnerSlot :: Ord SpinnerSlot where compare = gCompare

data ErrorBoxSlot = ErrorBoxSlot

derive instance genericErrorBoxSlot :: Generic ErrorBoxSlot
instance eqErrorBoxSlot :: Eq ErrorBoxSlot where eq = gEq
instance ordErrorBoxSlot :: Ord ErrorBoxSlot where compare = gCompare

data BodySlot = BodySlot

derive instance genericBodySlot :: Generic BodySlot
instance eqBodySlot :: Eq BodySlot where eq = gEq
instance ordBodySlot :: Ord BodySlot where compare = gCompare

type ChildState = Either Auth.State (Either Spinner.State (Either ErrorBox.State Body.StateP))
type ChildQuery = Coproduct Auth.Query (Coproduct Spinner.Query (Coproduct ErrorBox.Query Body.QueryP))
type ChildSlot = Either AuthSlot (Either SpinnerSlot (Either ErrorBoxSlot BodySlot))

cpAuth :: ChildPath Auth.State ChildState Auth.Query ChildQuery AuthSlot ChildSlot
cpAuth = cpL

cpSpinner :: ChildPath Spinner.State ChildState Spinner.Query ChildQuery SpinnerSlot ChildSlot
cpSpinner = cpR :> cpL

cpErrorBox :: ChildPath ErrorBox.State ChildState ErrorBox.Query ChildQuery ErrorBoxSlot ChildSlot
cpErrorBox = cpR :> cpR :> cpL

cpBody :: ChildPath Body.StateP ChildState Body.QueryP ChildQuery BodySlot ChildSlot
cpBody = cpR :> cpR :> cpR

--

data Query a
  = Foo a

data State = State

initialState :: State
initialState = State

type StateP = InstalledState State ChildState Query ChildQuery Metrix ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)

app :: Component StateP QueryP Metrix
app = parentComponent render eval
  where
    render :: RenderParent State ChildState Query ChildQuery Metrix ChildSlot
    render _ = H.div_
      [ H.slot' cpSpinner SpinnerSlot \_ -> { component: Spinner.spinner, initialState: Spinner.initialState }
      , H.slot' cpAuth AuthSlot \_ -> { component: Auth.auth, initialState: Auth.initialState }
      , H.slot' cpErrorBox ErrorBoxSlot \_ -> { component: ErrorBox.errorBox, initialState: ErrorBox.initialState }
      , H.slot' cpBody BodySlot \_ -> { component: Body.body, initialState: installedState Body.initialState }
      ]

    eval :: EvalParent Query State ChildState Query ChildQuery Metrix ChildSlot
    eval (Foo next) = do
      pure next
