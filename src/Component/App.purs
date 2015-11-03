module Component.App where

import Prelude

import Control.Plus (Plus)

import Data.Array
import Data.Either
import Data.Maybe
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)

import           Halogen
import           Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import qualified Component.Spinner as Spinner
import qualified Component.ErrorBox as ErrorBox
import qualified Component.Body as Body

import Api
import Api.Schema.Auth

import Utils
import Types

--

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

type ChildState = Either Spinner.State (Either ErrorBox.State Body.StateP)
type ChildQuery = Coproduct Spinner.Query (Coproduct ErrorBox.Query Body.QueryP)
type ChildSlot = Either SpinnerSlot (Either ErrorBoxSlot BodySlot)

cpSpinner :: ChildPath Spinner.State ChildState Spinner.Query ChildQuery SpinnerSlot ChildSlot
cpSpinner = cpL

cpErrorBox :: ChildPath ErrorBox.State ChildState ErrorBox.Query ChildQuery ErrorBoxSlot ChildSlot
cpErrorBox = cpR :> cpL

cpBody :: ChildPath Body.StateP ChildState Body.QueryP ChildQuery BodySlot ChildSlot
cpBody = cpR :> cpR

--

data AuthStatus
  = CheckingLicense
  | Authenticated String
  | LoggedOut

type State =
  { authStatus :: AuthStatus
  , authError :: Maybe String
  , customerId :: String
  , licenseKey :: String
  }

initialState :: State
initialState =
  { authStatus: CheckingLicense
  , authError: Nothing
  , customerId: ""
  , licenseKey: ""
  }

data Query a
  = Init a
  | Authenticate a
  | SetCustomerId String a
  | SetLicenseKey String a
  | LogOut a

type StateP = InstalledState State ChildState Query ChildQuery Metrix ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)

app :: Component StateP QueryP Metrix
app = parentComponent render eval
  where
    render :: RenderParent State ChildState Query ChildQuery Metrix ChildSlot
    render st = H.div_
      [ H.slot' cpErrorBox ErrorBoxSlot \_ -> { component: ErrorBox.errorBox, initialState: ErrorBox.initialState }
      -- about box?
      , H.div [ cls "status" ]
        [ H.slot' cpSpinner SpinnerSlot \_ -> { component: Spinner.spinner, initialState: Spinner.initialState }
        , H.div [ cls "login" ] $ case st.authStatus of
            Authenticated cId ->
              [ H.text $ "Logged in with customer id " <> cId <> ". "
              , H.button
                [ E.onClick (E.input_ $ LogOut) ]
                [ H.text "Logout" ]
              ]
            _ ->
              []
          -- about button together with logout in menu?
        ]
      , case st.authStatus of
          Authenticated _ ->
            H.slot' cpBody BodySlot \_ -> { component: Body.body, initialState: installedState Body.initialState }
          LoggedOut ->
            renderAuthForm st.customerId st.licenseKey
          CheckingLicense -> H.div_
            [ H.text "Checking license..." ]
      ]

    eval :: EvalParent Query State ChildState Query ChildQuery Metrix ChildSlot
    eval (Init next) = do
      apiCall loginStatus \(LoginStatus st) -> case st of
        Just customerId ->
          modify _{ authStatus = Authenticated customerId }
        Nothing ->
          modify _{ authStatus = LoggedOut }
      pure next
    eval (Authenticate next) = do
      st <- get
      apiCall (login st.customerId st.licenseKey) \(LoginResponse res) ->
        if res.lrSuccess
          then modify _{ authStatus = Authenticated st.customerId
                       , authError = Nothing }
          else modify _{ authError = Just res.lrMessage }
      pure next
    eval (LogOut next) = do
      apiCall logout \_ ->
        modify _{ authStatus = LoggedOut
                , customerId = ""
                , licenseKey = "" }
      pure next

renderAuthForm :: String -> String -> ParentHTML ChildState Query ChildQuery Metrix ChildSlot
renderAuthForm customerId licenseKey = H.div_
  [ H.table_
    [ H.tr_
      [ H.td_
        [ H.text "ID:"
        ]
      , H.td_
        [ H.input
          [ E.onValueChange $ E.input SetCustomerId
          , P.value customerId
          ]
        ]
      ]
    , H.tr_
      [ H.td_
        [ H.text "License key:"
        ]
      , H.td_
        [ H.input
          [ E.onValueChange $ E.input SetLicenseKey
          , P.value licenseKey
          ]
        ]
      ]
    ]
  , H.button
    [ E.onClick (E.input_ Authenticate) ]
    [ H.text "Authenticate" ]
    ]
