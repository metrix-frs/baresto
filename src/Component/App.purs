module Component.App where

import Prelude

import Control.Plus (Plus)

import Control.Monad.Eff.Console (log)

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
import           Component.Common (modal)

import Api
import Api.Schema.Auth

import Utils
import Types

import Version (versionStr)

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
  , aboutOpen :: Boolean
  }

initialState :: State
initialState =
  { authStatus: CheckingLicense
  , authError: Nothing
  , customerId: ""
  , licenseKey: ""
  , aboutOpen: false
  }

data Query a
  = Boot a
  | Authenticate a
  | SetCustomerId String a
  | SetLicenseKey String a
  | AboutOpen a
  | AboutClose a
  | LogOut a

type StateP = InstalledState State ChildState Query ChildQuery Metrix ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)

app :: Component StateP QueryP Metrix
app = parentComponent render eval
  where
    render :: RenderParent State ChildState Query ChildQuery Metrix ChildSlot
    render st = H.div [ cls "app" ] $
      [ H.slot' cpErrorBox ErrorBoxSlot \_ ->
        { component: ErrorBox.errorBox, initialState: ErrorBox.initialState }
      , H.div [ cls "status" ] $
        [ H.slot' cpSpinner SpinnerSlot \_ ->
          { component: Spinner.spinner, initialState: Spinner.initialState }
        , H.div [ cls "menu" ] $
          ( case st.authStatus of
              Authenticated cId ->
                [ H.text $ "Using license for customer: " <> cId
                , H.button
                  [ E.onClick (E.input_ $ LogOut) ]
                  [ H.span [ cls "octicon octicon-sign-out" ] []
                  , H.text "Logout" ]
                ]
              _ ->
                []
          ) <>
          [ H.button
            [ E.onClick $ E.input_ AboutOpen ]
            [ H.text "About" ]
          ]
        ]
      , case st.authStatus of
          Authenticated _ ->
            H.slot' cpBody BodySlot \_ ->
            { component: Body.body, initialState: installedState Body.initialState }
          LoggedOut ->
            renderAuthForm st.customerId st.licenseKey st.authError
          CheckingLicense -> H.div_ []
      ] <> (
        if st.aboutOpen
          then
            [ modal "About"
              [ H.p_ [ H.b_ [ H.text $ "Metrix Suite " <> versionStr ] ]
              , H.p_ [ H.text "This is an early alpha release, bugs may occur." ]
              , H.p_ [ H.text "For feedback, contact us at "
                     , H.a [ P.href "mailto:info@metrix-frs.de" ]
                           [ H.text "info@metrix-frs.de" ]
                     , H.text " or visit "
                     , H.a [ P.href "http://www.metrix-frs.de" ]
                           [ H.text "metrix-frs.de" ]
                     , H.text "."
                     ]
              ]
              [ H.button
                [ E.onClick $ E.input_ AboutClose ]
                [ H.text "Close" ]
              ]
            ]
          else
            []
      )

    eval :: EvalParent Query State ChildState Query ChildQuery Metrix ChildSlot
    eval (Boot next) = do
      apiCallParent loginStatus \(LoginStatus st) -> case st of
        Just customerId ->
          modify _{ authStatus = Authenticated customerId }
        Nothing ->
          modify _{ authStatus = LoggedOut }
      pure next

    eval (Authenticate next) = do
      st <- get
      apiCallParent (login st.customerId st.licenseKey) \(LoginResponse res) ->
        if res.lrSuccess
          then modify _{ authStatus = Authenticated st.customerId
                       , authError = Nothing }
          else modify _{ authError = Just res.lrMessage }
      pure next

    eval (SetCustomerId customerId next) = do
      modify _{ customerId = customerId}
      pure next

    eval (SetLicenseKey key next) = do
      modify _{ licenseKey = key }
      pure next

    eval (AboutOpen next) = do
      modify _{ aboutOpen = true }
      pure next

    eval (AboutClose next) = do
      modify _{ aboutOpen = false }
      pure next

    eval (LogOut next) = do
      apiCallParent logout \_ ->
        modify _{ authStatus = LoggedOut
                , customerId = ""
                , licenseKey = "" }
      pure next

renderAuthForm :: String -> String -> Maybe String -> ParentHTML ChildState Query ChildQuery Metrix ChildSlot
renderAuthForm customerId licenseKey authError =
  H.div [ cls "splash-background" ]
  [ H.div [ cls "splash-cube" ] []
  , H.div [ cls "splash-auth" ]
    [ H.div [ cls "splash-auth-logo" ] []
    , H.div [ cls "splash-auth-box" ] $
      [ H.p_ [ H.text "Please enter your customer id and license key:" ]
      , H.input
        [ E.onValueChange $ E.input SetCustomerId
        , P.value customerId
        , P.placeholder "ID"
        ]
      , H.input
        [ E.onValueChange $ E.input SetLicenseKey
        , P.value licenseKey
        , P.placeholder "License Key"
        ]
      , H.button
        [ E.onClick (E.input_ Authenticate) ]
        [ H.text "Authenticate" ]
      ] <> (
        case authError of
          Just err ->
            [ H.p_ [ H.text $ "Auth error: " <> err ]
            ]
          Nothing ->
            []
      )
    ]
  ]
