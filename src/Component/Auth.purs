module Component.Auth where

import Prelude

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import qualified Component.Spinner as Spinner

import Types

data State = State Boolean String

initialState :: String -> State
initialState msg = State false msg

data Query a
  = On a
  | Off a

auth :: forall eff. Component State Query (Metrix eff)
auth = component render eval
  where

    render :: Render State Query
    render (State on msg) = H.div_
      [ H.text (show on)
      , H.button [ E.onClick (E.input_ On) ] [ H.text "On" ]
      , H.button [ E.onClick (E.input_ Off) ] [ H.text "Off" ]
      , H.text msg
      ]

    eval :: Eval Query State Query (Metrix eff)
    eval (On next) = do
      modify (\_ -> State true "")
      liftEff' $ Spinner.dispatch true
      pure next
    eval (Off next) = do
      modify (\_ -> State false "")
      liftEff' $ Spinner.dispatch false
      pure next
