module Component.Auth where

import Prelude

import Control.Monad.Eff.Class (liftEff)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import qualified Component.Spinner as Spinner

import Types

data State = State Boolean

initialState :: State
initialState = State false

data Query a
  = On a
  | Off a

auth :: forall eff. Component State Query (Metrix eff)
auth = component render eval
  where
    render :: Render State Query
    render (State on) = H.div_
      [ H.text (show on)
      , H.button [ E.onClick (E.input_ On) ] [ H.text "On" ]
      , H.button [ E.onClick (E.input_ Off) ] [ H.text "Off" ]
      ]

    eval :: Eval Query State Query (Metrix eff)
    eval (On next) = do
      modify (\_ -> State true)
      liftEff' $ Spinner.dispatch true
      pure next
    eval (Off next) = do
      modify (\_ -> State false)
      liftEff' $ Spinner.dispatch false
      pure next
