module Component.Spinner
  ( dispatch
  , State()
  , initialState
  , Query()
  , spinner
  ) where

import Prelude (Unit, pure, (-), bind, (+), ($), (>), unit, (<>), (<$>), (>>=))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)
import Control.Monad.Eff.Exception (catchException)

import Data.Nullable (toMaybe)
import Data.Foldable (for_)

import DOM (DOM)
import DOM.HTML.Types (HTMLElement())
import DOM.Event.EventTarget (eventListener, addEventListener, dispatchEvent)
import DOM.Event.Types (EventType(..))
import DOM.HTML (window)
import DOM.HTML.Types (htmlElementToEventTarget, htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.Types (elementToEventTarget)
import DOM.Node.ParentNode (querySelector)

import Halogen (Eval, Render, Component, component, modify, action, eventSource_, subscribe)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Types (Metrix)
import Utils (cls, createEvent)

spinnerName :: String
spinnerName = "spinner"

spinnerOn :: EventType
spinnerOn = EventType "spinnerOn"

spinnerOff :: EventType
spinnerOff = EventType "spinnerOff"

dispatch :: forall eff. Boolean -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit
dispatch on = do
  doc <- window >>= document
  maybeElem <- toMaybe <$> querySelector ("#" <> spinnerName) (htmlDocumentToParentNode doc)
  for_ maybeElem \el -> catchException print do
    dispatchEvent (createEvent $ if on then spinnerOn else spinnerOff) (elementToEventTarget el)
    pure unit

--

data State = State Int

initialState :: State
initialState = State 0

data Query a
  = Init HTMLElement a
  | Inc a
  | Dec a

spinner :: Component State Query Metrix
spinner = component render eval
  where

    render :: Render State Query
    render (State count) = H.div
      [ P.initializer \el -> action (Init el)
      , P.id_ spinnerName
      , cls "spinnerContainer"
      ] $ if count > 0
            then [ H.span [ cls "spinner-on" ] [] ]
            else [ H.div [ cls "spinner-off" ] [] ]

    eval :: Eval Query State Query Metrix
    eval (Init el next) = do
      let attach typ callback = addEventListener typ
            (eventListener \_ -> callback) true (htmlElementToEventTarget el)
      subscribe $ eventSource_ (attach spinnerOn) do
        pure $ action Inc
      subscribe $ eventSource_ (attach spinnerOff) do
        pure $ action Dec
      pure next
    eval (Inc next) = do
      modify (\(State count) -> State (count + 1))
      pure next
    eval (Dec next) = do
      modify (\(State count) -> State (count - 1))
      pure next
