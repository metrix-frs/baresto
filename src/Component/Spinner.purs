module Component.Spinner
  ( dispatch
  , State()
  , initialState
  , Query()
  , spinner
  ) where

import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)
import Control.Monad.Eff.Exception (catchException)
import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener, dispatchEvent)
import DOM.Event.Types (EventType(..))
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement, htmlElementToEventTarget, htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector)
import DOM.Node.Types (elementToEventTarget)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (Natural)
import Data.Nullable (toMaybe)
import Halogen (ComponentDSL, ComponentHTML, Component, modify, action, eventSource_, subscribe, gets, lifecycleComponent)
import Prelude (Unit, pure, (-), bind, (+), ($), (>), unit, (<>), (<$>), (>>=))
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

type State =
  { calls :: Int
  , element :: Maybe HTMLElement
  }

initialState :: State
initialState =
  { calls: 0
  , element: Nothing
  }

data Query a
  = Initialize a
  | SetElement (Maybe HTMLElement) a
  | Inc a
  | Dec a

spinner :: Component State Query Metrix
spinner = lifecycleComponent
    { render
    , eval
    , initializer: Just (action Initialize)
    , finalizer: Nothing
    }

render :: State -> ComponentHTML Query
render st = H.div
  [ P.ref \el -> action (SetElement el)
  , P.id_ spinnerName
  , cls "spinnerContainer"
  ] $ if st.calls > 0
        then [ H.span [ cls "spinner-on" ] [] ]
        else [ H.div [ cls "spinner-off" ] [] ]

eval :: Natural Query (ComponentDSL State Query Metrix)
eval (Initialize next) = do
  el <- gets _.element
  case el of
    Nothing -> pure unit
    Just el' -> do
      let attach typ callback = addEventListener typ
            (eventListener \_ -> callback) true (htmlElementToEventTarget el')
      subscribe $ eventSource_ (attach spinnerOn) do
        pure $ action Inc
      subscribe $ eventSource_ (attach spinnerOff) do
        pure $ action Dec
  pure next
eval (SetElement el next) = do
  modify $ _{ element = el }
  pure next
eval (Inc next) = do
  modify \st -> st { calls = st.calls + 1 }
  pure next
eval (Dec next) = do
  modify \st -> st { calls = st.calls - 1 }
  pure next
