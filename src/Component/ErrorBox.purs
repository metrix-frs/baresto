module Component.ErrorBox
  ( raise
  , State()
  , initialState
  , Query()
  , errorBox
  ) where

import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Component.Common (modal)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)
import Control.Monad.Eff.Exception (catchException)
import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener, dispatchEvent)
import DOM.Event.Types (EventType(EventType))
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement, htmlElementToEventTarget, htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector)
import DOM.Node.Types (elementToEventTarget)
import Data.Foldable (for_)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.NaturalTransformation (Natural)
import Data.Nullable (toMaybe)
import Halogen (ComponentDSL, ComponentHTML, Component, modify, action, eventSource, subscribe, gets, lifecycleComponent)
import Prelude (Unit, pure, bind, ($), unit, (<>), (<$>), (>>=))
import Types (Metrix, ErrorDetail)
import Utils (errorEventDetail, createErrorEvent)

errorId :: String
errorId = "error"

errorEvent :: EventType
errorEvent = EventType "error"

raise :: forall eff. ErrorDetail -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit
raise detail = do
  doc <- window >>= document
  maybeElem <- toMaybe <$> querySelector ("#" <> errorId) (htmlDocumentToParentNode doc)
  for_ maybeElem \el -> catchException print do
    dispatchEvent (createErrorEvent errorEvent detail) (elementToEventTarget el)
    pure unit

--

type State =
  { error :: Maybe ErrorDetail
  , element :: Maybe HTMLElement
  }

initialState :: State
initialState =
  { error: Nothing
  , element: Nothing
  }

data Query a
  = Initialize a
  | SetElement (Maybe HTMLElement) a
  | Open ErrorDetail a
  | Close a

errorBox :: Component State Query Metrix
errorBox = lifecycleComponent
  { render
  , eval
  , initializer: Just (action Initialize)
  , finalizer: Nothing
  }

render :: State -> ComponentHTML Query
render st = H.div
  [ P.ref \el -> action (SetElement el)
  , P.id_ errorId
  ] $ case st.error of
        Just detail ->
          [ modal "Error"
            [ H.p_ [ H.b_ [ H.text detail.title ] ]
            , H.p_ [ H.text detail.body ]
            ]
            [ H.button
              [ E.onClick (E.input_ Close) ]
              [ H.text "Close" ]
            ]
          ]
        Nothing ->
          []

eval :: Natural Query (ComponentDSL State Query Metrix)
eval (Initialize next) = do
  el <- gets _.element
  case el of
    Nothing -> pure unit
    Just el' -> do
      let attach cb = addEventListener errorEvent
            (eventListener \e -> cb $ errorEventDetail e) true (htmlElementToEventTarget el')
      subscribe $ eventSource attach \detail -> do
        pure $ action $ Open $ fromMaybe
          { title: "Internal error"
          , body: "Error reading event detail."
          } detail
  pure next
eval (SetElement el next) = do
  modify _{ element = el }
  pure next
eval (Open detail next) = do
  modify _{ error = Just detail }
  pure next
eval (Close next) = do
  modify _{ error = Nothing }
  pure next
