module Component.ErrorBox
  ( raise
  , State()
  , initialState
  , Query()
  , errorBox
  ) where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception (catchException)

import Data.Maybe
import Data.Nullable (toMaybe)
import Data.Foldable (for_)

import DOM
import DOM.HTML.Types (HTMLElement())
import DOM.Event.EventTarget (eventListener, addEventListener, dispatchEvent)
import DOM.Event.Types (EventType(..))
import DOM.HTML (window)
import DOM.HTML.Types (htmlElementToEventTarget, htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.Types (elementToEventTarget)
import DOM.Node.ParentNode (querySelector)

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed as E

import Component.Common (modal)

import Types
import Utils

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

type State = Maybe ErrorDetail

initialState :: State
initialState = Nothing

data Query a
  = Init HTMLElement a
  | Open ErrorDetail a
  | Close a

errorBox :: Component State Query Metrix
errorBox = component render eval
  where

    render :: Render State Query
    render st = H.div
      [ P.initializer \el -> action (Init el)
      , P.id_ errorId
      ] $ case st of
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

    eval :: Eval Query State Query Metrix
    eval (Init el next) = do
      let attach cb = addEventListener errorEvent
            (eventListener \e -> cb $ errorEventDetail e) true (htmlElementToEventTarget el)
      subscribe $ eventSource attach \detail -> do
        pure $ action $ Open $ fromMaybe
          { title: "Internal error"
          , body: "Error reading event detail."
          } detail
      pure next
    eval (Open detail next) = do
      modify $ const $ Just detail
      pure next
    eval (Close next) = do
      modify $ const Nothing
      pure next
