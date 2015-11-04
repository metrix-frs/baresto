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
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Types
import Utils

errorId :: String
errorId = "error"

errorEvent :: EventType
errorEvent = EventType "error"

raise :: forall eff. String -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit
raise msg = do
  doc <- window >>= document
  maybeElem <- toMaybe <$> querySelector ("#" <> errorId) (htmlDocumentToParentNode doc)
  for_ maybeElem \el -> catchException print do
    dispatchEvent (createCustomEvent errorEvent msg) (elementToEventTarget el)
    pure unit

--

type State = Maybe String

initialState :: State
initialState = Nothing

data Query a
  = Init HTMLElement a
  | Open String a
  | Close a

errorBox :: Component State Query Metrix
errorBox = component render eval
  where

    render :: Render State Query
    render st = H.div
      [ P.initializer \el -> action (Init el)
      , P.id_ errorId
      ] $ case st of
            Just msg ->
              [ H.div [ cls "modalContainer" ]
                [ H.div [ cls "modalFade" ] []
                , H.div [ cls "modal" ]
                  [ H.h1_ [ H.text "Error" ]
                  , H.p_ [ H.text msg ]
                  , H.div [ cls "controls" ]
                    [ H.button
                      [ E.onClick (E.input_ Close) ]
                      [ H.text "Close" ]
                    ]
                  ]
                ]
              ]
            Nothing ->
              []

    eval :: Eval Query State Query Metrix
    eval (Init el next) = do
      let attach cb = addEventListener errorEvent
            (eventListener \e -> cb $ customEventDetail e) true (htmlElementToEventTarget el)
      subscribe $ eventSource attach \msg -> do
        pure $ action $ Open $ fromMaybe "Error reading event" msg
      pure next
    eval (Open msg next) = do
      modify $ const $ Just msg
      pure next
    eval (Close next) = do
      modify $ const Nothing
      pure next
