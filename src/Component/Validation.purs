module Component.Validation where

import Prelude
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Api (validate, apiCall)
import Api.Schema.Validation (ValidationResult, emptyValidationResult)
import Component.Validation.Finding (renderFinding)
import Data.Lens (Lens', lens, (%~))
import Data.Maybe (Maybe(Nothing, Just))
import Halogen (ComponentDSL, ComponentHTML, Component, modify, gets, action, lifecycleComponent)
import Lib.Validation (patchValidationResult, flattenValidationResult)
import Types (Metrix, UpdateId)
import Utils (maxOrd, cls, paginate)

type State =
  { open :: Boolean
  , updateId :: UpdateId
  , results :: ValidationResult
  , page :: Int
  }

initialState :: UpdateId -> State
initialState updateId =
  { open: false
  , updateId: updateId
  , results: emptyValidationResult
  , page: 1
  }

_results :: Lens' State ValidationResult
_results = lens _.results _{ results = _ }

_page :: Lens' State Int
_page = lens _.page _{ page = _ }

data Query a
  = Init a
  | Open a
  | Close a
  | PageNext a
  | PagePrev a
  | SetUpdateId UpdateId a
  | Patch ValidationResult a
  | ValidateAll UpdateId a

validation :: Component State Query Metrix
validation = lifecycleComponent
  { render
  , eval
  , initializer: Just (action Init)
  , finalizer: Nothing
  }

render :: State -> ComponentHTML Query
render st = H.div_
  [ if st.open
      then let pagination = paginate 25 (flattenValidationResult st.results) st.page in
        H.div
        [ cls "validation-open" ] $
        [ H.span
          [ cls "navigation octicon octicon-chevron-down"
          , E.onClick $ E.input_ Close
          ] []
        , H.span
          [ cls "pagination" ]
          [ if st.page > 1 then
              H.span
              [ cls "navigation octicon octicon-chevron-left"
              , E.onClick $ E.input_ PagePrev
              ] []
            else
              H.span
              [ cls "navigation octicon octicon-chevron-left disabled" ] []
          , H.span
            [ cls "fromto" ]
            [ H.b_ [ H.text $ show pagination.from ]
            , H.text " to "
            , H.b_ [ H.text $ show pagination.to ]
            , H.text " out of "
            , H.b_ [ H.text $ show pagination.total ]
            ]
          , if st.page < pagination.pages then
              H.span
              [ cls "navigation octicon octicon-chevron-right"
              , E.onClick $ E.input_ PageNext
              ] []
            else
              H.span
              [ cls "navigation octicon octicon-chevron-right disabled" ] []
          ]
        , H.div [ cls "validation-content" ]
          [ H.ul_ $ renderFinding <$> pagination.items
          ]
        ]
      else H.div_
        [ H.div
          [ cls "validation-closed"
          , E.onClick $ E.input_ Open
          ]
          [ H.span [ cls "navigation octicon octicon-chevron-up" ] []
          , H.text "Validation"
          ]
        ]
  ]

-- TODO: report purescript-halogen issue about type inference
htmlProblem :: forall f. Int -> ComponentHTML f
htmlProblem x = H.li_ ([H.br_ :: ComponentHTML f, H.text "hl" ] <> [H.li_ [], H.br_])

eval :: Query ~> ComponentDSL State Query Metrix
eval (Init next) = do
  updateId <- gets _.updateId
  apiCall (validate updateId) \results ->
    modify _{ results = results }
  pure next

eval (Open next) = do
  modify _{ open = true }
  pure next

eval (Close next) = do
  modify _{ open = false }
  pure next

eval (PageNext next) = do
  modify $ _page %~ \p -> p + 1
  pure next

eval (PagePrev next) = do
  modify $ _page %~ \p -> maxOrd 1 (p - 1)
  pure next

eval (SetUpdateId updateId next) = do
  modify _{ updateId = updateId }
  pure next

eval (Patch patch next) = do
  modify $ _results %~ patchValidationResult patch
  pure next

eval (ValidateAll updateId next) = do
  modify _{ updateId = updateId }
  apiCall (validate updateId) \results ->
    modify _{ results = results }
  pure next
