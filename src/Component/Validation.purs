module Component.Validation where

import Prelude

import qualified Data.Map as M
import           Data.Maybe
import           Data.Array hiding ((..))
import           Data.Foldable

import Control.Monad.State (execState)

import Optic.Core
import Optic.At

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Types
import Utils

import Api
import Api.Schema.Finding

import Component.Validation.Finding

type State =
  { open :: Boolean
  , findings :: Array Finding
  , updateId :: UpdateId
  }

initialState :: UpdateId -> State
initialState updateId =
  { open: false
  , findings: []
  , updateId: updateId
  }

data Query a
  = ToggleOpen a
  | SetUpdateId UpdateId a
  | Validate a

validation :: Component State Query Metrix
validation = component render eval
  where

    render :: Render State Query
    render st = if st.open
      then H.div
        [ cls "validation-open" ]
        [ H.button
          [ E.onClick $ E.input_ ToggleOpen ]
          [ H.span [ cls "octicon octicon-chevron-down" ] [] ]
        , H.br_
        , H.br_
        , H.button
          [ E.onClick $ E.input_ Validate ]
          [ H.span [ cls "octicon octicon-checklist" ] []
          ]
        , H.div [ cls "validation-content" ]
          [ H.ul_ $ renderFinding <$> st.findings
          ]
        ]
      else H.div
        [ cls "validation-closed"]
        [ H.button
          [ E.onClick $ E.input_ ToggleOpen ]
          [ H.span [ cls "octicon octicon-chevron-up" ] [] ]
        ]

    -- TODO purescript-halogen issue about type inference
    htmlProblem :: forall f. Int -> ComponentHTML f
    htmlProblem x = H.li_ ([H.br_ :: ComponentHTML f, H.text "hl" ] <> [H.li_ [], H.br_])

    eval :: Eval Query State Query Metrix
    eval (ToggleOpen next) = do
      modify \st -> st{ open = not st.open }
      pure next

    eval (SetUpdateId updateId next) = do
      modify _{ updateId = updateId }
      pure next

    eval (Validate next) = do
      updateId <- gets _.updateId
      apiCall (validate updateId) \findings ->
        modify _{ findings = findings }
      pure next
