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
  }

initialState :: State
initialState =
  { open: false
  , findings: []
  }

data Query a
  = ToggleOpen a
  | Validate a

validation :: ModuleId -> Component State Query Metrix
validation propModId = component render eval
  where

    render :: Render State Query
    render st = if st.open
      then H.div
        [ cls "validation-open" ]
        [ H.button
          [ E.onClick $ E.input_ Validate ]
          [ H.span [ cls "octicon octicon-tasklist" ] []
          , H.text "Validate" ]
        , H.button
          [ E.onClick $ E.input_ ToggleOpen ]
          [ H.text "Close" ]
        , H.ul_ $ renderFinding <$> st.findings
        ]
      else H.div
        [ cls "validation-closed"
        , E.onClick $ E.input_ ToggleOpen
        ]
        [ H.text "^"
        ]

    -- TODO purescript-halogen issue about type inference
    htmlProblem :: forall f. Int -> ComponentHTML f
    htmlProblem x = H.li_ ([H.br_ :: ComponentHTML f, H.text "hl" ] <> [H.li_ [], H.br_])

    eval :: Eval Query State Query Metrix
    eval (ToggleOpen next) = do
      modify \st -> st{ open = not st.open }
      pure next

    eval (Validate next) = do
      apiCall (validate propModId) \findings ->
        modify _{ findings = findings }
      pure next
