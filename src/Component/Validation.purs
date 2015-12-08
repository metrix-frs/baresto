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
import Api.Schema.Validation

import Lib.Validation

import Component.Validation.Finding

type State =
  { open :: Boolean
  , updateId :: UpdateId
  , results :: ValidationResult
  }

initialState :: UpdateId -> State
initialState updateId =
  { open: false
  , updateId: updateId
  , results: emptyValidationResult
  }

_results :: LensP State ValidationResult
_results = lens _.results _{ results = _ }

data Query a
  = Open a
  | Close a
  | SetUpdateId UpdateId a
  | Patch ValidationResult a
  | ValidateAll a

validation :: Component State Query Metrix
validation = component render eval
  where

    render :: Render State Query
    render st = if st.open
      then H.div
        [ cls "validation-open" ]
        [ H.button
          [ E.onClick $ E.input_ Close ]
          [ H.span [ cls "octicon octicon-chevron-down" ] [] ]
        , H.br_
        , H.br_
        , H.button
          [ E.onClick $ E.input_ ValidateAll ]
          [ H.span [ cls "octicon octicon-checklist" ] []
          ]
        , H.div [ cls "validation-content" ]
          [ H.ul_ $ renderFinding <$> (flattenValidationResult st.results)
          ]
        ]
      else H.div
        [ cls "validation-closed"]
        [ H.button
          [ E.onClick $ E.input_ Open ]
          [ H.span [ cls "octicon octicon-chevron-up" ] [] ]
        ]

    -- TODO: report purescript-halogen issue about type inference
    htmlProblem :: forall f. Int -> ComponentHTML f
    htmlProblem x = H.li_ ([H.br_ :: ComponentHTML f, H.text "hl" ] <> [H.li_ [], H.br_])

    eval :: Eval Query State Query Metrix
    eval (Open next) = do
      modify _{ open = true }
      runValidation
      pure next

    eval (Close next) = do
      modify _{ open = false }
      pure next

    eval (SetUpdateId updateId next) = do
      modify _{ updateId = updateId }
      pure next

    eval (Patch patch next) = do
      modify $ _results %~ patchValidationResult patch
      pure next

    eval (ValidateAll next) = do
      runValidation
      pure next

    runValidation :: ComponentDSL State Query Metrix Unit
    runValidation = do
      updateId <- gets _.updateId
      apiCall (validate updateId) \results ->
        modify _{ results = results }
