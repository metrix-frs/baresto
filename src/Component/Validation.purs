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
  = Init a
  | Open a
  | Close a
  | SetUpdateId UpdateId a
  | Patch ValidationResult a
  | ValidateAll UpdateId a

validation :: Component State Query Metrix
validation = component render eval
  where

    render :: Render State Query
    render st = H.div
      [ P.initializer $ \_ -> action $ Init
      ]
      [ if st.open
          then H.div
            [ cls "validation-open" ]
            [ H.span
              [ cls "octicon octicon-chevron-down"
              , E.onClick $ E.input_ Close
              ] []
            , H.div [ cls "validation-content" ]
              [ H.ul_ $ renderFinding <$> (flattenValidationResult st.results)
              ]
            ]
          else H.div_
            [ H.div
              [ cls "validation-closed"
              , E.onClick $ E.input_ Open
              ]
              [ H.span [ cls "octicon octicon-chevron-up" ] []
              , H.text "Validation"
              ]
            ]
      ]

    -- TODO: report purescript-halogen issue about type inference
    htmlProblem :: forall f. Int -> ComponentHTML f
    htmlProblem x = H.li_ ([H.br_ :: ComponentHTML f, H.text "hl" ] <> [H.li_ [], H.br_])

    eval :: Eval Query State Query Metrix
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
