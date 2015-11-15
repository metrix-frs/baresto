module Component.File where

import Prelude

import Optic.Core

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Api
import Api.Schema.Selector
import Api.Schema.File

import Types
import Utils

type State =
  { file :: File
  , tagsOpen :: Boolean
  , tags :: Array Tag
  }

_tagsOpen :: LensP State Boolean
_tagsOpen = lens _.tagsOpen _{ tagsOpen = _ }

initialState :: File -> State
initialState f =
  { file: f
  , tagsOpen: false
  , tags: []
  }

data Query a
  = Init a
  | Open ModuleId UpdateId a
  | Delete a
  | ToggleTagsOpen a

file :: Component State Query Metrix
file = component render eval
  where

    render :: Render State Query
    render st = case st.file of
      File f -> H.li
        [ cls "file"
        , P.initializer \_ -> action Init
        ]
        [ H.div
          [ cls "label"
          , E.onClick (E.input_ (Open f.fileModuleId f.fileLastUpdateId))
          ]
          [ H.span
            [ cls "octicon octicon-file-text"
            ] []
          , H.text f.fileLabel
          ]
        , H.div [ cls "details" ]
          [ H.text $ "Created: " <> f.fileCreated
          ]
        , H.button
          [ E.onClick $ E.input_ ToggleTagsOpen ]
          [ H.text $ if st.tagsOpen then "Close tags" else "Open tags" ]
        ]

    eval :: Eval Query State Query Metrix
    eval (Init next) = do
      pure next

    eval (Open _ _ next) = do
      pure next

    eval (Delete next) = do
      pure next

    eval (ToggleTagsOpen next) = do
      modify $ _tagsOpen %~ (not :: Boolean -> Boolean)
      pure next
