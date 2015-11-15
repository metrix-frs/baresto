module Component.File where

import Prelude

import Data.Maybe

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
  , deleteConfirmOpen :: Boolean
  , renaming :: Maybe String
  , tags :: Array Tag
  }

_file :: LensP State File
_file = lens _.file _{ file = _ }

_tagsOpen :: LensP State Boolean
_tagsOpen = lens _.tagsOpen _{ tagsOpen = _ }

initialState :: File -> State
initialState f =
  { file: f
  , tagsOpen: false
  , deleteConfirmOpen: false
  , renaming: Nothing
  , tags: []
  }

data Query a
  = Init a
  | Open ModuleId UpdateId a
  | Delete a
  | DeleteYes a
  | DeleteNo a
  | RenameOpen a
  | RenameSetNewName String a
  | RenameDone a
  | ToggleTagsOpen a

file :: Component State Query Metrix
file = component render eval
  where

    render :: Render State Query
    render st = case st.file of
      File f -> H.li
        [ cls "file"
        , P.initializer \_ -> action Init
        ] $
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
        ] <> (
          if st.deleteConfirmOpen
            then
              [ H.text "Really delete? All data will be lost!"
              , H.button
                [ E.onClick $ E.input_ DeleteNo ]
                [ H.text "No" ]
              , H.button
                [ E.onClick $ E.input_ DeleteYes ]
                [ H.text "Yes" ]
              ]
            else
              [ H.button
                [ E.onClick $ E.input_ Delete ]
                [ H.text "Delete" ]
              ]
        ) <> (
          case st.renaming of
            Just name ->
              [ H.input
                [ E.onValueChange $ E.input RenameSetNewName
                , P.value name
                ]
              , H.button
                [ E.onClick $ E.input_ RenameDone ]
                [ H.text "Ok" ]
              ]
            Nothing ->
              [ H.button
                [ E.onClick $ E.input_ RenameOpen ]
                [ H.text "Rename" ]
              ]
        )

    eval :: Eval Query State Query Metrix
    eval (Init next) = do
      pure next

    eval (Open _ _ next) = do
      pure next

    eval (Delete next) = do
      modify $ _{ deleteConfirmOpen = true }
      pure next

    eval (DeleteYes next) = do
      pure next

    eval (DeleteNo next) = do
      modify $ _{ deleteConfirmOpen = false }
      pure next

    eval (RenameOpen next) = do
      (File f) <- gets _.file
      modify $ _{ renaming = Just f.fileLabel }
      pure next

    eval (RenameSetNewName name next) = do
      modify $ _{ renaming = Just name }
      pure next

    eval (RenameDone next) = do
      renaming <- gets _.renaming
      (File f) <- gets _.file
      case renaming of
        Nothing -> pure unit
        Just newName -> apiCall (renameFile f.fileId newName) \_ -> do
          modify $ _file .. _fileLabel .~ newName
          pure unit
      modify $ _{ renaming = Nothing }
      pure next

    eval (ToggleTagsOpen next) = do
      modify $ _tagsOpen %~ (not :: Boolean -> Boolean)
      pure next
