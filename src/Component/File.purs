module Component.File where

import Prelude

import Control.Monad (when)

import Data.Maybe
import Data.Array (filter)
import Data.Foldable (find)

import Optic.Core

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Api
import Api.Schema.Selector
import Api.Schema.File
import Api.Schema.BusinessData

import Component.Common

import Types
import Utils

data Renaming
  = RNone
  | RFile String
  | RTag TagId String

data DeleteConfirm
  = DNone
  | DFile
  | DTag TagId
  | DOrphan UpdateId

type State =
  { file          :: File
  , tagsOpen      :: Boolean
  , tags          :: Array TagDesc
  , orphans       :: Array UpdateDesc
  , fetchedTags   :: Boolean
  , deleteConfirm :: DeleteConfirm
  , renaming      :: Renaming
  }

_file :: LensP State File
_file = lens _.file _{ file = _ }

_tags :: LensP State (Array TagDesc)
_tags = lens _.tags _{ tags = _ }

_orphans :: LensP State (Array UpdateDesc)
_orphans = lens _.orphans _{ orphans = _ }

initialState :: File -> State
initialState f =
  { file:          f
  , tagsOpen:      false
  , tags:          []
  , orphans:       []
  , fetchedTags:   false
  , deleteConfirm: DNone
  , renaming:      RNone
  }

data Query a
  = Init a
  | Open ModuleId UpdateId a
  | DeleteFile a
  | DeleteFileYes a
  | DeleteTag TagId a
  | DeleteTagYes a
  | DeleteOrphan UpdateId a
  | DeleteOrphanYes a
  | DeleteNo a
  | RenameFileStart a
  | RenameFileSetNewName String a
  | RenameFileDone a
  | RenameTagStart TagId a
  | RenameTagSetNewName String a
  | RenameTagDone a
  | TagsOpen a
  | TagsClose a

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
        , H.div [ cls "details" ] $
          [ H.text $ "Created: " <> f.fileCreated
          , H.button
            [ E.onClick $ E.input_ DeleteFile ]
            [ H.text "Delete" ]
          ] <> (
            if st.tagsOpen
              then
                [ H.button
                  [ E.onClick $ E.input_ TagsClose ]
                  [ H.text "Close tags" ]
                ]
              else
                [ H.button
                  [ E.onClick $ E.input_ TagsOpen ]
                  [ H.text "Open tags" ]
                ]
          ) <> (
            case st.deleteConfirm of
              DFile ->
                [ modal "Delete File"
                  [ H.p_ [ H.text "Really delete? All data will be lost and there is no way to recover!" ] ]
                  [ H.button
                    [ E.onClick $ E.input_ DeleteNo ]
                    [ H.text "No" ]
                  , H.button
                    [ E.onClick $ E.input_ DeleteFileYes ]
                    [ H.text "Yes" ]
                  ]
                ]
              _ ->
                [ ]
          ) <> (
            case st.renaming of
              RFile name ->
                [ H.input
                  [ E.onValueChange $ E.input RenameFileSetNewName
                  , P.value name
                  ]
                , H.button
                  [ E.onClick $ E.input_ RenameFileDone ]
                  [ H.text "Ok" ]
                ]
              _ ->
                [ H.button
                  [ E.onClick $ E.input_ RenameFileStart ]
                  [ H.text "Rename" ]
                ]
          )
        ]

    eval :: Eval Query State Query Metrix
    eval (Init next) = do
      pure next

    eval (Open _ _ next) = do
      pure next

    eval (DeleteFile next) = do
      modify $ _{ deleteConfirm = DFile }
      pure next

    eval (DeleteFileYes next) = do
      pure next

    eval (DeleteTag tagId next) = do
      modify $ _{ deleteConfirm = DTag tagId }
      pure next

    eval (DeleteTagYes next) = do
      del <- gets _.deleteConfirm
      case del of
        DTag tagId -> apiCall (deleteTag tagId) \_ -> do
          modify $ _{ deleteConfirm = DNone }
          modify $ _tags %~ filter (\(TagDesc t) -> t.tagDescTagId /= tagId)
        _ -> pure unit
      pure next

    eval (DeleteOrphan updId next) = do
      modify $ _{ deleteConfirm = DOrphan updId }
      pure next

    eval (DeleteOrphanYes next) = do
      del <- gets _.deleteConfirm
      case del of
        DOrphan updId -> apiCall (pruneOrphan updId) \_ -> do
          modify $ _{ deleteConfirm = DNone }
          modify $ _orphans %~ filter (\(UpdateDesc u) -> u.updateDescUpdateId /= updId)
        _ -> pure unit
      pure next

    eval (DeleteNo next) = do
      modify $ _{ deleteConfirm = DNone }
      pure next

    eval (RenameFileStart next) = do
      (File f) <- gets _.file
      modify $ _{ renaming = RFile f.fileLabel }
      pure next

    eval (RenameFileSetNewName name next) = do
      modify $ _{ renaming = RFile name }
      pure next

    eval (RenameFileDone next) = do
      renaming <- gets _.renaming
      (File f) <- gets _.file
      case renaming of
        RFile newName -> apiCall (renameFile f.fileId newName) \_ ->
          modify $ _file .. _fileLabel .~ newName
        _ -> pure unit
      modify $ _{ renaming = RNone }
      pure next

    eval (RenameTagStart tagId next) = do
      tags <- gets _.tags
      case find (\(TagDesc t) -> t.tagDescTagId == tagId) tags of
        Nothing -> pure unit
        Just (TagDesc t) -> modify $ _{ renaming = RTag tagId t.tagDescTagName }
      pure next

    eval (RenameTagSetNewName tagName next) = do
      renaming <- gets _.renaming
      case renaming of
        RTag tagId _ -> modify $ _{ renaming = RTag tagId tagName }
        _ -> pure unit
      pure next

    eval (RenameTagDone next) = do
      renaming <- gets _.renaming
      case renaming of
        RTag tagId newName -> apiCall (renameTag tagId newName) \_ -> do
          let rename (TagDesc t) = TagDesc $ if t.tagDescTagId == tagId
                then t { tagDescTagName = newName }
                else t
          modify $ _tags %~ map rename
        _ -> pure unit
      pure next

    eval (TagsOpen next) = do
      modify $ _{ tagsOpen = true }
      fetched <- gets _.fetchedTags
      when (not fetched) $ do
        (File f) <- gets _.file
        apiCall (getFileTags f.fileId) \tags ->
          modify $ _{ tags = tags }
        apiCall (getFileOrphans f.fileId) \orphans ->
          modify $ _{ orphans = orphans }
        modify $ _{ fetchedTags = true }
      pure next

    eval (TagsClose next) = do
      modify $ _{ tagsOpen = false }
      pure next
