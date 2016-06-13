module Component.File where

import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Api (getFileOrphans, apiCall, getFileTags, renameTag, renameFile, pruneOrphan, deleteTag, apiUrl)
import Api.Schema.BusinessData (TagDesc(TagDesc), UpdateDesc(UpdateDesc))
import Api.Schema.File (File(File), _fileLabel, _fileCreated, _fileChanged, _fileId, _fileLastUpdateId)
import Component.Common (modal)
import Control.Monad (when)
import Data.Array (filter)
import Data.Foldable (find)
import Data.Maybe (Maybe(Just, Nothing))
import Data.NaturalTransformation (Natural)
import Halogen (ComponentDSL, ComponentHTML, Component, modify, gets, action, lifecycleComponent)
import Optic.Core (LensP, (%~), (.~), (..), (^.), lens)
import Prelude (pure, ($), bind, not, unit, map, (==), (/=), show, (<>), (<$>))
import Types (Metrix, TagId, UpdateId)
import Utils (cls)

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
  | Open UpdateId a
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
file = lifecycleComponent
  { render
  , eval
  , initializer: Just (action Init)
  , finalizer: Nothing
  }

render :: State -> ComponentHTML Query
render st = H.div_ $
  [ H.li
    [ cls "file" ] $
    [ H.span
      [ cls $ "hlabel octicon octicon-chevron-" <> if st.tagsOpen then "down" else "right"
      , E.onClick $ E.input_ $ if st.tagsOpen then TagsClose else TagsOpen
      ] []
    ] <> (
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
          [ H.span
            [ cls "hlabel"
            , E.onClick $ E.input_ $ Open (st.file ^. _fileLastUpdateId)
            ]
            [ H.text (st.file ^. _fileLabel)
            ]
          ]
    ) <>
    [ H.div
      [ cls "actions" ]
      [ H.span
        [ cls "hlabel octicon octicon-pencil"
        , E.onClick $ E.input_ RenameFileStart
        ] []
      , H.a
        [ P.href $ apiUrl <> "/api/v0.1/baresto/export/" <> show (st.file ^. _fileId)
        , P.target "_blank"
        ]
        [ H.span
          [ cls "hlabel octicon octicon-arrow-down"
          , P.title "Export as baresto file"
          ] []
        ]
      , H.span
        [ cls "hlabel octicon octicon-x"
        , E.onClick $ E.input_ DeleteFile
        ] []
      ]
    , H.div [ cls "details" ]
      [ H.div [ cls "edited" ]
        [ H.text $ "Last edited: " <> show (st.file ^. _fileChanged)
        ]
      , H.div [ cls "created" ]
        [ H.text $ "Created: " <> show (st.file ^. _fileCreated)
        ]
      ]
    ] <> (
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
        DTag _ ->
          [ modal "Delete Tag"
            [ H.p_ [ H.text "Are you sure you want to delete this tag?" ] ]
            [ H.button
              [ E.onClick $ E.input_ DeleteNo ]
              [ H.text "No" ]
            , H.button
              [ E.onClick $ E.input_ DeleteTagYes ]
              [ H.text "Yes" ]
            ]
          ]
        DOrphan _ ->
          [ modal "Delete Autosave"
            [ H.p_ [ H.text "Really delete? All data will be lost and there is no way to recover!" ] ]
            [ H.button
              [ E.onClick $ E.input_ DeleteNo ]
              [ H.text "No" ]
            , H.button
              [ E.onClick $ E.input_ DeleteOrphanYes ]
              [ H.text "Yes" ]
            ]
          ]
        _ ->
          [ ]
    )
  ] <> (
    if st.tagsOpen
      then
        [ H.li
          [ cls "tag-title" ]
          [ H.span
            [ cls "label octicon octicon-tag" ]
            []
          , H.span
            [ cls "label" ]
            [ H.text "Tags"
            ]
          ]
        ] <> (renderTag st <$> st.tags) <>
        [ H.li
          [ cls "orphan-title" ]
          [ H.span
            [ cls "label octicon octicon-watch" ]
            []
          , H.span
            [ cls "label" ]
            [ H.text "Autosaves"
            ]
          ]
        ] <> (renderOrphan st <$> st.orphans)
      else
        []
  ) <>
  [ H.li
    [ cls "sep"
    ] []
  ]

renderTag :: State -> TagDesc -> ComponentHTML Query
renderTag st (TagDesc tag) =
  H.li
  [ cls "tag" ] $
  ( case st.renaming of
      RTag tagId name | tagId == tag.tagDescTagId ->
        [ H.input
          [ E.onValueChange $ E.input RenameTagSetNewName
          , P.value name
          ]
        , H.button
          [ E.onClick $ E.input_ RenameTagDone ]
          [ H.text "Ok" ]
        ]
      _ ->
        [ H.span
          [ cls "hlabel"
          , E.onClick $ E.input_ $ Open tag.tagDescUpdateId
          ]
          [ H.text tag.tagDescTagName ]
        ]
  ) <>
  [ H.div
    [ cls "actions" ]
    [ H.span
      [ cls "hlabel octicon octicon-pencil"
      , E.onClick $ E.input_ $ RenameTagStart tag.tagDescTagId
      ] []
    , H.span
      [ cls "hlabel octicon octicon-x"
      , E.onClick $ E.input_ $ DeleteTag tag.tagDescTagId
      ] []
    ]
  ]

renderOrphan :: State -> UpdateDesc -> ComponentHTML Query
renderOrphan st (UpdateDesc upd) =
  H.li
  [ cls "orphan" ]
  [ H.span
    [ cls "hlabel"
    , E.onClick $ E.input_ $ Open upd.updateDescUpdateId
    ]
    [ H.text $ show upd.updateDescCreated
    ]
  , H.div
    [ cls "actions" ]
    [ H.span
      [ cls "hlabel octicon octicon-x"
      , E.onClick $ E.input_ $ DeleteOrphan upd.updateDescUpdateId
      ] []
    ]
  ]

eval :: Natural Query (ComponentDSL State Query Metrix)
eval (Init next) = do
  pure next

-- peeked by Body
eval (Open _ next) = do
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
      (File f) <- gets _.file
      apiCall (getFileOrphans f.fileId) \orphans ->
        modify $ _{ orphans = orphans }
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
    RFile newName -> apiCall (renameFile f.fileId newName) \resultName ->
      modify $ _file .. _fileLabel .~ resultName
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
    RTag tagId newName -> apiCall (renameTag tagId newName) \resultName -> do
      let rename (TagDesc t) = TagDesc $ if t.tagDescTagId == tagId
            then t { tagDescTagName = resultName }
            else t
      modify $ _tags %~ map rename
      modify $ _{ renaming = RNone }
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
