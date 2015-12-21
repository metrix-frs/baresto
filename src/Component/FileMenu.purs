module Component.FileMenu where

import Prelude

import Data.Maybe
import Data.Array (snoc)

import Optic.Core

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Api
import Api.Schema
import Api.Schema.Import
import Api.Schema.BusinessData

import Component.Common

import Types
import Utils

data Location
  = LocationHome
  | LocationImportCsv
  | LocationPast (Array UpdateDesc)

type State =
  { open :: Boolean
  , location :: Location
  , csvImportResponse :: Maybe CsvImportConf
  , newTagName :: String
  , lastUpdateId :: UpdateId
  }

_open :: LensP State Boolean
_open = lens _.open _{ open = _ }

initialState :: UpdateId -> State
initialState updateId =
  { open: false
  , location: LocationHome
  , csvImportResponse: Nothing
  , newTagName: ""
  , lastUpdateId: updateId
  }

data Query a
  = ToggleOpen a
  | GoHome a
  | GoImportCsv a
  | GoPast a
  | UploadCsv a
  | UploadCsvConfirm UpdateGet a
  | UploadCsvClose a
  | NewTagSetName String a
  | NewTagCreate a
  | OpenUpdate UpdateId a
  | SetLastUpdateId UpdateId a

fileMenu :: Component State Query Metrix
fileMenu = component render eval
  where

    render :: Render State Query
    render st = H.div_ $
      [ toolButton "Menu" "octicon octicon-three-bars" "menu" ToggleOpen
      ] <> if st.open
             then [ renderMenu st ]
             else []

    eval :: Eval Query State Query Metrix
    eval (ToggleOpen next) = do
      modify $ _open %~ (not :: Boolean -> Boolean)
      pure next

    eval (GoHome next) = do
      modify $ _{ location = LocationHome }
      pure next

    eval (GoImportCsv next) = do
      modify $ _{ location = LocationImportCsv }
      pure next

    eval (GoPast next) = do
      updateId <- gets _.lastUpdateId
      apiCall (getUpdatePast updateId) \past ->
        modify $ _{ location = LocationPast past }
      pure next

    eval (UploadCsv next) = do
      mFiles <- liftEff' $ getInputFileList "csvFile"
      updateId <- gets _.lastUpdateId
      case mFiles of
        Nothing -> pure unit
        Just files -> apiCall (uploadCsv updateId files) \resp ->
          modify $ _{ csvImportResponse = Just resp }
      pure next

    -- peeked by FileViewer
    eval (UploadCsvConfirm _ next) = do
      modify _{ open = false
              , location = LocationHome
              , csvImportResponse = Nothing
              }
      pure next

    eval (UploadCsvClose next) = do
      modify _{ csvImportResponse = Nothing }
      pure next

    eval (NewTagSetName name next) = do
      modify _{ newTagName = name }
      pure next

    eval (NewTagCreate next) = do
      st <- get
      if st.newTagName /= ""
        then apiCall (newTag st.lastUpdateId st.newTagName) \tag ->
              case st.location of
                LocationPast past -> do
                  let go (UpdateDesc upd) = UpdateDesc $
                        if upd.updateDescUpdateId == st.lastUpdateId
                          then upd { updateDescTags = snoc upd.updateDescTags tag }
                          else upd
                  modify $ _{ location = LocationPast (go <$> past) }
                  modify $ _{ newTagName = "" }
                _ -> pure unit
        else pure unit
      pure next

    -- peeked by FileViewer
    eval (OpenUpdate updateId next) = do
      modify _{ open = false
              , location = LocationHome
              , lastUpdateId = updateId
              }
      pure next

    eval (SetLastUpdateId updateId next) = do
      modify $ _{ lastUpdateId = updateId }
      pure next

renderMenu :: Render State Query
renderMenu st = H.div [ cls "menu-content" ] $
  case st.location of
    LocationHome ->
      [ H.ul_
        [ H.li
          [ E.onClick $ E.input_ GoPast ]
          [ H.span [ cls "octicon octicon-git-commit" ] []
          , H.text "Version History"
          ]
        , H.li
          [ E.onClick $ E.input_ GoImportCsv ]
          [ H.span [ cls "octicon octicon-repo-push" ] []
          , H.text "Import CSV"
          ]
        , H.li
          [ cls "href" ]
          [ H.a
            [ P.href $ "/api/v0.1/xbrl/create/" <> show st.lastUpdateId
            , P.target "_blank"
            ]
            [ H.span [ cls "octicon octicon-file-code" ] []
            , H.text "Export XBRL" ]
          ]
        , H.li
          [ cls "href" ]
          [ H.a
            [ P.href $ "/api/v0.1/csv/create/" <> show st.lastUpdateId
            , P.target "_blank"
            ]
            [ H.span [ cls "octicon octicon-file-symlink-file" ] []
            , H.text "Export CSV"
            ]
          ]
        ]
      ]
    LocationImportCsv -> case st.csvImportResponse of
      Nothing ->
        [ H.ul_
          [ H.li
            [ E.onClick $ E.input_ GoHome ]
            [ H.span [ cls "octicon octicon-arrow-left" ] []
            , H.text "Back"
            ]
          ]
        , H.div [ cls "entry-content" ]
          [ H.p_ [ H.text "Please select the CSV file to import:" ]
          , H.input
            [ cls "full"
            , P.inputType P.InputFile
            , P.id_ "csvFile"
            ]
          , H.button
            [ cls "full"
            , E.onClick $ E.input_ UploadCsv ]
            [ H.span [ cls "octicon octicon-repo-push" ] []
            , H.text "Import CSV"
            ]
          ]
        ]
      Just (CsvImportConf conf) ->
        [ modal "Import CSV"
          [ H.p_ [ H.text "Csv successfully imported!" ]
          , H.h2_ [ H.text "Warnings:" ]
          , H.ul_ $ renderCsvWarning <$> conf.warnings
          ]
          [ H.button
            [ E.onClick $ E.input_ (UploadCsvConfirm conf.update) ]
            [ H.text "Ok" ]
          ]
        ]
    LocationPast past ->
      [ H.ul_
          [ H.li
            [ E.onClick $ E.input_ GoHome ]
            [ H.span [ cls "octicon octicon-arrow-left" ] []
            , H.text "Back"
            ]
          ]
      , H.div [ cls "entry-content" ]
        [ H.input
          [ cls "full"
          , E.onValueChange $ E.input NewTagSetName
          , P.value st.newTagName
          , P.placeholder "Tag Name"
          ]
        , H.button
          [ cls "full"
          , E.onClick $ E.input_ NewTagCreate ]
          [ H.text "Create Tag" ]
        , H.table_ $ renderUpdate <$> past
        ]
      ]

renderCsvWarning :: Warning -> ComponentHTML Query
renderCsvWarning (Warning w) = H.li_
  [ H.b_ [ H.text "Message: " ]
  , H.text w.message
  , H.br_
  , H.b_ [ H.text "Context: " ]
  , H.text w.context
  ]

renderUpdate :: UpdateDesc -> ComponentHTML Query
renderUpdate (UpdateDesc upd) = H.tr_ $
    [ H.td_
      [ H.span
        [ cls "label"
        , E.onClick $ E.input_ (OpenUpdate upd.updateDescUpdateId)
        ]
        [ H.text $ show upd.updateDescCreated ]
      ]
    , H.td_ (renderTag <$> upd.updateDescTags)
    ]
  where
    renderTag (TagDesc tag) =
      H.span [ cls "tag" ]
      [ H.span [ cls "octicon octicon-tag" ] []
      , H.text tag.tagDescTagName
      ]
