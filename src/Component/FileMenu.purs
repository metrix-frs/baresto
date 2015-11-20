module Component.FileMenu where

import Prelude

import Data.Maybe

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
  | LocationTags

type State =
  { open :: Boolean
  , location :: Location
  , csvImportResponse :: Maybe (ServerResponse CsvImportConf)
  , lastUpdateId :: UpdateId
  }

_open :: LensP State Boolean
_open = lens _.open _{ open = _ }

initialState :: UpdateId -> State
initialState updateId =
  { open: false
  , location: LocationHome
  , csvImportResponse: Nothing
  , lastUpdateId: updateId
  }

data Query a
  = ToggleOpen a
  | Go Location a
  | UploadCsv a
  | UploadCsvCancel a
  | UploadCsvConfirm UpdateGet a
  | SetLastUpdateId UpdateId a

fileMenu :: Component State Query Metrix
fileMenu = component render eval
  where

    render :: Render State Query
    render st = H.div_ $
      [ H.div
        [ cls "tool-menu"
        , E.onClick $ E.input_ ToggleOpen ]
        [ H.span [ cls "mega-octicon octicon-three-bars" ] []
        ]
      ] <> if st.open
             then [ renderMenu st ]
             else []

    eval :: Eval Query State Query Metrix
    eval (ToggleOpen next) = do
      modify $ _open %~ (not :: Boolean -> Boolean)
      pure next

    eval (Go location next) = do
      modify $ _{ location = location }
      pure next

    eval (UploadCsv next) = do
      mFiles <- liftEff' $ getInputFileList "csvFile"
      case mFiles of
        Nothing -> pure unit
        Just files -> apiCall (uploadCsv files) \resp ->
          modify $ _{ csvImportResponse = Just resp }
      pure next

    -- peeked by FileViewer
    eval (UploadCsvConfirm _ next) = do
      modify _{ open = false
              , location = LocationHome
              , csvImportResponse = Nothing
              }
      pure next

    eval (UploadCsvCancel next) = do
      modify _{ open = false
              , location = LocationHome
              , csvImportResponse = Nothing
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
          [ E.onClick $ E.input_ (Go LocationImportCsv) ]
          [ H.span [ cls "octicon octicon-repo-push" ] []
          , H.text "Import CSV"
          ]
        , H.li
          [ E.onClick $ E.input_ (Go LocationTags) ]
          [ H.span [ cls "octicon octicon-git-commit" ] []
          , H.text "Tags"
          ]
        , H.li_
          [ H.a
            [ P.href $ "/api/v0.1/xbrl/create/" <> show st.lastUpdateId
            , P.target "_blank"
            ]
            [ H.text "Export XBRL" ]
          ]
        ]
      ]
    LocationImportCsv -> case st.csvImportResponse of
      Nothing ->
        [ H.button
          [ E.onClick $ E.input_ (Go LocationHome) ]
          [ H.text "Back" ]
        , H.text "Import CSV"
        , H.input
          [ P.inputType P.InputFile
          , P.id_ "csvFile"
          ]
        , H.button
          [ E.onClick $ E.input_ UploadCsv ]
          [ H.span [ cls "octicon octicon-repo-push" ] []
          , H.text "Import CSV"
          ]
        ]
      Just resp -> case resp of
        ServerSuccess (CsvImportConf conf) ->
          [ H.p_ [ H.text "Csv successfully imported. The following warnings were issued:" ]
          , H.ul_ $ renderCsvWarning <$> conf.warnings
          , H.p_ [ H.text "Do you want to apply the imported changes to the current file?" ]
          , H.button
            [ E.onClick $ E.input_ UploadCsvCancel ]
            [ H.text "No, discard changes" ]
          , H.button
            [ E.onClick $ E.input_ (UploadCsvConfirm conf.changes) ]
            [ H.text "Yes, apply changes" ]
          ]
        ServerError err ->
          [ H.h2_ [ H.text err.title ]
          , H.p_ [ H.text err.body ]
          , H.button
            [ E.onClick $ E.input_ UploadCsvCancel ]
            [ H.text "Ok" ]
          ]
    LocationTags ->
      [ H.button
        [ E.onClick $ E.input_ (Go LocationHome) ]
        [ H.text "Back" ]
      , H.text "Tags"
      ]

renderCsvWarning :: Warning -> ComponentHTML Query
renderCsvWarning (Warning w) = H.li_
  [ H.b_ [ H.text "Message: " ]
  , H.text w.message
  , H.b_ [ H.text "Context: " ]
  , H.text w.context
  ]
