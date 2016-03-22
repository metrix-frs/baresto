module Component.FileMenu where

import Prelude

import Data.Maybe
import Data.Array (snoc)
import Data.Tuple
import Data.String (take)

import Optic.Core

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed as E

import Api
import Api.Schema
import Api.Schema.Import
import Api.Schema.BusinessData

import Api (apiUrl)

import Component.Common

import Component.Validation.Finding (renderHoleCoords)

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
      [ H.ul
        [ cls "menu" ]
        [ H.li
          [ E.onClick $ E.input_ GoPast ]
          [ H.span [ cls "octicon octicon-git-commit" ] []
          , H.text "Version History"
          ]
        , H.li
          [ cls "href" ]
          [ H.a
            [ P.href $ apiUrl <> "/api/v0.1/xbrl/create/" <> show st.lastUpdateId
            , P.target "_blank"
            ]
            [ H.span [ cls "octicon octicon-file-code" ] []
            , H.text "Export XBRL"
            ]
          ]
        , H.li
          [ cls "href" ]
          [ H.a
            [ P.href $ apiUrl <> "/api/v0.1/xbrl/createExtraNet/" <> show st.lastUpdateId
            , P.target "_blank"
            ]
            [ H.span [ cls "octicon octicon-file-code" ] []
            , H.text "Export Zipped XBRL for ExtraNet"
            ]
          ]
        , H.li
          [ E.onClick $ E.input_ GoImportCsv ]
          [ H.span [ cls "octicon octicon-repo-push" ] []
          , H.text "Import CSV"
          ]
        , H.li
          [ cls "href" ]
          [ H.a
            [ P.href $ apiUrl <> "/api/v0.1/csv/create/" <> show st.lastUpdateId
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
        [ H.ul
          [ cls "menu" ]
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
      [ H.ul
        [ cls "menu" ]
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
        , H.ul [ cls "updates" ] $ renderUpdate <$> past
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
renderUpdate (UpdateDesc upd) = H.li
    [ E.onClick $ E.input_ (OpenUpdate upd.updateDescUpdateId)
    ]
    [ H.span
      [ cls "label" ]
      [ H.b_ [ H.text $ show upd.updateDescCreated ]
      , H.text " by "
      , H.b_ [ H.text $ upd.updateDescAuthor ]
      ]
    , H.div
      [ cls "tags"
      ] (renderTag <$> upd.updateDescTags)
    , H.ul [ cls "entries" ] $ renderUpdateEntry <$> upd.updateDescEntries
    ]
  where
    renderTag (TagDesc tag) =
      H.span [ cls "tag" ]
      [ H.span [ cls "octicon octicon-tag" ] []
      , H.text tag.tagDescTagName
      ]
    renderUpdateEntry e@(UpdateEntry entry) =
      case entry.updateEntryLoc of
        HumanHeaderFact label -> entryLayout
          [ H.text $ "Header, " <> label ]
          [ renderChange e ]
        HumanFact table coords -> entryLayout
          [ H.text $ table <> ", "
          , renderHoleCoords coords
          ]
          [ renderChange e ]
        HumanSubsetZ table member -> entryLayout
          [ H.text $ table <> ", z axis member '" <> member <> "'" ]
          [ renderAddDelete e "selected" "deselected" ]
        HumanCustomZ table -> entryLayout
          [ H.text $ table <> ", z axis member" ]
          [ renderChange e ]
        HumanCustomRow table member sheet -> entryLayout
          [ H.text $ table <> ", row '" <> take 8 member <> "'" <> (if sheet /= "" then " on sheet '" <> take 8 sheet <> "'" else "") ]
          [ renderAddDelete e "added" "deleted" ]
    entryLayout location action = H.li_
      [ H.div [ cls "location" ] $
        [ H.div [ cls "action" ] action
        ] <> location
      ]
    renderChange (UpdateEntry entry) = H.text $
      case Tuple entry.updateEntryOld entry.updateEntryNew of
        Tuple Nothing (Just n)  -> "added '" <> n <> "'"
        Tuple (Just o) Nothing  -> "deleted '" <> o <> "'"
        Tuple (Just o) (Just n) -> "'" <> o <> "' > '" <> n <> "'"
        _                       -> ""
    renderAddDelete (UpdateEntry entry) add del = H.text $
      case Tuple entry.updateEntryOld entry.updateEntryNew of
        Tuple Nothing (Just _)  -> add
        Tuple (Just _) Nothing  -> del
        _                       -> ""
