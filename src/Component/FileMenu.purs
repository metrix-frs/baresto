module Component.FileMenu where

import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Api (apiUrl, newTag, apiCall, uploadCsv, getUpdatePast)
import Api.Schema.BusinessData (UpdateGet, TagDesc(TagDesc), UpdateDesc(UpdateDesc), UpdateEntry(UpdateEntry), UpdateEntryHuman(HumanCustomRow, HumanCustomZ, HumanSubsetZ, HumanFact, HumanHeaderFact))
import Api.Schema.Import (CsvImportConf(CsvImportConf), Warning(Warning))
import Component.Common (modal, toolButton)
import Component.Validation.Finding (renderHoleCoords)
import Control.Monad.Aff.Free (fromEff)
import Data.Array (snoc)
import Data.Maybe (Maybe(Nothing, Just))
import Data.NaturalTransformation (Natural)
import Data.String (take)
import Data.Tuple (Tuple(Tuple))
import Halogen (ComponentDSL, ComponentHTML, Component, component, modify, get, gets)
import Optic.Core (LensP, (%~), lens)
import Prelude ((<$>), ($), show, (<>), (/=), (<), (>), pure, bind, (+), (-), unit, (==))
import Types (Metrix, UpdateId)
import Utils (cls, paginate, maxOrd, getInputFileList)

data Location
  = LocationHome
  | LocationImportCsv
  | LocationPast Int (Array UpdateDesc) -- page and updates

type State =
  { open :: Boolean
  , location :: Location
  , csvImportResponse :: Maybe CsvImportConf
  , newTagName :: String
  , lastUpdateId :: UpdateId
  }

_location :: LensP State Location
_location = lens _.location _{ location = _ }

initialState :: UpdateId -> State
initialState updateId =
  { open: false
  , location: LocationHome
  , csvImportResponse: Nothing
  , newTagName: ""
  , lastUpdateId: updateId
  }

data Query a
  = Open a
  | Close a
  | GoHome a
  | GoImportCsv a
  | GoPast a
  | UploadCsv a
  | UploadCsvConfirm UpdateGet a
  | UploadCsvClose a
  | NewTagSetName String a
  | NewTagCreate a
  | PastPagePrev a
  | PastPageNext a
  | OpenUpdate UpdateId a
  | SetLastUpdateId UpdateId a

fileMenu :: Component State Query Metrix
fileMenu = component
  { render
  , eval
  }

render :: State -> ComponentHTML Query
render st = H.div_ $
  [ toolButton "Menu" "octicon octicon-three-bars" "menu" true (if st.open then Close else Open)
  ] <> if st.open
         then [ renderMenu st ]
         else []

eval :: Natural Query (ComponentDSL State Query Metrix)
eval (Open next) = do
  modify $ _{ open = true }
  pure next

eval (Close next) = do
  modify $ _{ open = false
            , location = LocationHome
            }
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
    modify $ _{ location = LocationPast 1 past }
  pure next

eval (UploadCsv next) = do
  mFiles <- fromEff $ getInputFileList "csvFile"
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
            LocationPast page past -> do
              let go (UpdateDesc upd) = UpdateDesc $
                    if upd.updateDescUpdateId == st.lastUpdateId
                      then upd { updateDescTags = snoc upd.updateDescTags tag }
                      else upd
              modify $ _{ location = LocationPast page (go <$> past) }
              modify $ _{ newTagName = "" }
            _ -> pure unit
    else pure unit
  pure next

eval (PastPagePrev next) = do
  modify $ _location %~ \l -> case l of
    LocationPast p past -> LocationPast (maxOrd 1 (p - 1)) past
    LocationHome        -> LocationHome
    LocationImportCsv   -> LocationImportCsv
  pure next

eval (PastPageNext next) = do
  modify $ _location %~ \l -> case l of
    LocationPast p past -> LocationPast (p + 1) past
    LocationHome        -> LocationHome
    LocationImportCsv   -> LocationImportCsv
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

renderMenu :: State -> ComponentHTML Query
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
    LocationPast page past ->
      let pagination = paginate 100 past page in
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
        , H.div
          [ cls "pagination" ]
          [ if page > 1 then
              H.div
              [ cls "left octicon octicon-chevron-left"
              , E.onClick $ E.input_ PastPagePrev
              ] []
            else
              H.div
              [ cls "left octicon octicon-chevron-left disabled" ] []
          , H.div
            [ cls "fromto" ]
            [ H.b_ [ H.text $ show pagination.from ]
            , H.text " to "
            , H.b_ [ H.text $ show pagination.to ]
            , H.text " out of "
            , H.b_ [ H.text $ show pagination.total ]
            ]
          , if page < pagination.pages then
              H.div
              [ cls "right octicon octicon-chevron-right"
              , E.onClick $ E.input_ PastPageNext
              ] []
            else
              H.div
              [ cls "right octicon octicon-chevron-right disabled" ] []
          ]
        , H.ul [ cls "updates" ] $ renderUpdate <$> pagination.items
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
