module Component.FileViewer where

import Component.FileMenu as Menu
import Component.Handsontable as Hot
import Component.ModuleBrowser as MB
import Component.Validation as V
import Data.Map as M
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Api (getUpdateSnapshot, apiCallParent, getHeader, getTable, postUpdate, getModule, getFileDetails)
import Api.Schema.BusinessData (Update, UpdateDesc(UpdateDesc), UpdateGet(UpdateGet), UpdatePost(UpdatePost), UpdatePostResult(UpdatePostResult), ValidationType(VTUpdate))
import Api.Schema.File (FileDesc(FileDesc))
import Api.Schema.Module (_templateLabel, _tableEntryCode, _tableEntryId, _templateTables, _templates, _templateGroups)
import Api.Schema.Table (Ordinate(Ordinate), SubsetMemberOption(SubsetMemberOption), Table(Table), YAxis(YAxisCustom), ZAxis(ZAxisSubset, ZAxisCustom, ZAxisClosed, ZAxisSingleton))
import Component.Common (toolButton)
import Control.Monad (unless)
import Control.Monad.Aff.Free (fromAff, fromEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Rec.Class (tailRecM)
import Data.Array (head, replicate)
import Data.Either (Either(Right, Left))
import Data.Foldable (find, foldl)
import Data.Functor.Coproduct (Coproduct)
import Data.Generic (class Generic, gEq, gCompare)
import Data.List (fromList)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.NaturalTransformation (Natural)
import Data.Tuple (Tuple(Tuple))
import Halogen (ParentHTML, ParentDSL, ChildF, Component, ParentState, gets, action, query', modify, eventSource, subscribe', lifecycleParentComponent)
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Lib.BusinessData (BusinessData, Edit(DeleteCustomRow, NewCustomRow, SetFacts, DeselectSubsetZMember, SelectSubsetZMember, DeleteCustomZMember, RenameCustomZMember, NewCustomZMember), _snapshot, getSubsetZMembers, getCustomZMembers, isSubsetZMemberSelected, doesSheetExist, emptyBusinessData, applyUpdate, sheetToZLocation, editToUpdate, getMaxSheet)
import Lib.Table (S(S))
import Optic.Core (LensP, (^.), (%~), (..), (.~), lens)
import Optic.Refractor.Prism (_Just)
import Prelude (class Ord, class Eq, Unit, ($), (<$>), show, (<>), (==), (<<<), unit, pure, bind, void, not, flip)
import Types (Metrix, TableId, UpdateId, ModuleId)
import Utils (cls, makeIndexed, readId, peek', getEntropy, minOrd)

foreign import data Queue :: * -> *

foreign import newQueue        :: forall a eff. Eff (ref :: REF | eff) (Queue a)
foreign import registerQueue   :: forall a eff. Queue a -> (a -> Eff (ref :: REF | eff) Unit) -> Eff (ref :: REF | eff) Unit
foreign import unregisterQueue :: forall a eff. Queue a -> Eff (ref :: REF | eff) Unit
foreign import pushQueue       :: forall a eff. Queue a -> a -> Eff (ref :: REF | eff) Unit
foreign import nextElemQueue   :: forall a eff. Queue a -> Eff (ref :: REF | eff) Boolean

--

data ModuleBrowserSlot = ModuleBrowserSlot

derive instance genericModuleBrowserSlot :: Generic ModuleBrowserSlot
instance eqModuleBrowserSlot :: Eq ModuleBrowserSlot where eq = gEq
instance ordModuleBrowserSlot :: Ord ModuleBrowserSlot where compare = gCompare

data HotSlot = HotSlot

derive instance genericHotSlot :: Generic HotSlot
instance eqHotSlot :: Eq HotSlot where eq = gEq
instance ordHotSlot :: Ord HotSlot where compare = gCompare

data ValidationSlot = ValidationSlot

derive instance genericValidationSlot :: Generic ValidationSlot
instance eqValidationSlot :: Eq ValidationSlot where eq = gEq
instance ordValidationSlot :: Ord ValidationSlot where compare = gCompare

data MenuSlot = MenuSlot

derive instance genericMenuSlot :: Generic MenuSlot
instance eqMenuSlot :: Eq MenuSlot where eq = gEq
instance ordMenuSlot :: Ord MenuSlot where compare = gCompare

type ChildState = Either MB.State (Either Hot.State (Either V.State Menu.State))
type ChildQuery = Coproduct MB.Query (Coproduct Hot.Query (Coproduct V.Query Menu.Query))
type ChildSlot = Either ModuleBrowserSlot (Either HotSlot (Either ValidationSlot MenuSlot))

cpModuleBrowser :: ChildPath MB.State ChildState MB.Query ChildQuery ModuleBrowserSlot ChildSlot
cpModuleBrowser = cpL

cpHot :: ChildPath Hot.State ChildState Hot.Query ChildQuery HotSlot ChildSlot
cpHot = cpR :> cpL

cpValidation :: ChildPath V.State ChildState V.Query ChildQuery ValidationSlot ChildSlot
cpValidation = cpR :> cpR :> cpL

cpMenu :: ChildPath Menu.State ChildState Menu.Query ChildQuery MenuSlot ChildSlot
cpMenu = cpR :> cpR :> cpR

--

data SaveState
  = Saved
  | Saving
  | SaveError String

type FileData =
  { businessData  :: BusinessData
  , fileDesc      :: FileDesc
  , moduleId      :: ModuleId
  , lastUpdateId  :: UpdateId
  , saveState     :: SaveState
  , queue         :: Queue Update
  }

_fdBusinessData :: LensP FileData BusinessData
_fdBusinessData = lens _.businessData _{ businessData = _ }

type TableData =
  { table                 :: Table
  , selectedSheet         :: S
  , sheetConfiguratorOpen :: Boolean
  }

_sheetConfiguratorOpen :: LensP TableData Boolean
_sheetConfiguratorOpen = lens _.sheetConfiguratorOpen _{ sheetConfiguratorOpen = _ }

type State =
  { fileData  :: Maybe FileData
  , tableData :: Maybe TableData
  }

_fileData :: LensP State (Maybe FileData)
_fileData = lens _.fileData _{ fileData = _ }

_tableData :: LensP State (Maybe TableData)
_tableData = lens _.tableData _{ tableData = _ }

initialState :: State
initialState =
  { fileData: Nothing
  , tableData: Nothing
  }

data Query a
  = Init a
  | ProcessUpdate Update a
  | CloseFile a
  | SelectSheet S a
  | ToggleSheetConfiguratorOpen a
  | AddSheet String a
  | RenameSheet Int String a
  | DeleteSheet Int a
  | ChooseMember Int a
  | UnchooseMember Int a

type StateP = ParentState State ChildState Query ChildQuery Metrix ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)

viewer :: UpdateId -> Component StateP QueryP Metrix
viewer propUpdateId = lifecycleParentComponent
  { render
  , eval: eval propUpdateId
  , peek: Just peek
  , initializer: Just (action Init)
  , finalizer: Nothing
  }

render :: State -> ParentHTML ChildState Query ChildQuery Metrix ChildSlot
render st = H.div
    [ cls "container"
    ]
    [ H.div [ cls "toolbar" ]
      [ let enabled = case st.fileData of
              Just fd -> case fd.saveState of
                Saving -> false
                Saved -> true
                SaveError _ -> false
              Nothing -> true
        in toolButton "Close" "octicon octicon-x" "close" enabled CloseFile
      , H.div [ cls "toolsep tooldim-sep-close" ] []
      , case st.fileData of
          Just fd ->
            H.slot' cpMenu MenuSlot \_ ->
            { component: Menu.fileMenu
            , initialState: Menu.initialState fd.lastUpdateId
            }
          Nothing -> H.div_ []
      , H.div [ cls "toolsep tooldim-sep-menu" ] []
      , H.slot' cpModuleBrowser ModuleBrowserSlot \_ ->
        { component: MB.moduleBrowser, initialState: MB.initialState }
      , H.div [ cls "toolsep tooldim-sep-mb" ] []
      , viewSheetSelector st
      , H.div [ cls "toolsep tooldim-sep-sheets" ] []
      , case st.fileData of
          Just fd -> case fd.fileDesc of
            FileDesc fd' ->
              H.div [ cls "tool tooldim-fileinfo" ]
              [ H.div [ cls "name" ]
                [ H.text fd'.fileDescLabel ]
              , H.div [ cls "saved" ] $ case fd.saveState of
                  Saving ->
                    [ H.span [ cls "spinner" ] []
                    , H.text "Saving and validating..." ]
                  Saved ->
                    [ H.span [ cls "octicon octicon-check" ] []
                    , H.text "All changes saved" ]
                  SaveError msg ->
                    [ H.span [ cls "octicon octicon-x" ] []
                    , H.text $ "Error saving changes: " <> msg ]
              , H.div [ cls "module" ]
                [ H.p_
                  [ H.text fd'.fileDescTaxLabel
                  ]
                , H.p_
                  [ H.text fd'.fileDescModLabel
                  ]
                ]
              ]
          Nothing -> H.div_ []
      ]
    , case st.fileData of
        Just fd -> H.slot' cpValidation ValidationSlot \_ ->
                   { component: V.validation
                   , initialState: V.initialState fd.lastUpdateId }
        Nothing -> H.div_ []
    , H.div [ cls "content" ]
      [ H.div [ cls "panel-table"]
        [ H.div [ cls "frame"]
          [ case Tuple st.fileData st.tableData of
              Tuple (Just fd) (Just td) -> if hasSheets td.table fd.businessData
                then H.slot' cpHot HotSlot \_ ->
                       { component: Hot.handsontable td.selectedSheet td.table fd.businessData
                       , initialState: Hot.initialState }
                else H.text "No sheets to display. Add member or select member for the z-Axis."
              _ -> H.div_ []
          -- , H.div [ cls "bd-debug" ]
          --   [ case st.fileData of
          --       Just fd -> debugBusinessData fd.businessData
          --       Nothing -> H.div_ []
          --   ]
          ]
        ]
      ]
    ]
  where
    hasSheets table bd = doesSheetExist (S 0) table bd

eval :: UpdateId -> Natural Query (ParentDSL State ChildState Query ChildQuery Metrix ChildSlot)
eval propUpdateId (Init next) = do
  queue <- fromEff newQueue

  apiCallParent (getFileDetails propUpdateId) \(fileDesc@(FileDesc fd)) -> do
    apiCallParent (getUpdateSnapshot propUpdateId) \(UpdateGet upd) ->
      modify $ _{ fileData = Just
                  { businessData: applyUpdate upd.updateGetUpdate emptyBusinessData
                  , fileDesc: fileDesc
                  , moduleId: fd.fileDescModId
                  , lastUpdateId: upd.updateGetId
                  , saveState: Saved
                  , queue: queue
                  }
                }
    apiCallParent (getModule fd.fileDescModId) \mod -> do
      query' cpModuleBrowser ModuleBrowserSlot $ action $ MB.Boot mod
      let mTableSelect = do
            firstGroup <- head (mod ^. _templateGroups)
            firstTempl <- head (firstGroup ^. _templates)
            firstTbl   <- head (firstTempl ^. _templateTables)
            pure { id: firstTbl ^. _tableEntryId
                 , header: false
                 , code: firstTbl ^. _tableEntryCode
                 , label: firstTempl ^. _templateLabel
                 }
      case mTableSelect of
        Just tableSelect -> do
          loadTable tableSelect.id
          query' cpModuleBrowser ModuleBrowserSlot $ action $ MB.SelectTable tableSelect
          pure unit
        Nothing -> pure unit

  subscribe' $ eventSource (registerQueue queue) (pure <<< action <<< ProcessUpdate)
  pure next

eval _ (ProcessUpdate update next) = do
  withFileData \fd -> do
    let payload = UpdatePost
          { updatePostParentId: fd.lastUpdateId
          , updatePostUpdate: update
          , updatePostValidationType: VTUpdate
          }
    flip tailRecM unit \_ -> do
      result <- fromAff $ runExceptT $ postUpdate payload
      case result of
        Left err -> do
          modify $ _fileData .. _Just %~ _{ saveState = SaveError err.title }
          fromEff do
            log $ "Error: " <> err.title
            log $ "Details: " <> err.body
          pure $ Left unit
        Right (UpdatePostResult res) -> case res.uprUpdateDesc of
          UpdateDesc desc -> do
            modify $ _fileData .. _Just %~ _{ lastUpdateId = desc.updateDescUpdateId }
            query' cpValidation ValidationSlot $ action $ V.SetUpdateId desc.updateDescUpdateId
            query' cpValidation ValidationSlot $ action $ V.Patch res.uprValidationResult
            query' cpMenu MenuSlot $ action $ Menu.SetLastUpdateId desc.updateDescUpdateId
            pure $ Right unit
    running <- fromEff $ nextElemQueue fd.queue
    unless (running) $ modify $ _fileData .. _Just %~ _{ saveState = Saved }
  pure next

eval _ (CloseFile next) = do
  withFileData \fd -> fromEff $ unregisterQueue fd.queue
  pure next

eval _ (SelectSheet s next) = do
  modify $ _tableData .. _Just %~ _{ selectedSheet = s }
  rebuildHot
  pure next

eval _ (ToggleSheetConfiguratorOpen next) = do
  modify $ _tableData .. _Just .. _sheetConfiguratorOpen %~ (not :: Boolean -> Boolean)
  pure next

eval _ (AddSheet name next) = do
  withTable \(Table tbl) -> case tbl.tableZAxis of
    ZAxisCustom axId _ -> do
      cm <- fromEff $ getEntropy 32
      processEdit (NewCustomZMember axId cm name)
      rebuildHot
    _ -> pure unit
  pure next

eval _ (RenameSheet index name next) = do
  withTable \(Table tbl) -> case tbl.tableZAxis of
    ZAxisCustom axId _ -> do
      processEdit (RenameCustomZMember axId index name)
    _ -> pure unit
  pure next

eval _ (DeleteSheet index next) = do
  withTable \(Table tbl) -> case tbl.tableZAxis of
    ZAxisCustom axId _ -> do
      processEdit (DeleteCustomZMember axId index)
      setSelectedSheetMaxNumberSheets
      rebuildHot
    _ -> pure unit
  pure next

eval _ (ChooseMember smId next) = do
  withTable \(Table tbl) -> case tbl.tableZAxis of
    ZAxisSubset axId _ _ -> do
      processEdit (SelectSubsetZMember axId smId)
      rebuildHot
    _ -> pure unit
  pure next

eval _ (UnchooseMember smId next) = do
  withTable \(Table tbl) -> case tbl.tableZAxis of
    ZAxisSubset axId _ _ -> do
      processEdit (DeselectSubsetZMember axId smId)
      setSelectedSheetMaxNumberSheets
      rebuildHot
    _ -> pure unit
  pure next

setSelectedSheetMaxNumberSheets :: ParentDSL State ChildState Query ChildQuery Metrix ChildSlot Unit
setSelectedSheetMaxNumberSheets = withFileData $ \fd -> do
  mTableData <- gets _.tableData
  case mTableData of
    Nothing -> pure unit
    Just td -> do
      let minS (S a) (S b) = S $ minOrd a b
          newS = minS (getMaxSheet td.table fd.businessData) (td.selectedSheet)
      modify $ _tableData .. _Just %~ _{ selectedSheet = newS }

processEdit :: Edit -> ParentDSL State ChildState Query ChildQuery Metrix ChildSlot Unit
processEdit edit = withFileData \fd ->
  case editToUpdate edit fd.businessData of
    Nothing -> pure unit
    Just update -> do
      modify $ _fileData .. _Just .. _fdBusinessData %~ applyUpdate update
      modify $ _fileData .. _Just %~ _{ saveState = Saving }
      fromEff $ pushQueue fd.queue update

rebuildHot :: ParentDSL State ChildState Query ChildQuery Metrix ChildSlot Unit
rebuildHot = do
  mTableData <- gets _.tableData
  case mTableData of
    Nothing -> pure unit
    Just td -> withFileData \fd -> do
      let act = Hot.Rebuild td.selectedSheet td.table fd.businessData
      void $ query' cpHot HotSlot $ action act

loadTable :: TableId -> ParentDSL State ChildState Query ChildQuery Metrix ChildSlot Unit
loadTable tableId =
  withFileData \fd ->
    apiCallParent (getTable fd.moduleId tableId) \table -> do
      modify $ _tableData .~ Just { table: table
                                  , selectedSheet: S 0
                                  , sheetConfiguratorOpen: false
                                  }
      rebuildHot

peek :: forall a. ChildF ChildSlot ChildQuery a -> ParentDSL State ChildState Query ChildQuery Metrix ChildSlot Unit
peek child = do

  peek' cpHot child \_ q -> case q of
    Hot.Edit changes _ ->
      withTable \table ->
        processEdit (SetFacts table changes)
    Hot.AddRow _ ->
      withSheetTableBd \s (table@(Table tbl)) bd -> case tbl.tableYAxis of
        YAxisCustom axId _ ->
          case sheetToZLocation s table bd of
            Nothing -> pure unit
            Just zLoc -> do
              cm <- fromEff $ getEntropy 32
              processEdit (NewCustomRow axId zLoc cm)
              rebuildHot
        _ -> pure unit
    Hot.DeleteRow index _ ->
      withSheetTableBd \s (table@(Table tbl)) bd -> case tbl.tableYAxis of
        YAxisCustom axId _ -> do
          case sheetToZLocation s table bd of
            Nothing -> pure unit
            Just zLoc -> do
              processEdit (DeleteCustomRow axId zLoc index)
              rebuildHot
        _ -> pure unit
    _ -> pure unit

  peek' cpModuleBrowser child \s q -> case q of
    MB.SelectTable tSelect _ -> if tSelect.header
      then  apiCallParent getHeader \table -> do
              modify $ _tableData .~ Just { table: table
                                          , selectedSheet: S 0
                                          , sheetConfiguratorOpen: false
                                          }
              rebuildHot
      else  loadTable tSelect.id
    _ -> pure unit

  peek' cpMenu child \s q -> case q of
    Menu.UploadCsvConfirm (UpdateGet u) _ -> do
      modify $ _fileData .. _Just .. _fdBusinessData %~ applyUpdate u.updateGetUpdate
      modify $ _fileData .. _Just %~ _{ lastUpdateId = u.updateGetId }
      query' cpValidation ValidationSlot $ action $ V.ValidateAll u.updateGetId
      query' cpMenu MenuSlot $ action $ Menu.SetLastUpdateId u.updateGetId
      rebuildHot

    Menu.OpenUpdate updateId _ ->
      apiCallParent (getUpdateSnapshot updateId) \(UpdateGet upd) -> do
        modify $ _fileData .. _Just %~
           _{ businessData = applyUpdate upd.updateGetUpdate emptyBusinessData
            , lastUpdateId = upd.updateGetId
            }
        query' cpValidation ValidationSlot $ action $ V.ValidateAll upd.updateGetId
        rebuildHot

    _ -> pure unit

withTable :: (Table -> ParentDSL State ChildState Query ChildQuery Metrix ChildSlot Unit) -> ParentDSL State ChildState Query ChildQuery Metrix ChildSlot Unit
withTable action = do
  mTableData <- gets _.tableData
  case mTableData of
    Nothing -> pure unit
    Just tableData -> action tableData.table

withFileData :: (FileData -> ParentDSL State ChildState Query ChildQuery Metrix ChildSlot Unit) -> ParentDSL State ChildState Query ChildQuery Metrix ChildSlot Unit
withFileData action = do
  mFileData <- gets _.fileData
  case mFileData of
    Nothing -> pure unit
    Just fileData -> action fileData

withSheetTableBd :: (S -> Table -> BusinessData -> ParentDSL State ChildState Query ChildQuery Metrix ChildSlot Unit) -> ParentDSL State ChildState Query ChildQuery Metrix ChildSlot Unit
withSheetTableBd action = do
  mTableData <- gets _.tableData
  mFileData <- gets _.fileData
  case Tuple mTableData mFileData of
    Tuple (Just td) (Just fd) ->
      action td.selectedSheet td.table fd.businessData
    _ -> pure unit

viewSheetSelector :: State -> ParentHTML ChildState Query ChildQuery Metrix ChildSlot
viewSheetSelector st = case Tuple st.fileData st.tableData of
    Tuple (Just fd) (Just td) ->
      let
        selectSheet = SelectSheet <<< S <<< readId
        currentSheet = if doesSheetExist td.selectedSheet td.table fd.businessData
          then td.selectedSheet
          else S 0

        customZMember (Tuple s (Tuple memId name)) = H.tr_
          [ H.td [ cls "small" ]
            [ H.button
              [ E.onClick $ E.input_ $ DeleteSheet s ]
              [ H.span [ cls "octicon octicon-dash" ] []
              ]
            ]
          , H.td_
            [ H.input
              [ E.onValueInput $ E.input $ RenameSheet s
              , P.value name
              ]
            , H.span
              [ cls "customMemberId"
              , P.title "Custom member ID" ]
              [ H.text memId ]
            ]
          ]

        subsetMember axId (SubsetMemberOption m) = H.tr_
          [ H.td_
            -- TODO where is P.style?
            -- [ H.span [ P.style $ space m.memberLevel ] []
            [ H.span
              [ cls "subsetMemberId"
              , P.title "DPM member ID"]
              [ H.text $ show m.memberId ]
            , H.text m.memberLabel
            ]
          , H.td [ cls "small" ]
            [ H.input
              [ P.inputType P.InputCheckbox
              , P.checked $ isSubsetZMemberSelected axId m.memberId fd.businessData
              , E.onChecked $ E.input \v -> case v of
                  true  -> ChooseMember m.memberId
                  false -> UnchooseMember m.memberId
              ]
            ]
          ]

        closedOption (Tuple s (Ordinate o)) =
          let selected = S s == currentSheet
              text = (foldl (<>) "" $ replicate o.ordinateLevel "--") <> o.ordinateLabel
          in  H.option [ P.selected selected
                       , P.value $ show s
                       ] [ H.text text ]

        subsetOption mems (Tuple s (Tuple memId _)) =
          let selected = S s == currentSheet
              -- TODO: may need performance optimization in the future
              memLabel = maybe "NOT FOUND" (\(SubsetMemberOption m) -> m.memberLabel)
                         $ find (\(SubsetMemberOption m) -> m.memberId == memId) mems
          in  H.option [ P.selected selected
                       , P.value $ show s
                       ] [ H.text memLabel ]

        customOption (Tuple s (Tuple memId name)) =
          H.option [ P.selected (S s == currentSheet)
                   , P.value $ show s
                   ] [ H.text name ]

      in case td.table of
        Table tbl -> case tbl.tableZAxis of
          ZAxisSingleton ->
            H.div_
            [ ]
          ZAxisClosed _ ords ->
            H.div [ cls "tool tooldim-sheets" ]
            [ H.p_
              [ H.text "Sheet:" ]
            , H.select [ E.onValueChange $ E.input $ selectSheet ]
                       $ closedOption <$> makeIndexed ords
            ]
          ZAxisCustom axId label ->
            H.div [ cls "tool tooldim-sheets" ] $
            [ H.p_
              [ H.text $ label <> ":" ]
            , H.select [ E.onValueChange $ E.input $ selectSheet ]
                       $ customOption <$> makeIndexed (getCustomZMembers axId fd.businessData)
            , toolButton "Configure" "octicon octicon-tools" "conf" true ToggleSheetConfiguratorOpen
            ] <> if td.sheetConfiguratorOpen then
                [ H.div [ cls "sheet-configurator" ]
                  [ H.table_ $
                    [ H.tr_
                      [ H.td [ cls "small" ]
                        [ H.button
                          [ E.onClick $ E.input_ $ AddSheet "" ]
                          [ H.span [ cls "octicon octicon-plus" ] []
                          ]
                        ]
                      , H.td_ [ H.text label ]
                      ]
                    ] <> (customZMember <$> makeIndexed (getCustomZMembers axId fd.businessData))
                  ]
                ]
              else []
          ZAxisSubset axId label mems ->
            H.div [ cls "tool tooldim-sheets" ] $
            [ H.p_
              [ H.text $ label <> ":" ]
            , H.select
              [ E.onValueChange $ E.input $ selectSheet ]
              $ subsetOption mems <$> makeIndexed (getSubsetZMembers axId fd.businessData)
            , toolButton "Configure" "octicon octicon-tools" "conf" true ToggleSheetConfiguratorOpen
            ] <> if td.sheetConfiguratorOpen then
                [ H.div [ cls "sheet-configurator" ]
                  [ H.table_ $ subsetMember axId <$> mems ]
                ]
              else []

    _ -> H.text ""

debugBusinessData :: BusinessData -> ParentHTML ChildState Query ChildQuery Metrix ChildSlot
debugBusinessData bd = H.div_
    [ H.table_ $ bdEntry <$> (fromList $ M.toList (bd ^. _snapshot))
    ]
  where
    bdEntry (Tuple key val) = H.tr_
      [ H.td_ [ H.text $ show key ]
      , H.td_ [ H.text val ]
      ]
