module Component.FileViewer where

import Prelude

import Control.Monad.Eff.Console (log)
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.AVar

import           Data.Either
import           Data.Array (head, replicate)
import           Data.Maybe
import qualified Data.Map as M
import           Data.Tuple
import           Data.List (fromList)
import           Data.Foldable
import           Data.Functor.Coproduct (Coproduct())
import           Data.Generic (Generic, gEq, gCompare)

import           Halogen
import           Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import qualified Component.ModuleBrowser as MB
import qualified Component.Handsontable as Hot
import qualified Component.Validation as V
import qualified Component.FileMenu as Menu

import Optic.Core
import Optic.Monad.Setter
import Optic.Refractor.Prism

import Api
import Api.Schema.Table
import Api.Schema.Module
import Api.Schema.BusinessData
import Lib.Table
import Lib.BusinessData
import Lib.Queue

import Types
import Utils

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

type FileData =
  { businessData  :: BusinessData
  , moduleId      :: ModuleId
  , lastUpdateId  :: UpdateId
  , lastSaved     :: Maybe UTCTime
  , queue         :: AVar Update
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

-- TODO: use this in slot as soon as psc #1443 is fixed
type Props =
  { moduleId :: ModuleId
  , updateId :: UpdateId
  }

data Query a
  = Init a
  | CloseFile a
  | SelectSheet S a
  | ToggleSheetConfiguratorOpen a
  | AddSheet String a
  | RenameSheet Int String a
  | DeleteSheet Int a
  | ChooseMember Int a
  | UnchooseMember Int a

type StateP = InstalledState State ChildState Query ChildQuery Metrix ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)

viewer :: ModuleId -> UpdateId -> Component StateP QueryP Metrix
viewer propModId propUpdateId = parentComponent' render eval peek
  where

    render :: RenderParent State ChildState Query ChildQuery Metrix ChildSlot
    render st = H.div
        [ cls "container"
        , P.initializer \_ -> action Init
        ]
        [ H.div [ cls "toolbar" ]
          [ H.div
            [ cls "tool-close"
            , E.onClick $ E.input_ CloseFile
            ]
            [ H.span [ cls "mega-octicon octicon-x" ] []
            ]
          , case st.fileData of
              Just fd ->
                H.slot' cpMenu MenuSlot \_ ->
                { component: Menu.fileMenu
                , initialState: Menu.initialState fd.lastUpdateId
                }
              Nothing -> H.div_ []
          , H.div [ cls "toolsep-mb-right" ] []
          , H.slot' cpModuleBrowser ModuleBrowserSlot \_ ->
            { component: MB.moduleBrowser, initialState: MB.initialState }
          , H.div [ cls "toolsep-mb-left" ] []
          , H.div [ cls "tool-sheets" ]
            [ viewSheetSelector st
            ]
          , H.div [ cls "toolsep-right" ] []
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

    eval :: EvalParent Query State ChildState Query ChildQuery Metrix ChildSlot
    eval (Init next) = do
      queue <- liftH $ liftAff' makeVar
      apiCallParent (getUpdateSnapshot propUpdateId) \(UpdateGet upd) -> do
        modify $ _{ fileData = Just
                    { businessData: applyUpdate upd.updateGetUpdate emptyBusinessData
                    , moduleId: propModId
                    , lastUpdateId: upd.updateGetId
                    , lastSaved: Just upd.updateGetCreated
                    , queue: queue
                    }
                  }
      apiCallParent (getModule propModId) \mod -> do
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
      postAgent queue
      pure next

    eval (CloseFile next) = do
      pure next

    eval (SelectSheet s next) = do
      modify $ _tableData .. _Just %~ _{ selectedSheet = s }
      rebuildHot
      pure next

    eval (ToggleSheetConfiguratorOpen next) = do
      modify $ _tableData .. _Just .. _sheetConfiguratorOpen %~ (not :: Boolean -> Boolean)
      pure next

    eval (AddSheet name next) = do
      withTable \(Table tbl) -> case tbl.tableZAxis of
        ZAxisCustom axId _ -> do
          cm <- liftH $ liftEff' $ getEntropy 32
          processEdit (NewCustomZMember axId cm name)
          rebuildHot
        _ -> pure unit
      pure next

    eval (RenameSheet index name next) = do
      withTable \(Table tbl) -> case tbl.tableZAxis of
        ZAxisCustom axId _ -> do
          processEdit (RenameCustomZMember axId index name)
        _ -> pure unit
      pure next

    eval (DeleteSheet index next) = do
      withTable \(Table tbl) -> case tbl.tableZAxis of
        ZAxisCustom axId _ -> do
          processEdit (DeleteCustomZMember axId index)
          rebuildHot
        _ -> pure unit
      pure next

    eval (ChooseMember smId next) = do
      withTable \(Table tbl) -> case tbl.tableZAxis of
        ZAxisSubset axId _ _ -> do
          processEdit (SelectSubsetZMember axId smId)
          rebuildHot
        _ -> pure unit
      pure next

    eval (UnchooseMember smId next) = do
      withTable \(Table tbl) -> case tbl.tableZAxis of
        ZAxisSubset axId _ _ -> do
          processEdit (DeselectSubsetZMember axId smId)
          rebuildHot
        _ -> pure unit
      pure next

    -- TODO not stack-safe? Try to get a `MonadRec` instance and use `forever`
    postAgent :: AVar Update -> ParentDSL State ChildState Query ChildQuery Metrix ChildSlot Unit
    postAgent queue = do
      update <- liftH $ liftAff' $ takeVar queue
      withFileData \fd -> do
        let payload = UpdatePost
              { updatePostParentId: fd.lastUpdateId
              , updatePostUpdate: update
              , updatePostValidationType: VTUpdate
              }
        let post = do
              result <- liftH $ liftAff' $ attempt $ postUpdate payload
              case result of
                Left err -> do
                  modify $ _fileData .. _Just %~ _{ lastSaved = Nothing }
                  liftH $ liftEff' $ log $ show err
                  post
                Right (UpdatePostResult res) -> case res.uprUpdateDesc of
                  UpdateDesc desc -> do
                    modify $ _fileData .. _Just %~ _{ lastUpdateId = desc.updateDescUpdateId
                                                    , lastSaved = Just desc.updateDescCreated }
                    query' cpValidation ValidationSlot $ action $ V.SetUpdateId desc.updateDescUpdateId
                    query' cpValidation ValidationSlot $ action $ V.Patch res.uprValidationResult
                    query' cpMenu MenuSlot $ action $ Menu.SetLastUpdateId desc.updateDescUpdateId
        post
        pure unit
      postAgent queue

    processEdit :: Edit -> ParentDSL State ChildState Query ChildQuery Metrix ChildSlot Unit
    processEdit edit = withFileData \fd ->
      case editToUpdate edit fd.businessData of
        Nothing -> pure unit
        Just update -> do
          liftH $ liftAff' $ putVar fd.queue update
          modify $ _fileData .. _Just .. _fdBusinessData %~ applyUpdate update

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

    peek :: Peek (ChildF ChildSlot ChildQuery) State ChildState Query ChildQuery Metrix ChildSlot
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
                  cm <- liftH $ liftEff' $ getEntropy 32
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
          modify $ _fileData .. _Just %~ _{ lastUpdateId = u.updateGetId
                                          , lastSaved = Just u.updateGetCreated
                                          }
          query' cpValidation ValidationSlot $ action $ V.SetUpdateId u.updateGetId
          query' cpMenu MenuSlot $ action $ Menu.SetLastUpdateId u.updateGetId
          rebuildHot

        Menu.OpenUpdate updateId _ ->
          apiCallParent (getUpdateSnapshot updateId) \(UpdateGet upd) -> do
            modify $ _fileData .. _Just %~
               _{ businessData = applyUpdate upd.updateGetUpdate emptyBusinessData
                , lastUpdateId = upd.updateGetId
                , lastSaved = Just upd.updateGetCreated
                }
            query' cpValidation ValidationSlot $ action $ V.SetUpdateId upd.updateGetId
            rebuildHot

        _ -> pure unit

    withTable action = do
      mTableData <- gets _.tableData
      case mTableData of
        Nothing -> pure unit
        Just tableData -> action tableData.table

    withFileData action = do
      mFileData <- gets _.fileData
      case mFileData of
        Nothing -> pure unit
        Just fileData -> action fileData

    withSheetTableBd action = do
      mTableData <- gets _.tableData
      mFileData <- gets _.fileData
      case Tuple mTableData mFileData of
        Tuple (Just td) (Just fd) ->
          action td.selectedSheet td.table fd.businessData
        _ -> pure unit

viewSheetSelector :: RenderParent State ChildState Query ChildQuery Metrix ChildSlot
viewSheetSelector st = case Tuple st.fileData st.tableData of
    Tuple (Just fd) (Just td) ->
      let
        selectSheet = SelectSheet <<< S <<< readId
        currentSheet = if doesSheetExist td.selectedSheet td.table fd.businessData
          then td.selectedSheet
          else S 0

        customZMember (Tuple s (Tuple _ name)) = H.tr_
          [ H.td_
            [ H.button
              [ E.onClick $ E.input_ $ DeleteSheet s ]
              [ H.text "-" ]
            ]
          , H.td_
            [ H.input
              [ E.onValueInput $ E.input $ RenameSheet s
              , P.value name
              ]
            ]
          ]

        subsetMember axId (SubsetMemberOption m) = H.tr_
          [ H.td_
            -- TODO where is P.style?
            -- [ H.span [ P.style $ space m.memberLevel ] []
            [ H.text m.memberLabel
            ]
          , H.td_
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
        Table tbl -> H.div_ $ case tbl.tableZAxis of
          ZAxisSingleton ->
            [ H.text "Only one sheet available." ]
          ZAxisClosed _ ords ->
            [ H.text "Sheet: "
            , H.select [ E.onValueChange $ E.input $ selectSheet ]
                       $ closedOption <$> makeIndexed ords
            ]
          ZAxisCustom axId label ->
            [ H.select [ E.onValueChange $ E.input $ selectSheet ]
                       $ customOption <$> makeIndexed (getCustomZMembers axId fd.businessData)
            , H.button
              [ E.onClick $ E.input_ ToggleSheetConfiguratorOpen ]
              [ H.text "Configure" ]
            ] <> if td.sheetConfiguratorOpen then
                [ H.div [ cls "sheet-configurator" ]
                  [ H.table_ $
                    [ H.tr_
                      [ H.td_
                        [ H.button
                          [ E.onClick $ E.input_ $ AddSheet "" ]
                          [ H.text "+" ]
                        ]
                      , H.td_ [ H.text label ]
                      ]
                    ] <> (customZMember <$> makeIndexed (getCustomZMembers axId fd.businessData))
                  ]
                ]
              else []
          ZAxisSubset axId label mems ->
            [ H.select
              [ E.onValueChange $ E.input $ selectSheet ]
              $ subsetOption mems <$> makeIndexed (getSubsetZMembers axId fd.businessData)
            , H.button
              [ E.onClick $ E.input_ ToggleSheetConfiguratorOpen ]
              [ H.text "Configure" ]
            ] <> if td.sheetConfiguratorOpen then
                [ H.div [ cls "sheet-configurator" ]
                  [ H.text $ label <> ": "
                  , H.div [ cls "subsetMemberList" ]
                    [ H.table_ $ subsetMember axId <$> mems ]
                  ]
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
