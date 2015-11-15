module Component.FileViewer where

import Prelude

import Control.Monad.Eff.Console (log)
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.AVar

import           Data.Either
import           Data.Array (replicate)
import           Data.Maybe
import qualified Data.Map as M
import           Data.Tuple
import           Data.List (fromList)
import           Data.Foldable
import           Data.Functor.Coproduct (Coproduct())
import           Data.Generic (Generic, gEq, gCompare)

import           Halogen
import           Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>), prjSlot, prjQuery)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import qualified Component.ModuleBrowser as MB
import qualified Component.Handsontable as Hot
import qualified Component.Validation as V

import Optic.Core
import Optic.Monad.Setter
import Optic.Refractor.Prism

import Api
import Api.Schema.Table
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

type ChildState = Either MB.State (Either Hot.State V.State)
type ChildQuery = Coproduct MB.Query (Coproduct Hot.Query V.Query)
type ChildSlot = Either ModuleBrowserSlot (Either HotSlot ValidationSlot)

cpModuleBrowser :: ChildPath MB.State ChildState MB.Query ChildQuery ModuleBrowserSlot ChildSlot
cpModuleBrowser = cpL

cpHot :: ChildPath Hot.State ChildState Hot.Query ChildQuery HotSlot ChildSlot
cpHot = cpR :> cpL

cpValidation :: ChildPath V.State ChildState V.Query ChildQuery ValidationSlot ChildSlot
cpValidation = cpR :> cpR

--

type FileData =
  { businessData  :: BusinessData
  , moduleId      :: ModuleId
  , lastUpdateId  :: UpdateId
  , lastSaved     :: UTCTime
  , queue         :: AVar Update
  }

_fdBusinessData :: LensP FileData BusinessData
_fdBusinessData = lens _.businessData _{ businessData = _ }

type TableData =
  { table         :: Table
  , selectedSheet :: S
  }

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
          , H.div [ cls "tool-menu" ]
            [ H.span [ cls "mega-octicon octicon-three-bars" ] []
            ]
          , H.div [ cls "toolsep-mb-right" ] []
          , H.slot' cpModuleBrowser ModuleBrowserSlot \_ ->
            { component: MB.moduleBrowser, initialState: MB.initialState }
          , H.div [ cls "toolsep-mb-left" ] []
          , H.div [ cls "tool-sheets" ]
            [ viewSheetSelector st
            ]
          , H.div [ cls "toolsep-right" ] []
          ]
        , H.slot' cpValidation ValidationSlot \_ ->
            { component: V.validation propModId
            , initialState: V.initialState }
        , H.div [ cls "content" ]
          [ H.div [ cls "panel-table"]
            [ H.div [ cls "frame"]
              [ case Tuple st.fileData st.tableData of
                  Tuple (Just fd) (Just td) -> if hasSheets td.table fd.businessData
                    then H.slot' cpHot HotSlot \_ ->
                           { component: Hot.handsontable td.selectedSheet td.table fd.businessData
                           , initialState: Hot.initialState }
                    else H.text "No sheets to display. Add member or select member for the z-Axis."
                  _ -> H.p [ cls "msg" ] [ H.text "Please select a table from the list on the right..." ]
              ]
            ]
          ]
        , case st.fileData of
            Just fd -> debugBusinessData fd.businessData
            Nothing -> H.div_ []
        ]
      where
        hasSheets table bd = doesSheetExist table bd (S 0)

    eval :: EvalParent Query State ChildState Query ChildQuery Metrix ChildSlot
    eval (Init next) = do
      query' cpModuleBrowser ModuleBrowserSlot $ action $ MB.Boot propModId
      apiCallParent (getUpdateSnapshot propUpdateId) \(UpdateGet upd) -> do
        queue <- liftH $ liftAff' makeVar
        modify $ _{ fileData = Just
                    { businessData: applyUpdate upd.updateGetUpdate emptyBusinessData
                    , moduleId: propModId
                    , lastUpdateId: upd.updateGetId
                    , lastSaved: upd.updateGetCreated
                    , queue: queue
                    }
                  }
        postAgent queue
      pure next

    eval (CloseFile next) = do
      pure next

    eval (SelectSheet s next) = do
      modify $ _tableData .. _Just %~ _{ selectedSheet = s }
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
    postAgent queue = withFileData \fd -> do
      update <- liftH $ liftAff' $ takeVar queue
      let payload = UpdatePost
            { updatePostParentId: fd.lastUpdateId
            , updatePostUpdate: update
            }
      let post = do
            result <- liftH $ liftAff' $ attempt $ postUpdate payload
            case result of
              Left err -> do
                modify $ _fileData .. _Just %~ _{ lastSaved = "error" }
                post
              Right (UpdateConfirmation conf) -> do
                modify $ _fileData .. _Just %~ _{ lastUpdateId = conf.updateConfUpdateId
                                                , lastSaved = conf.updateConfCreated }
      post
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

    peek :: Peek (ChildF ChildSlot ChildQuery) State ChildState Query ChildQuery Metrix ChildSlot
    peek child = do
      peek' cpHot child \s q -> case q of
        Hot.Edit changes _ ->
          withTable \table ->
            processEdit (SetFacts table changes)
        Hot.AddRow _ ->
          withTable \(Table tbl) -> case tbl.tableYAxis of
            YAxisCustom axId _ -> do
              cm <- liftH $ liftEff' $ getEntropy 32
              processEdit (NewCustomYOrdinate axId cm)
              rebuildHot
            _ -> pure unit
        Hot.DeleteRow index _ ->
          withTable \(Table tbl) -> case tbl.tableYAxis of
            YAxisCustom axId _ -> do
              processEdit (DeleteCustomYOrdinate axId index)
              rebuildHot
            _ -> pure unit
        _ -> pure unit
      peek' cpModuleBrowser child \s q -> case q of
        MB.SelectTable tSelect _ ->
          withFileData \fd ->
            apiCallParent (getTable fd.moduleId tSelect.id) \table -> do
              modify $ _tableData .~ Just { table: table
                                          , selectedSheet: S 0 }
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

viewSheetSelector :: RenderParent State ChildState Query ChildQuery Metrix ChildSlot
viewSheetSelector st = case Tuple st.fileData st.tableData of
    Tuple (Just fd) (Just td) ->
      let
        selectSheet i = SelectSheet $ S $ readId i
        currentSheet = if doesSheetExist td.table fd.businessData td.selectedSheet
          then td.selectedSheet
          else S 0

        customZMember axId (Tuple s (Tuple memId name)) = H.tr_
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
          , H.input
            [ P.inputType P.InputRadio
            , P.checked (S s == currentSheet)
            , E.onChecked $ E.input_ $ selectSheet $ show s
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
              , P.checked $ isSubsetMemberSelected axId m.memberId fd.businessData
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
      in case td.table of
        Table tbl -> H.div_ $ case tbl.tableZAxis of
          ZAxisSingleton ->
            []
          ZAxisClosed _ ords ->
            [ H.text "Sheet: "
            , H.select [ E.onValueChange $ E.input $ selectSheet ]
                       $ closedOption <$> makeIndexed ords
            ]
          ZAxisCustom axId label ->
            [ H.table_ $
              [ H.tr_
                [ H.td_
                  [ H.button
                    [ E.onClick $ E.input_ $ AddSheet "" ]
                    [ H.text "+" ]
                  ]
                , H.td [ P.colSpan 2 ] [ H.text label ]
                ]
              ] <> (customZMember axId <$> makeIndexed (getCustomMembers axId fd.businessData))
            ]
          ZAxisSubset axId label mems ->
            [ H.text $ label <> ": "
            , H.div [ cls "subsetMemberList" ]
              [ H.table_ $ subsetMember axId <$> mems ]
            , H.select
              [ E.onValueChange $ E.input $ selectSheet ]
              $ subsetOption mems <$> makeIndexed (getSubsetMembers axId fd.businessData)
            ]

    _ -> H.text "loading..."

debugBusinessData :: BusinessData -> ParentHTML ChildState Query ChildQuery Metrix ChildSlot
debugBusinessData bd = H.div_
    [ H.table_ $ bdEntry <$> (fromList $ M.toList (bd ^. _snapshot))
    , H.table_ []
    ]
  where
    bdEntry (Tuple key val) = H.tr_
      [ H.td_ [ H.text $ show key ]
      , H.td_ [ H.text val ]
      ]

-- TODO: purescript-halogen PR
peek' :: forall s s' s'' f f' f'' g p p' a
       . ChildPath s'' s' f'' f' p' p
      -> ChildF p f' a
      -> (p' -> f'' a -> ParentDSL s s' f f' g p Unit)
      -> ParentDSL s s' f f' g p Unit
peek' cp (ChildF s q) action = case Tuple (prjSlot cp s) (prjQuery cp q) of
  Tuple (Just s') (Just q') -> action s' q'
  _ -> pure unit
