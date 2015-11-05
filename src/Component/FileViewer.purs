module Component.FileViewer where

import Prelude

import Control.Monad.Eff.Ref

import Data.Either
import Data.Maybe
import Data.Tuple
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)

import           Halogen
import           Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>), prjSlot, prjQuery)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import qualified Component.ModuleBrowser as MB
import qualified Component.Handsontable as Hot

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

type ChildState = Either MB.State Hot.State
type ChildQuery = Coproduct MB.Query Hot.Query
type ChildSlot = Either ModuleBrowserSlot HotSlot

cpModuleBrowser :: ChildPath MB.State ChildState MB.Query ChildQuery ModuleBrowserSlot ChildSlot
cpModuleBrowser = cpL

cpHot :: ChildPath Hot.State ChildState Hot.Query ChildQuery HotSlot ChildSlot
cpHot = cpR

--

type State =
  { fileData :: Maybe
    { businessData :: BusinessData
    , fileId :: FileId
    , lastUpdateId :: UpdateId
    , lastSaved :: UTCTime
    , queueRef :: Ref (Queue Update)
    }
  , tableData :: Maybe
    { table :: Table
    , selectedSheet :: S
    }
  }

_fileData :: LensP State (Maybe _)
_fileData = lens _.fileData _{ fileData = _ }

_tableData :: LensP State (Maybe _)
_tableData = lens _.tableData _{ tableData = _ }

initialState :: State
initialState =
  { fileData: Nothing
  , tableData: Nothing
  }

data Query a
  = Boot ModuleId FileId a
  | SelectSheet S a
  | AddSheet String a
  | RenameSheet Int String a
  | DeleteSheet Int a
  | ChooseMember Int a
  | UnchooseMember Int a

type StateP = InstalledState State ChildState Query ChildQuery Metrix ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)

viewer :: Component StateP QueryP Metrix
viewer = parentComponent' render eval peek
  where

    render :: RenderParent State ChildState Query ChildQuery Metrix ChildSlot
    render st = H.div [ cls "viewer" ]
      [ H.div [ cls "vieverBar" ]
        [ H.slot' cpModuleBrowser ModuleBrowserSlot \_ ->
          { component: MB.moduleBrowser 0, initialState: MB.initialState }
        , H.div [ cls "tableTitle" ]
          [ H.h1_
            [ H.text "Title"]
          , H.p_
            [ H.text "subtitle" ]
          ]
        , H.div [ cls "sheetSelector" ]
          [ viewSheetSelector st
          ]
        , H.div [ cls "fileActions" ]
          [ H.text "File Actions"
          ]
        ]
      , H.div [ cls "viewerContent" ]
        [ H.slot' cpHot HotSlot \_ ->
          { component: Hot.handsontable, initialState: Hot.initialState }
        ]
      ]

    eval :: EvalParent Query State ChildState Query ChildQuery Metrix ChildSlot
    eval (SelectSheet s next) = do
      _tableData .. _Just %= _{ selectedSheet = s }
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

    processEdit :: Edit -> ParentDSL State ChildState Query ChildQuery Metrix ChildSlot Unit
    processEdit edit = withFileData \fd ->
        case editToUpdate edit fd.businessData of
          Nothing -> pure unit
          Just update -> do
            liftH $ liftEff' $ modifyRef fd.queueRef $ push update
            post fd.queueRef
      where
        post queueRef = withFileData \fd -> do
          mUpdate <- liftH $ liftEff' $ modifyRef' queueRef pop
          case mUpdate of
            Nothing -> pure unit
            Just update -> do
              let payload = UpdatePost
                    { updatePostParentId: fd.lastUpdateId
                    , updatePostFileId: fd.fileId
                    , updatePostUpdate: update
                    }
              apiCallParent (postUpdate payload) \(UpdateConfirmation conf) -> do
                _fileData .. _Just %= _{ lastUpdateId = conf.updateConfUpdateId
                                       , lastSaved = conf.updateConfCreated }
                post queueRef

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
      peek' cpModuleBrowser child \s q -> case q of
        MB.SelectTable tId _ ->
          -- TODO where is moduleId?
          -- apiCallParent (getTable )
          pure unit
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
viewSheetSelector st = H.div_ []

-- TODO: purescript-halogen PR
peek' :: forall s s' s'' f f' f'' g p p' a
       . ChildPath s'' s' f'' f' p' p
      -> ChildF p f' a
      -> (p' -> f'' a -> ParentDSL s s' f f' g p Unit)
      -> ParentDSL s s' f f' g p Unit
peek' cp (ChildF s q) action = case Tuple (prjSlot cp s) (prjQuery cp q) of
  Tuple (Just s') (Just q') -> action s' q'
  _ -> pure unit
