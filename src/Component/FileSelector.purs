module Component.FileSelector where

import Prelude

import Control.Monad
import Control.Monad.Writer

import           Data.Array hiding ((..))
import           Data.List (fromList)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Foldable
import           Data.Tuple
import           Data.Functor.Coproduct (Coproduct())
import           Data.Generic (Generic, gEq, gCompare)

import Optic.Core
import Optic.At
import Optic.Refractor.Prism
import Optic.Refractor.Lens
import Optic.Monad.Setter
import Optic.Iso

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import qualified Component.File as F
import           Component.Common (modal)

import Api
import Api.Schema
import Api.Schema.Selector
import Api.Schema.File
import Api.Schema.Import

import Types
import Utils

data FileSlot = FileSlot FileId

derive instance genericFileSlot :: Generic FileSlot
instance eqFileSlot :: Eq FileSlot where eq = gEq
instance ordFileSlot :: Ord FileSlot where compare = gCompare

data SelectedNode
  = SelectedNone
  | SelectedFramework FrameworkId
  | SelectedTaxonomy TaxonomyId
  | SelectedConceptualModule TaxonomyId ConceptualModuleId
  | SelectedModule ModuleId

type StateInfo =
  { files :: Array File
  , frameworks :: Array Framework
  , openFramework :: M.Map FrameworkId Boolean
  , openTaxonomy :: M.Map TaxonomyId Boolean
  , openConceptualModule :: M.Map (Tuple TaxonomyId ConceptualModuleId) Boolean
  , selectedNode :: SelectedNode
  , newFileName :: String
  , xbrlImportResponse :: Maybe (ServerResponse XbrlImportConf)
  }

type State = Maybe StateInfo

_files :: LensP StateInfo (Array File)
_files = lens _.files _{ files = _ }

_openFramework :: LensP StateInfo (M.Map FrameworkId Boolean)
_openFramework = lens _.openFramework _{ openFramework = _ }

_openTaxonomy :: LensP StateInfo (M.Map TaxonomyId Boolean)
_openTaxonomy = lens _.openTaxonomy _{ openTaxonomy = _ }

_openConceptualModule :: LensP StateInfo (M.Map (Tuple TaxonomyId ConceptualModuleId) Boolean)
_openConceptualModule = lens _.openConceptualModule _{ openConceptualModule = _ }

_selectedNode :: LensP StateInfo SelectedNode
_selectedNode = lens _.selectedNode _{ selectedNode = _ }

_newFileName :: LensP StateInfo String
_newFileName = lens _.newFileName _{ newFileName = _ }

initialState :: State
initialState = Nothing

data Query a
  = Init a
  | UploadXbrl a
  | UploadXbrlCloseModal a
  | UploadXbrlOpenFile ModuleId UpdateId a
  | SetNewFileName String a
  | CreateFile ModuleId String a
  | SelectFramework FrameworkId a
  | SelectTaxonomy TaxonomyId a
  | SelectConceptualModule TaxonomyId ConceptualModuleId a
  | SelectModule ModuleId a
  | ToggleFrameworkOpen FrameworkId a
  | ToggleTaxonomyOpen TaxonomyId a
  | ToggleConceptualModuleOpen TaxonomyId ConceptualModuleId a

type StateP = InstalledState State F.State Query F.Query Metrix FileSlot
type QueryP = Coproduct Query (ChildF FileSlot F.Query)
type ComponentHTMLP = ParentHTML F.State Query F.Query Metrix FileSlot

selector :: Component StateP QueryP Metrix
selector = parentComponent' render eval peek
  where

    render :: RenderParent State F.State Query F.Query Metrix FileSlot
    render st = H.div [ cls "container" ] $
      -- TODO report halogen issue about initializer
      [ H.span [ P.initializer \_ -> action Init ] []
      , H.div [ cls "toolbar" ]
        [ H.div [ cls "tool-importxbrl" ]
          [ H.input
            [ P.inputType P.InputFile
            , P.id_ "xbrlFile"
            ]
          , H.button
            [ E.onClick $ E.input_ UploadXbrl ]
            [ H.span [ cls "octicon octicon-arrow-up" ] []
            , H.text "Import XBRL"
            ]
          ]
        , H.div [ cls "toolsep-left" ] []
        , H.div [ cls "tool-newfile" ] $ case st of
            Just st' -> case st'.selectedNode of
              SelectedModule mId ->
                [ H.input
                  [ E.onValueChange $ E.input SetNewFileName
                  , P.value st'.newFileName
                  ]
                , H.button
                  [ E.onClick $ E.input_ (CreateFile mId st'.newFileName) ]
                  [ H.text "Create" ]
                ]
              _ ->
                [ H.text "Please select a module to create a new file." ]
            Nothing -> []
        , H.div [ cls "toolsep-left" ] []
        ]
      , H.div [ cls "content" ] $ case st of
          Just st' ->
            [ renderFrameworks st'
            , renderFiles st'
            , renderXbrlImportResponse st'.xbrlImportResponse
            ]
          Nothing ->
            [ H.text ""
            ]
      ]

    eval :: EvalParent Query State F.State Query F.Query Metrix FileSlot
    eval (Init next) = do
      apiCallParent listFiles \files -> do
        apiCallParent listFrameworks \frameworks -> do
          modify $ const $ Just
            { files: files
            , frameworks: frameworks
            , openFramework: (M.empty :: M.Map FrameworkId Boolean)
            , openTaxonomy: (M.empty :: M.Map TaxonomyId Boolean)
            , openConceptualModule: (M.empty :: M.Map (Tuple TaxonomyId ConceptualModuleId) Boolean)
            , selectedNode: SelectedNone
            , newFileName: ""
            , xbrlImportResponse: Nothing
            }
          pure unit
      pure next

    eval (UploadXbrl next) = do
      mFiles <- liftH $ liftEff' $ getInputFileList "xbrlFile"
      case mFiles of
        Nothing -> pure unit
        Just files -> apiCallParent (uploadXbrl files) \resp ->
          modify $ _Just %~ _{ xbrlImportResponse = Just resp }
      pure next

    eval (UploadXbrlOpenFile _ _ next) =
      pure next

    eval (UploadXbrlCloseModal next) = do
      modify $ _Just %~ _{ xbrlImportResponse = Nothing }
      apiCallParent listFiles \files ->
        modify $ _Just .. _files .~ files
      pure next

    eval (SetNewFileName name next) = do
      modify $ _Just .. _newFileName .~ name
      pure next

    eval (CreateFile _ _ next) =
      pure next

    eval (SelectFramework f next) = do
      modify $ _Just .. _selectedNode .~ SelectedFramework f
      pure next

    eval (SelectTaxonomy t next) = do
      modify $ _Just .. _selectedNode .~ SelectedTaxonomy t
      pure next

    eval (SelectConceptualModule t c next) = do
      modify $ _Just .. _selectedNode .~ SelectedConceptualModule t c
      pure next

    eval (SelectModule m next) = do
      modify $ _Just .. _selectedNode .~ SelectedModule m
      pure next

    eval (ToggleFrameworkOpen f next) = do
      modify $ _Just .. _openFramework .. at f .. non true %~ (not :: Boolean -> Boolean)
      pure next

    eval (ToggleTaxonomyOpen t next) = do
      modify $ _Just .. _openTaxonomy .. at t .. non true %~ (not :: Boolean -> Boolean)
      pure next

    eval (ToggleConceptualModuleOpen t c next) = do
      modify $ _Just .. _openConceptualModule .. at (Tuple t c) .. non true %~ (not :: Boolean -> Boolean)
      pure next

    peek :: Peek (ChildF FileSlot F.Query) State F.State Query F.Query Metrix FileSlot
    peek (ChildF (FileSlot fileId) q) = case q of
      F.DeleteFileYes _ ->
        apiCallParent (deleteFile fileId) \_ -> do
          modify $ _Just .. _files %~ filter (\(File f) -> f.fileId /= fileId)
          pure unit
      _ -> pure unit

renderXbrlImportResponse :: Maybe (ServerResponse XbrlImportConf) -> ComponentHTMLP
renderXbrlImportResponse resp = case resp of
    Nothing -> H.div_ []
    Just (ServerSuccess (XbrlImportConf conf)) -> modal "Import XBRL"
      [ H.p_ [ H.text "XBRL file successfully imported!" ]
      , H.h2_ [ H.text "Warnings:" ]
      , H.ul_ $ warning <$> conf.warnings
      ]
      [ H.button
        [ E.onClick $ E.input_ UploadXbrlCloseModal ]
        [ H.text "Close" ]
      , H.button
        [ E.onClick $ E.input_ (UploadXbrlOpenFile conf.moduleId conf.updateId) ]
        [ H.text "Open File" ]
      ]
    Just (ServerError err) -> modal err.title
      [ H.p_ [ H.text err.body ] ]
      [ H.button
        [ E.onClick $ E.input_ UploadXbrlCloseModal ]
        [ H.text "Close" ]
      ]
  where
    warning (Warning w) = H.li_
      [ H.b_ [ H.text "Message: " ]
      , H.text w.message
      , H.br_
      , H.b_ [ H.text "Context: " ]
      , H.text w.context
      ]

renderFrameworks :: StateInfo -> ComponentHTMLP
renderFrameworks st = H.div [ cls "panel-frameworklist" ]
    [ H.div [ cls "frame" ]
      [ H.ul [ cls "frameworks" ] $ concat $ renderFramework <$> st.frameworks
      ]
    ]
  where
    renderFramework :: Framework -> Array ComponentHTMLP
    renderFramework (Framework f) =
        [ H.li
          [ cls $ "framework" <> if selected then " selected" else "" ]
          [ H.span
            [ cls $ "octicon octicon-chevron-" <> if open then "down" else "right"
            , E.onClick $ E.input_ (ToggleFrameworkOpen f.frameworkId)
            ] []
          , H.span
            [ cls "label"
            , E.onClick $ E.input_ (SelectFramework f.frameworkId)
            ]
            [ H.text f.frameworkLabel
            ]
          ]
        ] <> if open then concat $ renderTaxonomy <$> f.taxonomies else []
      where
        open = fromMaybe true $ M.lookup f.frameworkId st.openFramework
        selected = case st.selectedNode of
          SelectedFramework fId -> fId == f.frameworkId
          _                     -> false

    renderTaxonomy :: Taxonomy -> Array ComponentHTMLP
    renderTaxonomy (Taxonomy t) =
        [ H.li
          [ cls $ "taxonomy" <> if selected then " selected" else "" ]
          [ H.span
            [ cls $ "octicon octicon-chevron-" <> if open then "down" else "right"
            , E.onClick $ E.input_ (ToggleTaxonomyOpen t.taxonomyId)
            ] []
          , H.span
            [ cls "label"
            , E.onClick $ E.input_ (SelectTaxonomy t.taxonomyId)
            ]
            [ H.text t.taxonomyLabel
            ]
          ]
        ] <> if open then concat $ renderConceptualModule t.taxonomyId <$> t.conceptualModules else []
      where
        open = fromMaybe true $ M.lookup t.taxonomyId st.openTaxonomy
        selected = case st.selectedNode of
          SelectedTaxonomy tId -> tId == t.taxonomyId
          _                    -> false

    renderConceptualModule :: TaxonomyId -> ConceptualModule -> Array ComponentHTMLP
    renderConceptualModule tId (ConceptualModule c) =
        [ H.li
          [ cls $ "conceptualModule" <> if selected then " selected" else "" ]
          [ H.span
            [ cls $ "octicon octicon-chevron-" <> if open then "down" else "right"
            , E.onClick $ E.input_ (ToggleConceptualModuleOpen tId c.conceptId)
            ] []
          , H.span
            [ cls "label"
            , E.onClick $ E.input_ (SelectConceptualModule tId c.conceptId)
            ]
            [ H.text c.conceptLabel
            ]
          ]
        ] <> if open then renderModuleEntry <$> c.moduleEntries else []
      where
        open = fromMaybe true $ M.lookup (Tuple tId c.conceptId) st.openConceptualModule
        selected = case st.selectedNode of
          SelectedConceptualModule tId' cId -> tId' == tId && cId == c.conceptId
          _                                 -> false

    renderModuleEntry :: ModuleEntry -> ComponentHTMLP
    renderModuleEntry (ModuleEntry m) = H.li
        [ cls $ "module" <> if selected then " selected" else "" ]
        [ H.span
          [ cls $ "octicon octicon-package"
          ] []
        , H.span
          [ cls "label"
          , E.onClick $ E.input_ (SelectModule m.moduleEntryId)
          ]
          [ H.text m.moduleEntryLabel
          ]
        ]
      where
        selected = case st.selectedNode of
          SelectedModule mId -> mId == m.moduleEntryId
          _                  -> false

renderFiles :: StateInfo -> ComponentHTMLP
renderFiles st = H.div [ cls "panel-filelist" ]
    [ H.div [ cls "frame" ]
      [ H.ul [ cls "files" ] $ concat $ mod <$> arrangeFiles st
      ]
    ]
  where
    mod (Tuple (ModuleEntry m) files) =
      [ H.li [ cls "module" ]
        [ H.span
          [ cls $ "octicon octicon-package"
          ] []
        , H.text $ m.moduleEntryLabel
        ]
      ] <> (renderFile <$> files)
    renderFile file@(File f) = H.slot (FileSlot f.fileId) \_ ->
      { component: F.file
      , initialState: F.initialState file
      }

--

getModules :: StateInfo -> Array ModuleEntry
getModules st = execWriter $
  for_ st.frameworks \(Framework f) ->
    for_ f.taxonomies \(Taxonomy t) ->
      for_ t.conceptualModules \(ConceptualModule c) ->
        for_ c.moduleEntries \(m@(ModuleEntry m')) -> case st.selectedNode of
          SelectedNone ->
            tell [m]
          SelectedFramework fId ->
            when (fId == f.frameworkId) $ tell [m]
          SelectedTaxonomy tId ->
            when (tId == t.taxonomyId) $ tell [m]
          SelectedConceptualModule tId cId ->
            when (tId == t.taxonomyId && cId == c.conceptId) $ tell [m]
          SelectedModule mId ->
            when (mId == m'.moduleEntryId) $ tell [m]

arrangeFiles :: StateInfo -> Array (Tuple ModuleEntry (Array File))
arrangeFiles st = pruneEmpty <<< fromList <<< M.values <<< sortFiles <<< makeMap <<< getModules $ st
  where
    makeMap = foldr (\(mod@(ModuleEntry m)) -> M.insert m.moduleEntryId (Tuple mod [])) M.empty

    sortFiles modEntries = foldl go modEntries st.files
    go m file@(File f) = m # at' f.fileModuleId .. _Just .. _2 %~ flip snoc file

    pruneEmpty = filter (\(Tuple _ files) -> length files /= 0)

    at' :: forall k v. (Ord k) => k -> LensP (M.Map k v) (Maybe v)
    at' k = at k
