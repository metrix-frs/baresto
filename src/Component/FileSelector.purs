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

import Api
import Api.Schema.Selector

import Types
import Utils

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
  }

type State = Maybe StateInfo

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
  | OpenFile ModuleId FileId a
  | SetFileName String a
  | CreateFile ModuleId a
  | SelectFramework FrameworkId a
  | SelectTaxonomy TaxonomyId a
  | SelectConceptualModule TaxonomyId ConceptualModuleId a
  | SelectModule ModuleId a
  | ToggleFrameworkOpen FrameworkId a
  | ToggleTaxonomyOpen TaxonomyId a
  | ToggleConceptualModuleOpen TaxonomyId ConceptualModuleId a

selector :: Component State Query Metrix
selector = component render eval
  where

    render :: Render State Query
    render st = H.div_
      -- TODO report halogen issue about initializer
      [ H.span [ P.initializer \_ -> action Init ] []
      , case st of
          Just st' -> H.div_
            [ renderFrameworks st'
            , renderFiles st'
            ]
          Nothing -> H.div_
            [ H.text "loading..."
            ]
      ]

    eval :: Eval Query State Query Metrix
    eval (Init next) = do
      apiCall listFiles \files -> do
        apiCall listFrameworks \frameworks -> do
          modify $ const $ Just
            { files: files
            , frameworks: frameworks
            , openFramework: (M.empty :: M.Map FrameworkId Boolean)
            , openTaxonomy: (M.empty :: M.Map TaxonomyId Boolean)
            , openConceptualModule: (M.empty :: M.Map (Tuple TaxonomyId ConceptualModuleId) Boolean)
            , selectedNode: SelectedNone
            , newFileName: ""
            }
          pure unit
      pure next

    eval (OpenFile _ _ next) = do
      pure next

    eval (SetFileName name next) = do
      modify $ _Just .. _newFileName .~ name
      pure next

    eval (CreateFile modId next) = do
      st <- get
      case st of
        Just stInfo -> when (stInfo.newFileName /= "") do
          apiCall (newFile modId stInfo.newFileName) \resp ->
            -- TODO get fileId from server
            pure unit
        Nothing -> pure unit
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

renderFrameworks :: StateInfo -> ComponentHTML Query
renderFrameworks st = H.ul_ $ renderFramework <$> st.frameworks
  where
    renderFramework :: Framework -> ComponentHTML Query
    renderFramework (Framework f) = H.li_
        [ H.div [ cls "row" ]
          [ H.div
            [ cls $ if open then "open" else "closed"
            , E.onClick $ E.input_ (ToggleFrameworkOpen f.frameworkId)
            ] []
          , H.span
            [ cls "label"
            , E.onClick $ E.input_ (SelectFramework f.frameworkId)
            ]
            [ H.text f.frameworkLabel
            ]
          ]
        , H.ul [ cls "framework" ]
          $ if open then renderTaxonomy <$> f.taxonomies else []
        ]
      where
        open = fromMaybe true $ M.lookup f.frameworkId st.openFramework
        selected = case st.selectedNode of
          SelectedFramework fId -> fId == f.frameworkId
          _                     -> false

    renderTaxonomy :: Taxonomy -> ComponentHTML Query
    renderTaxonomy (Taxonomy t) = H.li_
        [ H.div [ cls "row" ]
          [ H.div
            [ cls $ if open then "open" else "closed"
            , E.onClick $ E.input_ (ToggleTaxonomyOpen t.taxonomyId)
            ] []
          , H.span
            [ cls "label"
            , E.onClick $ E.input_ (SelectTaxonomy t.taxonomyId)
            ]
            [ H.text t.taxonomyLabel
            ]
          ]
        , H.ul [ cls "taxonomy" ]
          $ if open then renderConceptualModule t.taxonomyId <$> t.conceptualModules else []
        ]
      where
        open = fromMaybe true $ M.lookup t.taxonomyId st.openTaxonomy
        selected = case st.selectedNode of
          SelectedTaxonomy tId -> tId == t.taxonomyId
          _                    -> false

    renderConceptualModule :: TaxonomyId -> ConceptualModule -> ComponentHTML Query
    renderConceptualModule tId (ConceptualModule c) = H.li_
        [ H.div [ cls "row" ]
          [ H.div
            [ cls $ if open then "open" else "closed"
            , E.onClick $ E.input_ (ToggleConceptualModuleOpen tId c.conceptId)
            ] []
          , H.span
            [ cls "label"
            , E.onClick $ E.input_ (SelectConceptualModule tId c.conceptId)
            ]
            [ H.text c.conceptLabel
            ]
          ]
        , H.ul [ cls "conceptualModule" ]
          $ if open then renderModuleEntry <$> c.moduleEntries else []
        ]
      where
        open = fromMaybe true $ M.lookup (Tuple tId c.conceptId) st.openConceptualModule
        selected = case st.selectedNode of
          SelectedConceptualModule tId' cId -> tId' == tId && cId == c.conceptId
          _                                 -> false

    renderModuleEntry :: ModuleEntry -> ComponentHTML Query
    renderModuleEntry (ModuleEntry m) = H.li_
        [ H.div [ cls "row"]
          [ H.span
            [ cls "label"
            , E.onClick $ E.input_ (SelectModule m.moduleEntryId)
            ]
            [ H.text m.moduleEntryLabel
            ]
          ]
        ]
      where
        selected = case st.selectedNode of
          SelectedModule mId -> mId == m.moduleEntryId
          _                  -> false

renderFiles :: StateInfo -> ComponentHTML Query
renderFiles st = H.div_
    [ H.div
      [ cls "newFile" ]
      $ case st.selectedNode of
          SelectedModule mId ->
            [ H.input
              [ E.onValueChange $ E.input SetFileName
              , P.value st.newFileName
              ]
            , H.button
              [ E.onClick $ E.input_ (CreateFile mId) ]
              [ H.text "Create" ]
            ]
          _ ->
            [ H.text "Please select a module to create a new file" ]
    , H.ul_ $ mod <$> arrangeFiles st
    ]
  where
    mod (Tuple (ModuleEntry m) files) = H.li_
      [ H.text $ m.moduleEntryLabel
      , H.ul_ $ file <$> files
      ]
    file (File f) = H.li_
      [ H.span
        [ E.onClick (E.input_ (OpenFile f.fileModuleId f.fileId)) ]
        [ H.text $ f.fileLabel <> ", created " <> f.fileCreated ]
      ]

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
