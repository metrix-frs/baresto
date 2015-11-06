module Component.ModuleBrowser where

import Prelude

import qualified Data.Map as M
import           Data.Maybe
import           Data.Foldable

import Control.Monad.State (execState)

import Optic.Core
import Optic.At
import Optic.Refractor.Prism
import Optic.Monad.Setter
import Optic.Iso

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Types
import Utils

import Api
import Api.Schema.Module

type TableSelect =
  { id :: TableId
  , code :: String
  , label :: String
  }

type ModuleBrowserInfo =
  { mod :: Module
  , open :: Boolean
  , groupOpen :: M.Map TemplateGroupId Boolean
  , selectedTable :: Maybe TableSelect
  }

type State = Maybe ModuleBrowserInfo

_groupOpen :: LensP ModuleBrowserInfo (M.Map TemplateGroupId Boolean)
_groupOpen = lens _.groupOpen _{ groupOpen = _ }

_mod :: LensP ModuleBrowserInfo Module
_mod = lens _.mod _{ mod = _ }

initialState :: State
initialState = Nothing

data Query a
  = Boot ModuleId a
  | SelectTable TableSelect a
  | ToggleGroupOpen TemplateGroupId a
  | ToggleOpen a

moduleBrowser :: Component State Query Metrix
moduleBrowser = component render eval
  where

    render :: Render State Query
    render st = H.div
      [ cls "moduleBrowser"
      ]
      [ case st of
          Nothing -> H.text "module not loaded"
          Just mbInfo -> renderModuleBrowser mbInfo
      ]

    eval :: Eval Query State Query Metrix
    eval (Boot modId next) = do
      apiCall (getModule modId) \mod -> do
        modify $ const $ Just
          { mod: mod
          , open: true
          , groupOpen: (M.empty :: M.Map TemplateGroupId Boolean)
          , selectedTable: Nothing
          }
      pure next
    eval (SelectTable tSelect next) = do
      modify $ map _{ selectedTable = Just tSelect }
      pure next
    eval (ToggleGroupOpen gId next) = do
      modify $ _Just .. _groupOpen .. at gId .. non true %~ (not :: Boolean -> Boolean)
      pure next
    eval (ToggleOpen next) = do
      modify $ map \info -> info { open = not info.open }
      pure next

renderModuleBrowser :: ModuleBrowserInfo -> ComponentHTML Query
renderModuleBrowser info = H.div_
    [ H.text (info.mod ^. _moduleLabel)
    , H.div_ $ case info.selectedTable of
        Nothing ->
          [ H.p_ [ H.text "no table selected" ] ]
        Just tSelect ->
          [ H.p_ [ H.text tSelect.code ]
          , H.p_ [ H.text tSelect.label ]
          ]
    , H.ul [ cls "group" ] $ if info.open
        then renderTemplateGroup <$> (info.mod ^. _templateGroups)
        else []
    ]
  where
    renderTemplateGroup :: TemplateGroup -> ComponentHTML Query
    renderTemplateGroup g = H.li_
        [ H.div [ cls "row" ]
          [ H.div [ cls $ if open then "open" else "closed" ] []
          , H.span
            [ cls "label"
            , E.onClick $ E.input_ $ ToggleGroupOpen gId
            ]
            [ H.text (g ^. _templateGroupLabel) ]
          ]
        , H.ul [ cls "template" ]
          $ if open then renderTemplate <$> (g ^. _templates) else []
        ]
      where
        gId = g ^. _templateGroupId
        open = fromMaybe true $ M.lookup gId info.groupOpen

    renderTemplate :: Template -> ComponentHTML Query
    renderTemplate t = H.li_
        [ H.div [ cls "row" ]
          [ H.span [ cls "template-label" ]
            [ H.text (t ^. _templateLabel) ]
          ]
        , H.ul [ cls "table" ]
          $ renderTable <$> (t ^. _templateTables)
        ]
      where
        renderTable tbl = H.li_
          [ H.div [ cls "row" ]
            [ H.span
              [ cls $ if selected tbl then "tableCode selected" else "tableCode"
              , E.onClick $ E.input_ $ SelectTable { id: tbl ^. _tableEntryId
                                                   , code: tbl ^. _tableEntryCode
                                                   , label: t ^. _templateLabel
                                                   }
              ]
              [ H.text (tbl ^. _tableEntryCode) ]
            ]
          ]
        selected tbl= case info.selectedTable of
          Nothing -> false
          Just sel -> (tbl ^. _tableEntryId) == sel.id
