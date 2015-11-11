module Component.ModuleBrowser where

import Prelude

import qualified Data.Map as M
import           Data.Maybe
import           Data.Array hiding ((..))
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

_mod :: LensP ModuleBrowserInfo Module
_mod = lens _.mod _{ mod = _ }

_groupOpen :: LensP ModuleBrowserInfo (M.Map TemplateGroupId Boolean)
_groupOpen = lens _.groupOpen _{ groupOpen = _ }

_selectedTable :: LensP ModuleBrowserInfo (Maybe TableSelect)
_selectedTable = lens _.selectedTable _{ selectedTable = _ }

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
          let next = goRelativeModuloLen tSelect ((+) 1) (flattenTables info.mod)
              prev = goRelativeModuloLen tSelect (\i -> i - 1) (flattenTables info.mod)
          in
          [ H.p_ [ H.text tSelect.code ]
          , H.p_ [ H.text tSelect.label ]
          , H.button [ E.onClick $ E.input_ $ SelectTable prev ] [ H.text "<"]
          , H.button [ E.onClick $ E.input_ $ SelectTable next ] [ H.text ">"]
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

--

flattenTables :: Module -> Array TableSelect
flattenTables mod     = concat $ goGroup <$> (mod ^. _templateGroups)
  where goGroup g     = concat $ goTemplate <$> (g ^. _templates)
        goTemplate t  = goTable (t ^. _templateLabel) <$> (t ^. _templateTables)
        goTable lbl t =
          { id: t ^. _tableEntryId
          , code: t ^. _tableEntryCode
          , label: lbl
          }

goRelativeModuloLen :: TableSelect -> (Int -> Int) -> Array TableSelect -> TableSelect
goRelativeModuloLen x dir xs = fromMaybe x do
    i <- findIndex (\x' -> x.id == x'.id) xs
    index xs $ (dir i) `realMod` (length xs)
  where realMod a b = let r = a `mod` b in if r < 0 then r + b else r
