module Component.ModuleBrowser where

import Prelude
import Data.Map as M
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Api.Schema.Module (Module, Template, TemplateGroup, _templateGroups, _tableEntryCode, _tableEntryId, _templateTables, _templateLabel, _templates, _templateGroupLabel, _templateGroupId)
import Data.Array (length, index, findIndex, concat)
import Data.Lens (Lens', _Just, lens, (%~), (^.))
import Data.Lens.At (at)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Halogen (ComponentHTML, ComponentDSL, Component, component, modify)
import Types (TemplateGroupId, Metrix, TableId)
import Utils (cls, non, shorten)

type TableSelect =
  { id :: TableId
  , header :: Boolean
  , code :: String
  , label :: String
  }

headerSelect :: TableSelect
headerSelect =
  { id: 0
  , header: true
  , code: "Header DE"
  , label: ""
  }

type ModuleBrowserInfo =
  { mod :: Module
  , open :: Boolean
  , groupOpen :: M.Map TemplateGroupId Boolean
  , selectedTable :: Maybe TableSelect
  }

type State = Maybe ModuleBrowserInfo

_mod :: Lens' ModuleBrowserInfo Module
_mod = lens _.mod _{ mod = _ }

_groupOpen :: Lens' ModuleBrowserInfo (M.Map TemplateGroupId Boolean)
_groupOpen = lens _.groupOpen _{ groupOpen = _ }

_selectedTable :: Lens' ModuleBrowserInfo (Maybe TableSelect)
_selectedTable = lens _.selectedTable _{ selectedTable = _ }

initialState :: State
initialState = Nothing

data Query a
  = Boot Module a
  | SelectTable TableSelect a
  | ToggleGroupOpen TemplateGroupId a
  | ToggleOpen a

moduleBrowser :: Component State Query Metrix
moduleBrowser = component
  { render
  , eval
  }

render :: State -> ComponentHTML Query
render st = H.div_
  [ case st of
      Nothing -> H.text ""
      Just mbInfo -> renderModuleBrowser mbInfo
  ]

eval :: Query ~> ComponentDSL State Query Metrix
eval (Boot mod next) = do
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
  modify $ _Just <<< _groupOpen <<< at gId <<< non true %~ (not :: Boolean -> Boolean)
  pure next

eval (ToggleOpen next) = do
  modify $ map \info -> info { open = not info.open }
  pure next

renderModuleBrowser :: ModuleBrowserInfo -> ComponentHTML Query
renderModuleBrowser info = H.div
    [ cls "tooldim-mb" ] $
    [ H.div
      [ cls "module-control"
      ] case info.selectedTable of
          Nothing ->
            [ H.text "" ]
          Just tSelect ->
            let next = goRelativeModuloLen tSelect ((+) 1) (flattenTables info.mod)
                prev = goRelativeModuloLen tSelect (\i -> i - 1) (flattenTables info.mod)
            in
            [ H.div
              [ cls "toolbutton left nav-button"
              , E.onClick $ E.input_ $ SelectTable prev
              ]
              [ H.span [ cls "mega-octicon octicon-triangle-left" ] []
              ]
            , H.div
              [ cls "toolbutton current"
              , E.onClick $ E.input_ ToggleOpen
              ]
              [ H.p_ [ H.text tSelect.code ]
              ]
            , H.div
              [ cls "toolbutton right nav-button"
              , E.onClick $ E.input_ $ SelectTable next
              ]
              [ H.span [ cls "mega-octicon octicon-triangle-right" ] []
              ]
            ]
    ] <> if info.open
           then [ H.div [ cls "modules" ]
                  [ H.ul_ $
                      [renderHeader] <>
                      (concat $ renderTemplateGroup <$> (info.mod ^. _templateGroups))
                  ]
                ]
           else []
  where
    renderHeader :: ComponentHTML Query
    renderHeader = H.li [ cls $ "table" <> if selected then " selected" else "" ]
      [ H.span
        [ cls "label"
        , E.onClick $ E.input_ $ SelectTable headerSelect
        ]
        [ H.span [ cls "octicon octicon-browser" ] []
        , H.text "German Header"
        ]
      ]
      where
        selected = case info.selectedTable of
          Just sel -> sel.header
          Nothing -> false

    renderTemplateGroup :: TemplateGroup -> Array (ComponentHTML Query)
    renderTemplateGroup g =
        [ H.li [ cls "group" ]
          [ H.span
            [ cls "label"
            , E.onClick $ E.input_ $ ToggleGroupOpen gId
            ]
            [ H.span [ cls $ "octicon octicon-chevron-" <> if open then "down" else "right" ] []
            , H.text (g ^. _templateGroupLabel)
            ]
          ]
        ] <> if open then concat $ renderTemplate <$> (g ^. _templates) else []
      where
        gId = g ^. _templateGroupId
        open = fromMaybe true $ M.lookup gId info.groupOpen

    renderTemplate :: Template -> Array (ComponentHTML Query)
    renderTemplate t =
        [ H.li [ cls "template" ]
          [ H.span [ cls "octicon octicon-primitive-dot" ] []
          , case shorten (t ^. _templateLabel) 45 of
              Nothing ->
                H.text (t ^. _templateLabel)
              Just short ->
                H.a [ cls "tooltip" ]
                  [ H.span_ [ H.text (t ^. _templateLabel) ]
                  , H.text short
                  , H.b_ [ H.text "..." ]
                  ]
          ]
        ] <> (renderTable <$> (t ^. _templateTables))
      where
        renderTable tbl = H.li
          [ cls $ "table" <> if selected tbl then " selected" else "" ]
          [ H.span
            [ cls "label"
            , E.onClick $ E.input_ $ SelectTable { id: tbl ^. _tableEntryId
                                                 , header: false
                                                 , code: tbl ^. _tableEntryCode
                                                 , label: t ^. _templateLabel
                                                 }
            ]
            [ H.span [ cls "octicon octicon-browser" ] []
            , H.text (tbl ^. _tableEntryCode)
            ]
          ]
        selected tbl = case info.selectedTable of
          Just sel -> (tbl ^. _tableEntryId) == sel.id
          Nothing -> false

--

flattenTables :: Module -> Array TableSelect
flattenTables mod     = [headerSelect] <> (concat $ goGroup <$> (mod ^. _templateGroups))
  where goGroup g     = concat $ goTemplate <$> (g ^. _templates)
        goTemplate t  = goTable (t ^. _templateLabel) <$> (t ^. _templateTables)
        goTable lbl t =
          { id: t ^. _tableEntryId
          , header: false
          , code: t ^. _tableEntryCode
          , label: lbl
          }

goRelativeModuloLen :: TableSelect -> (Int -> Int) -> Array TableSelect -> TableSelect
goRelativeModuloLen x dir xs = fromMaybe x do
    i <- findIndex (\x' -> x.id == x'.id) xs
    index xs $ (dir i) `realMod` (length xs)
  where realMod a b = let r = a `mod` b in if r < 0 then r + b else r
