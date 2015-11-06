module Component.FileSelector where

import Prelude

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Api

import Types

data State = State

initialState :: State
initialState = State

data Query a
  = Init a
  | SelectFile ModuleId FileId a

selector :: Component State Query Metrix
selector = component render eval
  where

    render :: Render State Query
    render _ = H.div_
      [ H.button [ E.onClick (E.input_ $ SelectFile 35 1) ]
        [ H.text "Open mock file" ]
      ]

    eval :: Eval Query State Query Metrix
    eval (Init next) = do
      apiCall listFiles \files ->
        apiCall listFrameworks \frameworks ->
          pure unit
      pure next

    eval (SelectFile _ _ next) = do
      pure next
