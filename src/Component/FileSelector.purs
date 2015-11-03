module Component.FileSelector where

import Prelude

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Types

data State = State

initialState :: State
initialState = State

data Query a
  = Foo a

selector :: Component State Query Metrix
selector = component render eval
  where

    render :: Render State Query
    render _ = H.div_ []

    eval :: Eval Query State Query Metrix
    eval (Foo next) = do
      pure next
