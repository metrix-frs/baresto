module Component.App where

import Prelude

import Control.Plus (Plus)

import Data.Array
import Data.Tuple
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Component.Ticker (TickState(..), TickQuery(..), ticker)

data Query a
  = IncSize a

type State = { width :: Int, height :: Int }

initialState :: State
initialState = { width: 10, height: 10 }

data TickSlot = TickSlot Int Int

derive instance genericTickP :: Generic TickSlot
instance eqTickP :: Eq TickSlot where eq = gEq
instance ordTickP :: Ord TickSlot where compare = gCompare

type StateP g = InstalledState State TickState Query TickQuery g TickSlot
type QueryP = Coproduct Query (ChildF TickSlot TickQuery)

app :: forall g. (Plus g) => Component (StateP g) QueryP g
app = parentComponent' render eval (const (pure unit))

render :: forall g. (Plus g) => RenderParent State TickState Query TickQuery g TickSlot
render st = H.div_
    [ H.button
      [ E.onClick (E.input_ IncSize) ]
      [ H.text "Increase size" ]
    , H.table_ $ row <$> range 0 st.height
    ]
  where
    row y = H.tr_ $ cell y <$> range 0 st.width
    cell y x = H.td_
      [ H.slot (TickSlot x y) \_ -> { component: ticker, initialState: TickState (x + y) } ]

eval :: forall g. (Plus g) => EvalParent Query State TickState Query TickQuery g TickSlot
eval (IncSize next) = do
  modify (\r -> { width: r.width+10, height: r.height+10 })
  pure next
