module Main where

import Component.App as App
import Control.Monad.Aff (later')
import Control.Monad.Eff (Eff)
import Data.Functor.Coproduct (left)
import Halogen (action, parentState, runUI)
import Halogen.Util (awaitBody, runHalogenAff)
import Prelude
import Types (Effects)

main :: Eff Effects Unit
main = runHalogenAff do
  body <- awaitBody
  driver <- runUI App.app (parentState App.initialState) body
  later' 100 $ driver $ left $ action App.Boot
