module Main where

import Prelude (Unit, ($), bind, unit, pure, const)

import Control.Monad.Aff (runAff, later')
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Data.Functor.Coproduct (left)

import Halogen (action, installedState, runUI)
import Halogen.Util (appendToBody)

import Component.App as App

import Types (Effects)

main :: Eff Effects Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI App.app (installedState App.initialState)
  appendToBody app.node
  later' 100 $ app.driver $ left $ action App.Boot
