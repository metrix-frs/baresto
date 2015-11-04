module Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff, later')
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Data.Functor.Coproduct (left)

import Halogen
import Halogen.Util (appendToBody)

import qualified Component.App as App

import Types

main :: Eff (Effects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI App.app (installedState App.initialState)
  appendToBody app.node
  later' 100 $ app.driver $ left $ action App.Boot
