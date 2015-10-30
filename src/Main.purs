module Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Halogen
import Halogen.Util (appendToBody)

import Component.App (app, initialState)

import Types

main :: Eff (Effects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI app (installedState initialState)
  appendToBody app.node
