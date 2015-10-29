module Types where

import Control.Monad.Aff (Aff())
import Control.Monad.Aff.AVar (AVAR())

import DOM

type Effects eff = (dom :: DOM, avar :: AVAR | eff)

type Metrix eff = Aff (Effects eff)
