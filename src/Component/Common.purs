module Component.Common where

import Prelude

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Utils (cls)

modal :: forall p f. String -> Array (HTML p f)
      -> Array (HTML p f) -> HTML p f
modal title body controls =
  H.div [ cls "modalContainer" ]
  [ H.div [ cls "modalFade" ] []
  , H.div [ cls "modal" ] $
    [ H.h1_ [ H.text title ]
    ] <> body <>
    [ H.div [ cls "controls" ] controls
    ]
  ]
