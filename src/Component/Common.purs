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
  H.div [ cls "modal-container" ]
  [ H.div [ cls "modal" ] $
    [ H.h1_ [ H.text title ]
    , H.div [ cls "modal-body" ] body
    , H.div [ cls "modal-controls" ] controls
    ]
  ]

toolButton :: forall p f. String -> String -> String -> Action f -> HTML p f
toolButton name icon dimClass action =
  H.div
  [ cls $ "toolbutton tooldim-" <> dimClass
  , E.onClick $ E.input_ action ]
  [ H.span
    [ cls $ "icon " <> icon
    ] []
  , H.div
    [ cls "label" ]
    [ H.text name ]
  ]
