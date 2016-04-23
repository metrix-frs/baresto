module Component.Common where

import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen (HTML, Action)
import Prelude ((<>), ($))
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

toolButton :: forall p f. String -> String -> String -> Boolean -> Action f -> HTML p f
toolButton name icon dimClass enabled action =
  H.div (
    [ cls $ "toolbutton tooldim-" <> dimClass <> (if enabled then "" else " disabled")
    ] <> if enabled then [ E.onClick $ E.input_ action ] else []
  )
  [ H.span
    [ cls $ "icon " <> icon
    ] []
  , H.div
    [ cls "label" ]
    [ H.text name ]
  ]
