module Tetris.Component.HTML.Footer where

import Tetris.Component.HTML.Utils (css)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

footer :: forall i p. HH.HTML i p
footer =
  HH.footer_
    [ HH.div
        [ css "container" ]
        [ HH.a
            [ css "logo-font"
            , HP.href "/"
            ]
            [ HH.text "conduit" ]
        , HH.span
            [ css "attribution" ]
            [ HH.text "Tetris!"
            ]
        ]
    ]
