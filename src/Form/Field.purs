module Tetris.Form.Field where

import Prelude

import Tetris.Component.HTML.Utils (css, maybeElem)
import Tetris.Form.Validation (FormError, errorToString)
import DOM.HTML.Indexed (HTMLinput, HTMLtextarea)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type StringField :: (Type -> Type -> Type -> Type) -> Type -> Type
type StringField f output = f String FormError output

submitButton :: forall i p. String -> HH.HTML i p
submitButton label =
  HH.input
    [ css "btn btn-lg btn-primary pull-xs-right"
    , HP.type_ HP.InputSubmit
    , HP.value label
    ]

type TextInput action output =
  { state :: F.FieldState String FormError output
  , action :: F.FieldAction action String FormError output
  }

textInput
  :: forall output action slots m
   . TextInput action output
  -> Array (HP.IProp HTMLinput action)
  -> H.ComponentHTML action slots m
textInput { state, action } props =
  HH.fieldset
    [ css "form-group" ]
    [ HH.input
        ( append
            [ css "form-control form-control-lg"
            , HP.value state.value
            , HE.onValueInput action.handleChange
            , HE.onBlur action.handleBlur
            ]
            props
        )
    , maybeElem (state.result >>= either pure (const Nothing)) \err ->
        HH.div
          [ css "error-messages" ]
          [ HH.text $ errorToString err ]
    ]