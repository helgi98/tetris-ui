-- | The registration form allows new users to sign up to the Conduit service and authenticate
-- | their session.
module Tetris.Page.Signup where

import Prelude

import Tetris.Form.Field (StringField)
import Tetris.Capability.Navigate (class Navigate, navigate)
import Tetris.Capability.Resource.User (class ManageUser, registerUser)
import Tetris.Component.HTML.Header (header)
import Tetris.Component.HTML.Utils (css, safeHref)
import Tetris.Data.Email (Email)
import Tetris.Data.Route (Route(..))
import Tetris.Data.Username (Username)
import Tetris.Form.Field as Field
import Tetris.Form.Validation (FormError)
import Tetris.Form.Validation as V
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | See the Formless tutorial to learn how to build your own forms:
-- | https://github.com/thomashoneyman/purescript-halogen-formless
type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( username :: StringField f Username
  , email :: StringField f Email
  , displayName :: StringField f String
  , password :: StringField f String
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Receive FormContext
  | Eval FormlessAction

component
  :: forall query output m
   . MonadAff m
  => ManageUser m
  => Navigate m
  => H.Component query Unit output m
component = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState: \context -> context
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Receive context -> H.put context
    Eval action -> F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = do
    let
      onSubmit = registerUser >=> traverse_ (\_ -> navigate Home)
      validation =
        { username: V.required >=> V.usernameFormat
        , email: V.required >=> V.minLength 3 >=> V.emailFormat
        , displayName: V.required >=> V.minLength 3
        , password: V.required >=> V.minLength 8 >=> V.maxLength 20
        }

    F.handleSubmitValidate onSubmit F.validate validation

  render :: FormContext -> H.ComponentHTML Action () m
  render { formActions, fields, actions } =
    container
      [ HH.h1
          [ css "text-xs-center" ]
          [ HH.text "Sign Up" ]
      , HH.p
          [ css "text-xs-center" ]
          [ HH.a
              [ safeHref Login ]
              [ HH.text "Already have an account?" ]
          ]
      , HH.form
          [ HE.onSubmit formActions.handleSubmit ]
          [ HH.fieldset_
              [ Field.textInput
                  { state: fields.username, action: actions.username }
                  [ HP.placeholder "Username" ]
              , Field.textInput
                  { state: fields.email, action: actions.email }
                  [ HP.placeholder "Email"
                  , HP.type_ HP.InputEmail
                  ]
              , Field.textInput
                  { state: fields.email, action: actions.email }
                  [ HP.placeholder "Display Name"
                  , HP.type_ HP.InputText
                  ]
              , Field.textInput
                  { state: fields.password, action: actions.password }
                  [ HP.placeholder "Password"
                  , HP.type_ HP.InputPassword
                  ]
              , Field.submitButton "Sign up"
              ]
          ]
      ]
    where
    container html =
      HH.div_
        [ header Nothing Signup
        , HH.div
            [ css "auth-page" ]
            [ HH.div
                [ css "container page" ]
                [ HH.div
                    [ css "row" ]
                    [ HH.div
                        [ css "col-md-6 offset-md-3 col-xs12" ]
                        html
                    ]
                ]
            ]
        ]
