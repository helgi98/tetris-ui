module Tetris.Page.Login where

import Prelude

import Tetris.Capability.Navigate (class Navigate, navigate)
import Tetris.Capability.Resource.User (class ManageUser, loginUser)
import Tetris.Component.HTML.Header (header)
import Tetris.Component.HTML.Utils (css, safeHref, whenElem)
import Tetris.Data.Email (Email)
import Tetris.Data.Route (Route(..))
import Tetris.Form.Field as Field
import Tetris.Form.Validation (FormError)
import Tetris.Form.Validation as V
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Input = { redirect :: Boolean }

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( email :: f String FormError Email
  , password :: f String FormError String
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Receive FormContext
  | Eval FormlessAction

type State =
  { form :: FormContext
  , loginError :: Boolean
  }

component
  :: forall query output m
   . MonadAff m
  => Navigate m
  => ManageUser m
  => H.Component query Input output m
component = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState: \context -> { form: context, loginError: false }
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
    Receive context -> H.modify_ _ { form = context }
    Eval action -> F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = do
    let
      onSubmit = loginUser >=> case _ of
        Nothing ->
          H.modify_ _ { loginError = true }
        Just _ -> do
          H.modify_ _ { loginError = false }
          { redirect } <- H.gets _.form.input
          when redirect (navigate Home)

      validation =
        { email: V.required >=> V.minLength 3 >=> V.emailFormat
        , password: V.required >=> V.minLength 2 >=> V.maxLength 20
        }

    F.handleSubmitValidate onSubmit F.validate validation

  render :: State -> H.ComponentHTML Action () m
  render { loginError, form: { formActions, fields, actions } } =
    container
      [ HH.h1
          [ css "text-xs-center" ]
          [ HH.text "Sign In" ]
      , HH.p
          [ css "text-xs-center" ]
          [ HH.a
              [ safeHref Signup ]
              [ HH.text "Need an account?" ]
          ]
      , HH.form
          [ HE.onSubmit formActions.handleSubmit ]
          [ whenElem loginError \_ ->
              HH.div
                [ css "error-messages" ]
                [ HH.text "Email or password is invalid" ]
          , HH.fieldset_
              [ Field.textInput
                  { state: fields.email, action: actions.email }
                  [ HP.placeholder "Email"
                  , HP.type_ HP.InputEmail
                  ]
              , Field.textInput
                  { state: fields.password, action: actions.password }
                  [ HP.placeholder "Password"
                  , HP.type_ HP.InputPassword
                  ]
              , Field.submitButton "Log in"
              ]
          ]
      ]
    where
    container html =
      HH.div
        [ css "auth-page" ]
        [ header Nothing Login
        , HH.div
            [ css "container page" ]
            [ HH.div
                [ css "row" ]
                [ HH.div
                    [ css "col-md-6 offset-md-3 col-xs12" ]
                    html
                ]
            ]
        ]
