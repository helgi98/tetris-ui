module Tetris.Page.Profile where

import Prelude

import Tetris.Capability.Resource.User (class ManageUser)
import Tetris.Component.HTML.Footer (footer)
import Tetris.Component.HTML.Header (header)
import Tetris.Component.HTML.Utils (css, maybeElem, safeHref, whenElem)
import Tetris.Data.Profile (Profile)
import Tetris.Data.Route (Route(..))
import Tetris.Data.Username (Username)
import Tetris.Data.Username as Username
import Tetris.Store as Store
import Data.Lens (Traversal')
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Network.RemoteData (RemoteData(..), _Success, fromMaybe, toMaybe)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Action
  = Initialize
  | Receive (Connected (Maybe Profile) Input)

type State =
  { currentUser :: Maybe Profile
  , username :: Username
  }

type Input =
  { username :: Username
  }

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageUser m
  => H.Component q Input o m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState :: Connected (Maybe Profile) Input -> State
  initialState { context: currentUser, input: { username } } =
    { currentUser
    , username
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> pure unit
    Receive _ -> pure unit

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render state =
    HH.div_
      [ header state.currentUser (Profile state.username)
      , HH.div
          [ css "profile-page" ]
          [ userInfo
          , HH.div
              [ css "container" ]
              [ HH.div
                  [ css "row" ]
                  [ mainView ]
              ]
          ]
      , footer
      ]
    where
    userInfo =
      HH.div
        [ css "user-info" ]
        [ HH.div
            [ css "container" ]
            [ HH.div
                [ css "row" ]
                [ HH.div
                    [ css "col-xs-12 col-md-10 offset-md-1" ]
                    [ HH.h4_
                        [ HH.text $ Username.toString state.username ]
                    ]
                ]
            ]
        ]

    mainView =
      HH.div
        [ css "col-xs-12 col-md-10 offset-md-1" ]
        [ HH.div
            [ css "articles-toggle" ]
            [ HH.ul
                [ css "nav nav-pills outline-active" ]
                [
                ]
            ]
        ]