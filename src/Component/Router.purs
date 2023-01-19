module Tetris.Component.Router where

import Prelude

import Tetris.Capability.LogMessages (class LogMessages)
import Tetris.Capability.Navigate (class Navigate, navigate)
import Tetris.Capability.Now (class Now)
import Tetris.Capability.Resource.User (class ManageUser)
import Tetris.Component.Utils (OpaqueSlot)
import Tetris.Data.Profile (Profile)
import Tetris.Data.Route (Route(..), routeCodec)
import Tetris.Page.Home as Home
import Tetris.Page.Login as Login
import Tetris.Page.Profile as Profile
import Tetris.Page.Signup as Signup
import Tetris.Store as Store
import Data.Either (hush)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Type.Proxy (Proxy(..))
import Tetris.Capability.Resource.Game (class ManageGame)

data Query a = Navigate Route a

type State =
  { route :: Maybe Route
  , currentUser :: Maybe Profile
  }

data Action
  = Initialize
  | Receive (Connected (Maybe Profile) Unit)

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , login :: OpaqueSlot Unit
  , signup :: OpaqueSlot Unit
  , profile :: OpaqueSlot Unit
  )

component
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Now m
  => LogMessages m
  => Navigate m
  => ManageUser m
  => ManageGame m
  => H.Component Query Unit Void m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState: \{ context: currentUser } -> { route: Nothing, currentUser }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      -- first we'll get the route the user landed on
      initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
      -- then we'll navigate to the new route (also setting the hash)
      navigate $ fromMaybe Home initialRoute

    Receive { context: currentUser } ->
      H.modify_ _ { currentUser = currentUser }

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route, currentUser } <- H.get
      -- don't re-render unnecessarily if the route is unchanged
      when (route /= Just dest) do
        -- don't change routes if there is a logged-in user trying to access
        -- a route only meant to be accessible to a not-logged-in session
        case (isJust currentUser && dest `elem` [ Login, Signup ]) of
          false -> H.modify_ _ { route = Just dest }
          _ -> pure unit
      pure (Just a)

  -- Display the login page instead of the expected page if there is no current user; a simple
  -- way to restrict access.
  authorize :: Maybe Profile -> H.ComponentHTML Action ChildSlots m -> H.ComponentHTML Action ChildSlots m
  authorize mbProfile html = case mbProfile of
    Nothing ->
      HH.slot (Proxy :: _ "login") unit Login.component { redirect: false } absurd
    Just _ ->
      html

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route, currentUser } = case route of
    Just r -> case r of
      Home ->
        HH.slot_ (Proxy :: _ "home") unit Home.component unit
      Login ->
        HH.slot_ (Proxy :: _ "login") unit Login.component { redirect: true }
      Signup ->
        HH.slot_ (Proxy :: _ "signup") unit Signup.component unit
      Profile username -> authorize currentUser do
        HH.slot_ (Proxy :: _ "profile") unit Profile.component { username }
    Nothing ->
      HH.div_ [ HH.text "Oh no! That page wasn't found." ]
