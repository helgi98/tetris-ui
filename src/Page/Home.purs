-- | The Conduit homepage allows users to explore articles in several ways: in a personalized feed,
-- | by tag, or by viewing all articles.
module Tetris.Page.Home where

import Effect
import Foreign
import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foldable (for_)
import Data.Lens (Traversal')
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Monoid (guard)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Tetris.Capability.Navigate (class Navigate)
import Tetris.Component.HTML.Footer (footer)
import Tetris.Component.HTML.Header (header)
import Tetris.Component.HTML.Utils (css, maybeElem, whenElem)
import Tetris.Data.Profile (Profile)
import Tetris.Data.Route (Route(..))
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import Web.Event.EventTarget as EET
import Tetris.Component.GameField as GF
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Web.Socket.Event.MessageEvent as ME
import Tetris.Store as Store
import Web.Socket.WebSocket as WS
import Web.Socket.Event.EventTypes as WSET
import Tetris.Capability.Resource.Game (class ManageGame)
import Data.Maybe (maybe)

data Action
  = Initialize
  | Receive (Connected State Unit)
  | StartGame
  | WSMessage String

type State =
  { currentUser :: Maybe Profile,
    scores :: Array Int
  }

type ChildSlots = ( gameField :: H.Slot GF.Query Unit Unit )

gameField = (Proxy :: _ "gameField" )

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageGame m
  => Navigate m
  => H.Component q Unit o m
component = connect (selectEq (\s -> {currentUser: s.currentUser, scores: s.scores})) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState { context: { currentUser, scores } } =
    { currentUser,
      scores
    }

  startGame :: String -> m (HS.Emitter Action)
  startGame id = do
      sessionIdOpt <- createGame
      let sessionId = maybe "" sessionIdOpt
      connection <- liftEffect $ WS.create ("ws://localhost:8080/api/game/connect/" <> sessionId) []
      { emitter, listener } <- liftEffect $ HS.create
      wsListener <- liftEffect $ EET.eventListener \ev -> do
        for_ (ME.fromEvent ev) \msgEvent ->
          for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
            HS.notify listener (WSMessage msg)
      _ <- liftEffect $ EET.addEventListener
        WSET.onMessage
        wsListener
        false
        (WS.toEventTarget connection)
      pure emitter
      where
      readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
      readHelper read =
        either (const Nothing) Just <<< runExcept <<< read <<< unsafeToForeign

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> pure unit
    Receive {context: { scores } } -> do
      H.modify_ _ { scores = scores }
    StartGame -> do
      --TODO: call game api to create game
      emitter <- startGame "123"
      _ <- H.subscribe emitter
      --TODO: send game start message to server
      pure unit
    WSMessage msg -> do
      --TODO: parse and send message to child component
      pure unit
  render :: State -> H.ComponentHTML Action ChildSlots m
  -- render scores array in a table
  render { currentUser, scores } =
    HH.div_ [ (HH.slot_ gameField unit GF.component unit),
        HH.div [] [HH.text ("Scores of " <> fromMaybe "Someone"
            ((\user -> fromMaybe (show user.username) user.displayName) <$> currentUser) <> ": "),
         HH.div_ (map (\score -> HH.div_ [ HH.div_ [ HH.text $ show score ]]) scores)]
      ]
