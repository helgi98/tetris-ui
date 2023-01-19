module Tetris.Api.GameAPI where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Tetris.Api.Endpoint (Endpoint(..))
import Tetris.Api.RequestUtils (RequestMethod(..), mkAuthRequest, decode)
import Tetris.Store (Action, Store)
import Halogen.Store.Monad (class MonadStore)
import Data.Codec.Argonaut as CA
import Tetris.Capability.LogMessages (class LogMessages)
import Tetris.Capability.Now (class Now)
import Data.Maybe (Maybe(..))

createGame :: forall m. MonadAff m
  => MonadStore Action Store m
  => Now m
  => LogMessages m
  => m (Maybe String)
createGame = do
  let
    request =
      { endpoint: CreateGame
      , method: Post Nothing
      }
  mbSessionId <- mkAuthRequest request
  decode CA.string mbSessionId