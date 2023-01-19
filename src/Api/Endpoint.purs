module Tetris.Api.Endpoint where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', prefix, root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Endpoint
  = Login
  | Signup
  | User
  | CreateGame

derive instance genericEndpoint :: Generic Endpoint _

-- https://github.com/natefaubion/purescript-routing-duplex/tree/v0.2.0
endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ prefix "api" $ sum
  { "Login": "user" / "login" / noArgs
  , "Signup": "user" / "signup" / noArgs
  , "User": "user" / "info" / noArgs
  , "CreateGame": "game" / "create" / noArgs
  }
