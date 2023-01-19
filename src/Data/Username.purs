module Tetris.Data.Username
  ( Username -- constructor not exported
  , parse
  , toString
  , codec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)

newtype Username = Username String

derive instance eqUsername :: Eq Username
derive instance ordUsername :: Ord Username
derive newtype instance showUsername :: Show Username


codec :: JsonCodec Username
codec = dimap (\(Username user) -> user) Username CA.string

parse :: String -> Maybe Username

parse "" = Nothing
parse str = Just (Username str)

toString :: Username -> String
toString (Username str) = str
