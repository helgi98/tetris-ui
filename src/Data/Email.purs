module Tetris.Data.Email where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)

newtype Email = Email String

derive instance newtypeEmail :: Newtype Email _
derive instance eqEmail :: Eq Email
derive instance ordEmail :: Ord Email

codec :: JsonCodec Email
codec = wrapIso Email CA.string
