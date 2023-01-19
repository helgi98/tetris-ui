module Tetris.Data.Profile where

import Data.Codec.Argonaut (JsonCodec)
import Data.Maybe (Maybe)
import Tetris.Data.Email (Email)
import Tetris.Data.Username (Username)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Tetris.Data.Email as Email
import Tetris.Data.Username as Username

type ProfileRep row =
  ( username :: Username
  , displayName :: Maybe String
  | row
  )

type Profile = { | ProfileRep () }

type ProfileWithEmail = { | ProfileRep (email :: Email) }

type ProfileWithEmailPassword = { | ProfileRep (email :: Email, password :: Maybe String) }

profileCodec :: JsonCodec Profile
profileCodec =
  CAR.object "Profile"
    { username: Username.codec
    , displayName: CAC.maybe CA.string
    }

profileWithEmailCodec :: JsonCodec ProfileWithEmail
profileWithEmailCodec =
  CAR.object "Profile"
    { username: Username.codec
    , displayName: CAC.maybe CA.string
    , email: Email.codec
    }

profileWithEmailPasswordCodec :: JsonCodec ProfileWithEmailPassword
profileWithEmailPasswordCodec =
  CAR.object "Profile"
    { username: Username.codec
    , displayName: CAC.maybe CA.string
    , email: Email.codec
    , password: CAC.maybe CA.string
    }

