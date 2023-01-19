module Tetris.Api.UserAPI where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Tetris.Api.Endpoint (Endpoint(..))
import Tetris.Api.RequestUtils (RequestMethod(..), RequestOptions, Token(..), defaultRequest)
import Tetris.Data.Email (Email)
import Tetris.Data.Email as Email
import Tetris.Data.Profile (Profile)
import Tetris.Data.Profile as Profile
import Tetris.Data.Username (Username)
import Tetris.Data.Username as Username
import Tetris.Store (BaseURL)
import Affjax.Web (request)
import Affjax (printError)

type RegisterFields =
  { email :: Email
  , password :: String
  , username :: Username
  , displayName :: String
  }

registerCodec :: JsonCodec RegisterFields
registerCodec =
  CAR.object "RegisterFields"
    { email: Email.codec
    , password: CA.string
    , username: Username.codec
    , displayName: CA.string
    }

type LoginFields =
  { email :: Email
  , password :: String
  }

loginCodec :: JsonCodec LoginFields
loginCodec =
  CAR.object "LoginFields"
    { email: Email.codec
    , password: CA.string
    }

type JSONDecoder o = Json -> Either JsonDecodeError o

mkUserAPIRequest :: forall o m. MonadAff m => BaseURL -> RequestOptions -> JSONDecoder o -> m (Either String o)
mkUserAPIRequest baseUrl opts decoder = do
  res <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  case res of
    Left e -> pure $ Left $ printError e
    Right v ->
        pure $ lmap printJsonDecodeError $ decoder =<< Codec.decode CA.json v.body

login :: forall m. MonadAff m => BaseURL -> LoginFields -> m (Either String Token)
login baseUrl fields =
  let
    method = Post $ Just $ Codec.encode loginCodec fields
  in
    mkUserAPIRequest baseUrl { endpoint: Login, method } decodeToken

decodeToken :: Json -> Either JsonDecodeError Token
decodeToken body = do
  { token } <- Codec.decode tokenCodec body
  pure (Token token)
  where
    tokenCodec = CAR.object "Token" { token: CA.string }

signup :: forall m. MonadAff m => BaseURL -> RegisterFields -> m (Either String Profile)
signup baseUrl fields =
  let
    method = Post $ Just $ Codec.encode registerCodec fields
  in
    mkUserAPIRequest baseUrl { endpoint: Signup, method } (\body -> Codec.decode Profile.profileCodec body)
