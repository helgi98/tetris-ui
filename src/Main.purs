module Main where

import Prelude

import Affjax.Web (printError, request)
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Argonaut (printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Tetris.Api.Endpoint (Endpoint(..))
import Tetris.Api.RequestUtils (RequestMethod(..), defaultRequest, readToken)
import Tetris.AppM (runAppM)
import Tetris.Component.Router as Router
import Tetris.Data.Profile (Profile)
import Tetris.Data.Profile as Profile
import Tetris.Data.Route (routeCodec)
import Tetris.Store (BaseURL(..), LogLevel(..), Store)
import Tetris.Game as G

main :: Effect Unit
main = HA.runHalogenAff do

  body <- HA.awaitBody
  let
    baseUrl = BaseURL "https://api.realworld.io"
    logLevel = Dev

  currentUser :: Maybe Profile <- (liftEffect readToken) >>= case _ of
    Nothing ->
      pure Nothing

    Just token -> do
      let requestOptions = { endpoint: User, method: Get }
      res <- request $ defaultRequest baseUrl (Just token) requestOptions

      let
        user :: Either String Profile
        user = case res of
          Left e ->
            Left (printError e)
          Right v -> lmap printJsonDecodeError do
            u <- Codec.decode (CAR.object "User" { user: CA.json }) v.body
            CA.decode Profile.profileCodec u.user

      pure $ hush user

  scores :: Array Int <- (liftEffect G.readScores) >>= case _ of
    Nothing ->
      pure []
    Just scores ->
      pure scores
  let
    initialStore :: Store
    initialStore = { baseUrl, logLevel, currentUser, scores }

  rootComponent <- runAppM initialStore Router.component

  halogenIO <- runUI rootComponent unit body

  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) $ launchAff_ do
      _response <- halogenIO.query $ H.mkTell $ Router.Navigate new
      pure unit
