module Tetris.AppM where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT, updateStore)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Safe.Coerce (coerce)
import Tetris.Api.Endpoint (Endpoint(..))
import Tetris.Api.RequestUtils (RequestMethod(..), decode, mkAuthRequest, removeToken, writeToken)
import Tetris.Api.UserAPI as UserAPI
import Tetris.Capability.LogMessages (class LogMessages, logError)
import Tetris.Capability.Navigate (class Navigate, navigate)
import Tetris.Capability.Now (class Now)
import Tetris.Capability.Resource.User (class ManageUser)
import Tetris.Data.Log as Log
import Tetris.Data.Profile as Profile
import Tetris.Data.Route as Route
import Tetris.Store (Action(..), LogLevel(..), Store)
import Tetris.Store as Store
import Tetris.Api.GameAPI as GameAPI
import Tetris.Capability.Resource.Game (class ManageGame)

newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

runAppM :: forall q i o. Store.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore Action Store AppM

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime


instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    { logLevel } <- getStore
    liftEffect case logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

instance navigateAppM :: Navigate AppM where
  navigate =
    liftEffect <<< setHash <<< print Route.routeCodec

  logout = do
    liftEffect $ removeToken
    updateStore LogoutUser
    navigate Route.Home

instance manageUserAppM :: ManageUser AppM where
  loginUser fields = do
   { baseUrl } <- getStore
   UserAPI.login baseUrl fields >>= case _ of
     Left err -> logError err *> pure Nothing
     Right token -> do
       liftEffect do
         writeToken token
       mbJson <- mkAuthRequest { endpoint: User, method: Get }
       decode Profile.profileWithEmailCodec mbJson

  registerUser =  \fields -> do
    { baseUrl } <- getStore
    UserAPI.signup baseUrl fields >>= case _ of
      Left err -> logError err *> pure Nothing
      Right profile -> do
        updateStore $ LoginUser profile
        pure (Just profile)

  getCurrentUser = do
       mbJson <- mkAuthRequest { endpoint: User, method: Get }
       decode Profile.profileWithEmailCodec mbJson

instance manageGameAppM :: ManageGame AppM where
  createGame = GameAPI.createGame