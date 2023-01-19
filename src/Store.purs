module Tetris.Store where

import Prelude

import Tetris.Data.Profile (Profile)
import Data.Maybe (Maybe(..))

data LogLevel = Dev | Prod

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

newtype BaseURL = BaseURL String

type Store =
  { logLevel :: LogLevel
  , baseUrl :: BaseURL
  , currentUser :: Maybe Profile
  , scores :: Array Int
  }

data Action
  = LoginUser Profile
  | LogoutUser
  | SetScores (Array Int)

reduce :: Store -> Action -> Store
reduce store = case _ of
  LoginUser profile ->
    store { currentUser = Just profile }

  LogoutUser ->
    store { currentUser = Nothing }

  SetScores scores ->
    store { scores = scores }
