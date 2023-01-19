module Tetris.Game where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)
import Data.Codec.Argonaut as CA
import Data.Argonaut.Parser as PA
import Data.Argonaut.Core as J
import Effect.Class (liftEffect)
import Data.Tuple (Tuple)
import Data.Either (hush)

type Pos = Tuple Int Int

data TetraminoType = I | O | T | S | Z | J | L

type Piece =
  { pos :: Pos
  , kind :: TetraminoType
  , theta :: Int
  }

data GameStatus = Active | Over

type GameState =
  { grid :: Tuple Int Int
  , placedBlocks :: Array Pos
  , currentPiece :: Piece
  , next :: TetraminoType
  , held :: Maybe TetraminoType
  , canHold :: Boolean
  , status :: GameStatus
  , totalLinesCleared :: Int
  }

scoresKey = "scores" :: String

readScores :: Effect (Maybe (Array Int))
readScores = do
  strM <- getItem scoresKey =<< localStorage =<< window
  pure $ (hush <<< CA.decode (CA.array CA.int)) =<< (hush <<< PA.jsonParser) =<< strM

writeScores :: Array Int -> Effect Unit
writeScores scores = do
  let scoreStrOpt = J.toString $ CA.encode (CA.array CA.int) scores
  case scoreStrOpt of
    Nothing -> pure unit
    Just scoreStr -> liftEffect $ setItem scoresKey scoreStr =<< localStorage =<< window