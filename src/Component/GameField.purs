module Tetris.Component.GameField where

import Prelude

import Data.Array ((..))
import Data.Foldable (foldl, for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Graphics.Canvas (Context2D, fillRect, getCanvasElementById, getContext2D, setFillStyle)
import Halogen (HalogenM)
import Tetris.Game (GameState, Pos)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data Query a = GameOver a |
               GameData
               { score :: Int
               , lvl :: Int
               , gs :: GameState
               } a

type Input = Unit

type Output = Unit

type Action = Unit

component
  :: forall m
   . MonadAff m
  => H.Component Query Input Output m
component = H.mkComponent
  { initialState: const unit
  , render: render
  , eval: H.mkEval $ H.defaultEval
    { handleQuery = handleQuery
    }
  }
  where
    render :: Unit -> H.ComponentHTML Action () m
    render _ = do
      HH.canvas [ HP.id "game", HP.width 640, HP.height 640 ]

    cleanField :: Context2D -> Effect Unit
    cleanField ctx = do
      setFillStyle ctx "black"
      fillRect ctx { x: 0.0, y: 0.0, height: 640.0, width: 640.0 }
      setFillStyle ctx "white"
      for_ (0 .. 17) \i -> do
        fillRect ctx { x: 0.0, y: toNumber (i * 40), height: 1.0, width: 640.0 }
      for_ (0 .. 11) \i -> do
        fillRect ctx { x: toNumber (i * 40), y: 0.0, height: 640.0, width: 1.0 }

    -- handle query to redraw canvas with filled blocks according to game state
    handleQuery :: forall a. Query a -> HalogenM Unit Action () Output m (Maybe a)
    handleQuery (GameData { score, lvl, gs } a) = do
      canvasMaybe <- liftEffect $ getCanvasElementById "game"
      case canvasMaybe of
        Nothing -> pure $ Just a
        Just canvas -> do
          ctx <- liftEffect $ getContext2D canvas
          liftEffect $ cleanField ctx
          -- draw the blocks
          let
            drawBlock :: Context2D -> Pos -> Effect Context2D
            drawBlock ctx (Tuple x y) = do
              _ <- setFillStyle ctx "black"
              _ <- fillRect ctx { x: (toNumber x * 40.0), y: (toNumber y * 40.0), height: 40.0, width: 40.0 }
              pure ctx
            -- for each block in the grid, draw it
            drawBlocks :: Context2D -> Array Pos -> Effect Context2D
            drawBlocks ctx blocks = foldl (\ctxE block -> do
              ctx <- ctxE
              drawBlock ctx block
            ) (pure ctx) blocks

          _ <- liftEffect $ drawBlocks ctx gs.placedBlocks
          pure $ Just a
    handleQuery (GameOver a) =  do
      -- clean up the canvas
      canvasMaybe <- liftEffect $ getCanvasElementById "game"
      case canvasMaybe of
        Nothing -> pure $ Just a
        Just canvas -> do
          ctx <- liftEffect $ getContext2D canvas
          liftEffect $ cleanField ctx
          pure $ Just a
