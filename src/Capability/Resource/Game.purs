module Tetris.Capability.Resource.Game where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)


class Monad m <= ManageGame m where
  createGame :: m (Maybe String)

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageGameHalogenM :: ManageGame m => ManageGame (HalogenM st act slots msg m) where
  createGame = lift createGame