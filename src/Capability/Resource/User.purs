module Tetris.Capability.Resource.User where

import Prelude

import Tetris.Data.Profile (Profile, ProfileWithEmail)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Tetris.Api.UserAPI (LoginFields, RegisterFields)


class Monad m <= ManageUser m where
  loginUser :: LoginFields -> m (Maybe ProfileWithEmail)
  registerUser :: RegisterFields -> m (Maybe Profile)
  getCurrentUser :: m (Maybe ProfileWithEmail)

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM st act slots msg m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  getCurrentUser = lift getCurrentUser