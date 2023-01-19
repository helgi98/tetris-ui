module Tetris.Capability.LogMessages where

import Prelude

import Tetris.Capability.Now (class Now)
import Tetris.Data.Log (Log, LogReason(..), mkLog)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen (HalogenM)

class Monad m <= LogMessages m where
  logMessage :: Log -> m Unit

instance logMessagesHalogenM :: LogMessages m => LogMessages (HalogenM st act slots msg m) where
  logMessage = lift <<< logMessage

log :: forall m. LogMessages m => Now m => LogReason -> String -> m Unit
log reason = logMessage <=< mkLog reason

logDebug :: forall m. LogMessages m => Now m => String -> m Unit
logDebug = log Debug

logInfo :: forall m. LogMessages m => Now m => String -> m Unit
logInfo = log Info

logWarn :: forall m. LogMessages m => Now m => String -> m Unit
logWarn = log Warn

logError :: forall m. LogMessages m => Now m => String -> m Unit
logError = log Error