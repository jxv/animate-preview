module Animate.Preview.Clock where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent (threadDelay)

class Monad m => Clock m where
  delayMilliseconds :: Int -> m ()
  default delayMilliseconds :: MonadIO m => Int -> m ()
  delayMilliseconds ms = liftIO $ threadDelay (1000 * ms)
