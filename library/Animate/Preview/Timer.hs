module Animate.Preview.Timer where

import qualified SDL
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)

import Animate.Preview.Frame

class Monad m => Timer m where
  startTicks :: m Int
  default startTicks :: MonadIO m => m Int
  startTicks = fromIntegral <$> liftIO SDL.ticks

  delayTicks :: Int -> m ()
  default delayTicks :: MonadIO m => Int -> m ()
  delayTicks start = liftIO $ do
    end <- fromIntegral <$> SDL.ticks
    let ms = (msps - (end - start) * fps) `div` fps
    when (ms > 0) (SDL.delay $ fromIntegral ms)
