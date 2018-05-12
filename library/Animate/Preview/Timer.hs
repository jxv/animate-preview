module Animate.Preview.Timer where

import qualified SDL
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)
import Control.Monad.Reader (asks)

import Animate.Preview.Config

class Monad m => Timer m where
  startTicks :: m Int
  default startTicks :: MonadIO m => m Int
  startTicks = fromIntegral <$> liftIO SDL.ticks

  delayTicks :: Int -> m ()
  default delayTicks :: (MonadIO m, R m) => Int -> m ()
  delayTicks start = do
    fps <- asks cFps
    liftIO $ do
      end <- fromIntegral <$> SDL.ticks
      let ms = (msps - (end -start) * fps) `div` fps
      when (ms > 0) (SDL.delay $ fromIntegral ms)
    where
      msps = 1000
