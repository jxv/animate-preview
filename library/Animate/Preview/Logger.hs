module Animate.Preview.Logger where

import qualified Data.Text.IO as T
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)

class Monad m => Logger m where
  logText :: Text -> m ()
  default logText :: MonadIO m => Text -> m ()
  logText = liftIO . T.putStrLn

  logShow :: Show a => a -> m ()
  default logShow :: (MonadIO m, Show a) => a -> m ()
  logShow = liftIO . print