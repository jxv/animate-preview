module Animate.Preview.Watcher where

import Control.Exception.Safe (MonadThrow, MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Animate.Preview.Config
import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO, modifyMVar_)
import System.FSNotify (withManager, watchDir, eventPath)
import System.FilePath (takeDirectory, takeFileName)

import Animate.Preview.Loader
import Animate.Preview.Logger

newtype Watcher a = Watcher (ReaderT Config IO a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadThrow, MonadCatch)

instance Logger Watcher
instance Loader Watcher

runWatcher :: Config -> Watcher a -> IO a
runWatcher cfg (Watcher m) = runReaderT m cfg

runWatcherAndReloader :: Config -> String -> IO ()
runWatcherAndReloader cfg filename = void $ forkIO $ withManager $ \mgr -> do
  -- start a watching job (in the background)
  void $ watchDir
    mgr
    (takeDirectory filename)
    (\event -> takeFileName (eventPath event) == takeFileName filename)
    (\_ -> modifyMVar_ (cReload cfg) $ \_ -> return True)
  -- sleep forever (until interrupted)
  forever $ threadDelay 1000000