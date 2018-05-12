{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Animate.Preview
  ( main
  ) where

import qualified SDL
import qualified SDL.Font as Font
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Monad (when, forever, void)
import Control.Concurrent (threadDelay, forkIO, newMVar)
import Control.Exception.Safe (MonadThrow, MonadCatch)
import Data.Maybe (fromMaybe)
import Data.StateVar (get)
import Options.Generic
import System.FSNotify (withManager, watchDir, eventPath)
import System.FilePath (takeDirectory, takeFileName)

import Animate.Preview.Config
import Animate.Preview.Logger
import Animate.Preview.Loader
import Animate.Preview.Renderer
import Animate.Preview.SDLInput
import Animate.Preview.SDLRenderer
import Animate.Preview.ManagerInput
import Animate.Preview.Resource
import Animate.Preview.Runner
import Animate.Preview.Scene
import Animate.Preview.State
import Animate.Preview.Timer

data Options = Options
  { target :: String  <?> "file path with sprite data (YAML or JSON)"
  , image :: (Maybe String) <?> "Force sprite sheet image path"
  , scale :: (Maybe Float) <?> "Scale the sprite size"
  , highDpi :: Bool <?> "Use high DPI"
  , watch :: Bool <?> "Watch files and reload on change"
  } deriving (Show, Generic)

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

main :: IO ()
main = do
  options <- getRecord "options"
  let highDpi' = unHelpful $ highDpi options

  SDL.initialize [SDL.InitVideo]
  Font.initialize
  window <- SDL.createWindow
    "Animate Preview"
    SDL.defaultWindow
      { SDL.windowHighDPI = highDpi'
      , SDL.windowResizable = True
      }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  resources <- loadResources highDpi' renderer
  loaded <- newMVar Nothing
  current <- newMVar Nothing

  winSize <- fmap fromIntegral <$> get (SDL.windowSize window)
  drawSize <- fmap fromIntegral <$> SDL.glGetDrawableSize window

  let settings = Settings
        { sTarget = unHelpful $ target options
        , sSpritesheet = unHelpful $ image options
        , sScale = fromMaybe 1 (unHelpful $ scale options)
        }

  let cfg = Config
        { cWindow = window
        , cRenderer = renderer
        , cResources = resources
        , cHighDpi = highDpi' && (winSize /= drawSize)
        , cSettings = settings
        , cCurrent = current
        , cLoaded = loaded
        }

  when (unHelpful $ watch options) $ do
    runWatcherAndReloader cfg (sTarget settings)
    case sSpritesheet settings of
      Nothing -> return ()
      Just imgPath -> runWatcherAndReloader cfg imgPath
  
  let v = initVars winSize drawSize
  runAnimatePreview cfg v (reload >> mainLoop)
  SDL.destroyWindow window
  freeResources resources
  Font.quit
  SDL.quit

runWatcherAndReloader :: Config -> String -> IO ()
runWatcherAndReloader cfg filename = void $ forkIO $ withManager $ \mgr -> do
  -- start a watching job (in the background)
  void $ watchDir
    mgr
    (takeDirectory filename)
    (\event -> takeFileName (eventPath event) == takeFileName filename)
    (\_ -> runWatcher cfg reload)
  -- sleep forever (until interrupted)
  forever $ threadDelay 1000000

newtype AnimatePreview a = AnimatePreview (ReaderT Config (StateT Vars IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO, MonadThrow, MonadCatch)

runAnimatePreview :: Config -> Vars -> AnimatePreview a -> IO a
runAnimatePreview config v (AnimatePreview m) = evalStateT (runReaderT m config) v

instance Logger AnimatePreview
instance SDLRenderer AnimatePreview
instance SDLInput AnimatePreview
instance HasInput AnimatePreview
instance Renderer AnimatePreview
instance Scene AnimatePreview
instance Loader AnimatePreview
instance Timer AnimatePreview

newtype Watcher a = Watcher (ReaderT Config IO a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadThrow, MonadCatch)

instance Logger Watcher
instance Loader Watcher

runWatcher :: Config -> Watcher a -> IO a
runWatcher cfg (Watcher m) = runReaderT m cfg