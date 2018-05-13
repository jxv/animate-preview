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
import Control.Concurrent (threadDelay, forkIO, newMVar, modifyMVar_)
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
import Animate.Preview.Watcher

data Options = Options
  { target :: String  <?> "File path with sprite information (YAML or JSON)"
  , image :: (Maybe String) <?> "Force sprite sheet's file path"
  , highDpi :: Bool <?> "Use high DPI (if available)"
  , fps :: (Maybe Int) <?> "Force frames per second (default: 60)"
  , watch :: Bool <?> "Watch target and image files. Automatically reload files when changed"
  } deriving (Show, Generic)

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

main :: IO ()
main = do
  options <- getRecord "options"
  let highDpi' = unHelpful $ highDpi options
  let fps' = max 1 (fromMaybe 60 (unHelpful $ fps options))

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
  reloadVal <- newMVar False

  winSize <- fmap fromIntegral <$> get (SDL.windowSize window)
  drawSize <- fmap fromIntegral <$> SDL.glGetDrawableSize window

  let settings = Settings
        { sTarget = unHelpful $ target options
        , sSpritesheet = unHelpful $ image options
        }

  let cfg = Config
        { cWindow = window
        , cRenderer = renderer
        , cResources = resources
        , cHighDpi = highDpi' && (winSize /= drawSize)
        , cSettings = settings
        , cCurrent = current
        , cLoaded = loaded
        , cFps = fps'
        , cFrameDeltaSeconds = 1.0 / fromIntegral fps'
        , cReload = reloadVal
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
