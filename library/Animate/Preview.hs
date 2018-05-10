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
import Control.Exception.Safe (MonadThrow, MonadCatch)
import Data.Maybe (fromMaybe)
import Options.Generic
import SDL.Vect

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
  { target :: String  <?> "file path with sprite data (JSON)"
  , image :: (Maybe String) <?> "Force sprite sheet image path"
  , width :: (Maybe Int) <?> "Window width (Minimum is 100 pixels)"
  , height :: (Maybe Int) <?> "Window height (Minimum is 100 pixels)"
  , scale :: (Maybe Float) <?> "Scale the sprite size"
  , highDpi :: Bool <?> "Use high DPI"
  } deriving (Show, Generic)

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

main :: IO ()
main = do
  options <- getRecord "options"
  let windowWidth = max 100 $ fromMaybe 640 (unHelpful (width options))
  let windowHeight = max 100 $ fromMaybe 480 (unHelpful (height options))
  let windowSize =  V2 windowWidth windowHeight
  let highDpi' = unHelpful $ highDpi options

  SDL.initialize [SDL.InitVideo]
  Font.initialize
  window <- SDL.createWindow "Animate Preview" SDL.defaultWindow { SDL.windowInitialSize = fromIntegral <$> windowSize, SDL.windowHighDPI = highDpi' }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  resources <- loadResources highDpi' renderer

  windowSize' <- fmap fromIntegral <$> SDL.glGetDrawableSize window
  let windowCenter = div <$> windowSize' <*> 2

  let settings = Settings
        { sJSON = unHelpful $ target options
        , sSpritesheet = unHelpful $ image options
        , sScale = fromMaybe 1 (unHelpful $ scale options)
        , sCenter = windowCenter
        }

  let cfg = Config
        { cWindow = window
        , cRenderer = renderer
        , cResources = resources
        , cWinSize = windowSize'
        , cOrgWinSize = windowSize
        , cHighDpi = highDpi'
        , cSettings = settings }
  runAnimatePreview cfg (initVars windowCenter) (reload >> mainLoop)
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
