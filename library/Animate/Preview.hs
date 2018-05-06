{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Animate.Preview
  ( main
  ) where

import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified SDL.Font as Font
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Exception.Safe (MonadThrow, MonadCatch)
import Data.Maybe (fromMaybe)
import Options.Generic
import SDL.Vect

import Animate.Preview.Config
import Animate.Preview.Clock
import Animate.Preview.Logger
import Animate.Preview.Renderer
import Animate.Preview.SDLInput
import Animate.Preview.SDLRenderer
import Animate.Preview.ManagerInput
import Animate.Preview.Resource
import Animate.Preview.Runner
import Animate.Preview.Scene
import Animate.Preview.State

data Sample = Sample
  { json :: String  <?> "JSON file path with sprite data"
  , spritesheet :: (Maybe String) <?> "Force sprite sheet path"
  , width :: (Maybe Int) <?> "Window width (Minimum is 100 pixels)"
  , height :: (Maybe Int) <?> "Window height (Minimum is 100 pixels)"
  , scale :: (Maybe Float) <?> "Scale the sprite size"
  , alpha :: (Maybe Int) <?> "Force alpha transparency on sprite sheet"
  } deriving (Show, Generic)

instance ParseRecord Sample

main :: IO ()
main = do
  options <- getRecord "options"
  let windowWidth = max 100 $ fromMaybe 640 (unHelpful (width options))
  let windowHeight = max 100 $ fromMaybe 480 (unHelpful (height options))
  let windowSize =  V2 windowWidth windowHeight
  SDL.initialize [SDL.InitVideo, SDL.InitAudio]
  Font.initialize
  Mixer.openAudio Mixer.defaultAudio 256
  window <- SDL.createWindow "Animate Preview" SDL.defaultWindow { SDL.windowInitialSize = fromIntegral <$> windowSize }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  resources <- loadResources renderer
  let cfg = Config
        { cWindow = window
        , cRenderer = renderer
        , cResources = resources
        , cWinSize = windowSize }
  runAnimatePreview cfg initVars mainLoop
  SDL.destroyWindow window
  freeResources resources
  Mixer.closeAudio
  Mixer.quit
  Font.quit
  SDL.quit

newtype AnimatePreview a = AnimatePreview (ReaderT Config (StateT Vars IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO, MonadThrow, MonadCatch)

runAnimatePreview :: Config -> Vars -> AnimatePreview a -> IO a
runAnimatePreview config v (AnimatePreview m) = evalStateT (runReaderT m config) v

instance Clock AnimatePreview
instance Logger AnimatePreview
instance SDLRenderer AnimatePreview
instance SDLInput AnimatePreview
instance HasInput AnimatePreview
instance Renderer AnimatePreview
instance Scene AnimatePreview
