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
import qualified Data.Text.IO as T
import Data.Maybe (fromMaybe)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Exception.Safe (MonadThrow, MonadCatch)
import SDL.Vect
import System.Random

import Options.Applicative
import Options.Generic
import Data.Semigroup ((<>))

import Animate.Preview.Config
import Animate.Preview.Effect.Clock
import Animate.Preview.Effect.Logger
import Animate.Preview.Effect.Renderer
import Animate.Preview.Wrapper.SDLInput
import Animate.Preview.Wrapper.SDLRenderer
import Animate.Preview.Manager.Input
import Animate.Preview.Resource
import Animate.Preview.Runner
import Animate.Preview.Scene.Title
import Animate.Preview.State
import Options.Applicative
import Data.Semigroup ((<>))

data Sample = Sample
  { json :: String  <?> "JSON file path with sprite data"
  , sheet :: (Maybe String) <?> "Force sprite sheet path"
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

instance Clock AnimatePreview where
  delayMilliseconds = liftIO . delayMilliseconds'

instance Logger AnimatePreview where
  logText = liftIO . T.putStrLn

instance SDLRenderer AnimatePreview where
  drawTexture = drawTexture'
  presentRenderer = presentRenderer'
  clearRenderer = clearRenderer'
  queryTexture = queryTexture'

instance SDLInput AnimatePreview where
  pollEventPayloads = pollEventPayloads'

instance HasInput AnimatePreview where
  updateInput = updateInput'
  getInput = getInput'
  setInput = setInput'

instance Renderer AnimatePreview where
  clearScreen = clearScreen'
  drawScreen = drawScreen'
  getDinoAnimations = getSpriteAnimations (rDinoSprites . cResources)
  drawDino = drawSprite (rDinoSprites . cResources)
  drawBackground = drawBackground'

instance Title AnimatePreview where
  titleStep = titleStep'
