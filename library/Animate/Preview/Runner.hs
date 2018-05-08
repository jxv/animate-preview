{-# LANGUAGE TemplateHaskell #-}
module Animate.Preview.Runner where

import qualified Animate
import qualified SDL
import qualified Data.Map as Map
import qualified Data.Vector as V
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (modify, gets)
import Control.Monad.Reader (asks)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text.Conversions (toText)
import KeyState
import System.IO.Error (catchIOError)

import Animate.Preview.Animation
import Animate.Preview.Config
import Animate.Preview.Clock
import Animate.Preview.Logger
import Animate.Preview.Renderer
import Animate.Preview.Input
import Animate.Preview.Frame
import Animate.Preview.Scene
import Animate.Preview.ManagerInput
import Animate.Preview.State
import Animate.Preview.Resource

mainLoop :: (R m, S m, Logger m, Clock m, Renderer m, HasInput m, Scene m) => m ()
mainLoop = do
  winSize <- asks cWinSize
  background <- gets vBackground
  updateInput
  input <- getInput
  clearScreen
  drawBackground winSize background
  sceneStep
  drawScreen
  delayMilliseconds frameDeltaMilliseconds
  let quit = iQuit input || ksStatus (iEscape input) == KeyStatus'Pressed
  unless quit mainLoop