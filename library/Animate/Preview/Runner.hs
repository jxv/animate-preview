module Animate.Preview.Runner where

import Control.Monad (unless)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets)
import KeyState
import SDL.FPS

import Animate.Preview.Config
import Animate.Preview.Logger
import Animate.Preview.Renderer
import Animate.Preview.Input
import Animate.Preview.Scene
import Animate.Preview.ManagerInput
import Animate.Preview.State

mainLoop :: (R m, S m, Logger m, Renderer m, HasInput m, Scene m, FPS m) => m ()
mainLoop = do
  ticks <- startFrame
  updateScreenInfo
  drawSize <- gets vDrawSize
  background <- gets vBackground
  updateInput
  input <- getInput
  clearScreen
  drawBackground drawSize background
  sceneStep
  drawScreen
  let quit = iQuit input || ksStatus (iEscape input) == KeyStatus'Pressed
  fps <- asks cFps
  endFrame fps ticks
  unless quit mainLoop