{-# LANGUAGE TemplateHaskell #-}
module Animate.Preview.Runner where

import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.State (MonadState(..), modify, gets)
import Control.Monad.Reader (MonadReader(..), asks)
import KeyState

import Animate.Preview.Config
import Animate.Preview.Clock
import Animate.Preview.Logger
import Animate.Preview.Renderer
import Animate.Preview.Input
import Animate.Preview.Frame
import Animate.Preview.Scene
import Animate.Preview.ManagerInput

import Animate.Preview.State

mainLoop :: (R m, S m, Logger m, Clock m, Renderer m, HasInput m, Scene m) => m ()
mainLoop = do
  winSize <- asks cWinSize
  updateInput
  input <- getInput
  clearScreen
  drawBackground winSize
  sceneStep
  drawScreen
  delayMilliseconds frameDeltaMilliseconds
  let quit = iQuit input || ksStatus (iEscape input) == KeyStatus'Pressed
  unless quit mainLoop