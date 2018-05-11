{-# LANGUAGE TemplateHaskell #-}
module Animate.Preview.Runner where

import Control.Monad (unless)
import Control.Monad.State (gets)
import KeyState

import Animate.Preview.Config
import Animate.Preview.Logger
import Animate.Preview.Renderer
import Animate.Preview.Input
import Animate.Preview.Scene
import Animate.Preview.ManagerInput
import Animate.Preview.State
import Animate.Preview.Timer

mainLoop :: (R m, S m, Logger m, Renderer m, HasInput m, Scene m, Timer m) => m ()
mainLoop = do
  ticks <- startTicks
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
  delayTicks ticks
  unless quit mainLoop