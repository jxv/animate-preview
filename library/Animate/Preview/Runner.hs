{-# LANGUAGE TemplateHaskell #-}
module Animate.Preview.Runner where

import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.State (MonadState(..), modify, gets)
import Control.Monad.Reader (MonadReader(..), asks)
import KeyState

import Animate.Preview.Config
import Animate.Preview.Effect.Clock
import Animate.Preview.Effect.Logger
import Animate.Preview.Effect.Renderer
import Animate.Preview.Engine.Input
import Animate.Preview.Engine.Frame
import Animate.Preview.Engine.Title
import Animate.Preview.Manager.Input
import Animate.Preview.Scene.Title

import Animate.Preview.State

mainLoop ::
  ( MonadReader Config m
  , MonadState Vars m
  , Logger m
  , Clock m
  , Renderer m
  , HasInput m
  , Title m
  ) => m ()
mainLoop = do
  winSize <- asks cWinSize
  updateInput
  input <- getInput
  clearScreen
  drawBackground winSize
  titleStep
  drawScreen
  delayMilliseconds frameDeltaMilliseconds
  let quit = iQuit input || ksStatus (iEscape input) == KeyStatus'Pressed
  unless quit mainLoop