{-# LANGUAGE TemplateHaskell #-}
module Animate.Preview.Scene where

import qualified Animate
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..), modify, gets)
import KeyState

import Animate.Preview.Config
import Animate.Preview.Renderer
import Animate.Preview.Input
import Animate.Preview.Frame
import Animate.Preview.Dino
import Animate.Preview.State
import Animate.Preview.ManagerInput

class Monad m => Title m where
  titleStep :: m ()

titleStep' :: (R m, S m, Renderer m, HasInput m) => m ()
titleStep' = do
  input <- getInput
  updateTitle
  drawTitle

updateTitle :: (S m, R m, Renderer m, HasInput m) => m ()
updateTitle = do
  dinoAnimations <- getDinoAnimations
  dinoPos <- gets vDinoPos
  let dinoPos' = Animate.stepPosition dinoAnimations dinoPos frameDeltaSeconds
  modify $ (\v -> v { vDinoPos = dinoPos' })

drawTitle :: (S m, R m, Renderer m, HasInput m) => m ()
drawTitle = do
  dinoPos <- gets vDinoPos
  dinoAnimations <- getDinoAnimations
  let dinoLoc = Animate.currentLocation dinoAnimations dinoPos
  drawDino dinoLoc (0, 0)
