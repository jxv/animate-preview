{-# LANGUAGE TemplateHaskell #-}
module Animate.Preview.Scene.Title where

import qualified Animate
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..), modify, gets)
import KeyState

import Animate.Preview.Config
import Animate.Preview.Effect.Renderer
import Animate.Preview.Engine.Input
import Animate.Preview.Engine.Frame
import Animate.Preview.Engine.Dino
import Animate.Preview.Engine.Title
import Animate.Preview.Manager.Input

class Monad m => Title m where
  titleStep :: m ()

titleStep' :: (HasTitleVars s, MonadReader Config m, MonadState s m, Renderer m, HasInput m) => m ()
titleStep' = do
  input <- getInput
  updateTitle
  drawTitle

updateTitle :: (HasTitleVars s, MonadReader Config m, MonadState s m, Renderer m, HasInput m) => m ()
updateTitle = do
  dinoAnimations <- getDinoAnimations
  dinoPos <- gets (tvDinoPos . view titleVars)
  let dinoPos' = Animate.stepPosition dinoAnimations dinoPos frameDeltaSeconds

  modify $ titleVars %~ (\tv -> tv
    { tvDinoPos = dinoPos'
    })

drawTitle :: (HasTitleVars s, MonadReader Config m, MonadState s m, Renderer m, HasInput m) => m ()
drawTitle = do
  tv <- gets (view titleVars)
  dinoAnimations <- getDinoAnimations
  let dinoPos = tvDinoPos tv
  let dinoLoc = Animate.currentLocation dinoAnimations dinoPos
  drawDino dinoLoc (0, 0)
