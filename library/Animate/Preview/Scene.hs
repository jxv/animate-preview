module Animate.Preview.Scene where

import qualified Animate
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..), asks)
import Control.Monad.State (MonadState(..), modify, gets)
import KeyState
import Linear

import Animate.Preview.Accel
import Animate.Preview.Animation
import Animate.Preview.Config
import Animate.Preview.Renderer
import Animate.Preview.Input
import Animate.Preview.Frame
import Animate.Preview.Dino
import Animate.Preview.State
import Animate.Preview.ManagerInput
import Animate.Preview.Color
import Animate.Preview.Logger

class Monad m => Scene m where
  sceneStep :: m ()
  default sceneStep :: (R m, S m, Renderer m, HasInput m, Logger m) => m ()
  sceneStep = do
    toggleVisuals
    updateOrigin
    updateSpeed
    updateAnimation
    drawScene

updateAnimation :: (R m, S m, Renderer m, HasInput m) => m ()
updateAnimation = do
  dinoAnimations <- getDinoAnimations
  dinoPos <- gets vDinoPos
  accel <- gets vAccel
  let accelScalar = accelAsSecondScalar accel
  let dinoPos' = Animate.stepPosition dinoAnimations dinoPos (frameDeltaSeconds * (Seconds accelScalar))
  modify $ \v -> v { vDinoPos = dinoPos' }

updateSpeed :: (S m, HasInput m, Logger m) => m ()
updateSpeed = do
  input <- getInput
  let slower = isPressed (iSlower input)
  let faster = isPressed (iFaster input)
  let change accel
        | faster && not slower = incrementAccel accel
        | slower && not faster = decrementAccel accel
        | otherwise = accel
  modify $ \v -> v { vAccel = change (vAccel v) }

updateOrigin :: (R m, S m, HasInput m, Logger m) => m ()
updateOrigin = do
  input <- getInput
  -- Recenter origin
  when (isPressed $ iCenterOrigin input) $ do
    center <- asks (sCenter . cSettings)
    modify $ \v -> v { vCenter = center }
  -- Origin by mouse click
  when (isTouched $ iMouseClick input) $ modify $ \v -> v
   { vCenter = iMousePos input }

toggleVisuals :: (S m, HasInput m) => m ()
toggleVisuals = do
  input <- getInput
  let toggleBackground = isPressed (iBackground input)
  let toggleOrigin = isPressed (iOrigin input)
  let toggleOutline = isPressed (iOutline input)
  modify $ (\v -> v
    { vBackground = (if toggleBackground then toggleMono else id) (vBackground v)
    , vOrigin = (if toggleOrigin then toggleColors else id) (vOrigin v)
    , vOutline = (if toggleOutline then toggleColors else id) (vOutline v)
    })

drawScene :: (R m, S m, Renderer m, HasInput m) => m ()
drawScene = do
  V2 x y <- gets vCenter
  dinoPos <- gets vDinoPos
  origin <- gets vOrigin
  outline <- gets vOutline
  dinoAnimations <- getDinoAnimations
  let dinoLoc = Animate.currentLocation dinoAnimations dinoPos

  drawDino outline dinoLoc (x, y)

  case origin of
    Nothing -> return ()
    Just origin' -> drawCrosshair (x,y) origin'