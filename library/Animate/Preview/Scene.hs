module Animate.Preview.Scene where

import qualified Animate
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..), asks)
import Control.Monad.State (MonadState(..), modify, gets)
import KeyState
import Linear

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
import Animate.Preview.Scalar

class Monad m => Scene m where
  sceneStep :: m ()
  default sceneStep :: (R m, S m, Renderer m, HasInput m, Logger m) => m ()
  sceneStep = do
    toggleVisuals
    updateOrigin
    updateSpeed
    updateScale
    updateAnimation
    drawScene

updateAnimation :: (R m, S m, Renderer m, HasInput m) => m ()
updateAnimation = do
  dinoAnimations <- getDinoAnimations
  dinoPos <- gets vDinoPos
  accel <- gets vAccel
  let accelScalar = scalarToSeconds accel
  let dinoPos' = Animate.stepPosition dinoAnimations dinoPos (frameDeltaSeconds * (Seconds accelScalar))
  modify $ \v -> v { vDinoPos = dinoPos' }

updateSpeed :: (S m, HasInput m) => m ()
updateSpeed = do
  input <- getInput
  let iup = iFaster input
  let idown = iSlower input
  let up = isPressed iup || (isHeld iup && counterGreater 20 iup)
  let down = isPressed idown || (isHeld idown && counterGreater 20 idown)
  let reset = isPressed (iAccelReset input)
  let change s
        | reset = Scalar'None
        | up && not down = incrementScalar 16 s
        | down && not up = decrementScalar 9 s
        | otherwise = s
  modify $ \v -> v { vAccel = change (vAccel v) }

counterGreater :: Int -> KeyState Int -> Bool  
counterGreater n ks = case ksCounter ks of
  Nothing -> False
  Just counter -> counter > n

updateScale :: (S m, HasInput m) => m ()
updateScale = do
  input <- getInput
  let iup = iScaleUp input
  let idown = iScaleDown input
  let up = isPressed iup || (isHeld iup && counterGreater 20 iup)
  let down = isPressed idown || (isHeld idown && counterGreater 20 idown)
  let reset = isPressed (iScaleReset input)
  let change s
        | reset = Scalar'None
        | up && not down = incrementScalar 80 s
        | down && not up = decrementScalar 9 s
        | otherwise = s
  modify $ \v -> v { vScale = change (vScale v) }

updateOrigin :: (R m, S m, HasInput m) => m ()
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
  scale <- gets vScale
  let scalar = scalarToSpriteScale scale
  drawDino outline scalar dinoLoc (x, y)
  case origin of
    Nothing -> return ()
    Just origin' -> drawCrosshair (x,y) origin'