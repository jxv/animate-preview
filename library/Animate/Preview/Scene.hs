module Animate.Preview.Scene where

import qualified Animate
import qualified Data.Vector as V
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..), asks)
import Control.Monad.State (MonadState(..), modify, gets)
import Data.Text.Conversions (toText)
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
import Animate.Preview.Mode

class Monad m => Scene m where
  sceneStep :: m ()
  default sceneStep :: (R m, S m, Renderer m, HasInput m, Logger m) => m ()
  sceneStep = do
    updateMode
    toggleVisuals
    updateOrigin
    updateSpeed
    updateScale
    updateInfo
    updateAnimation
    drawScene

updateMode :: (S m, HasInput m) => m ()
updateMode = do
  input <- getInput
  when (onceThenFire $ iMode input) $ modify $ \v -> v { vMode = if vMode v == Mode'Playback then Mode'Stepper else Mode'Playback }

updateAnimation :: (R m, S m, Renderer m, HasInput m) => m ()
updateAnimation = do
  mode <- gets vMode
  dinoAnimations <- getDinoAnimations
  dinoPos <- gets vDinoPos
  case mode of
    Mode'Stepper -> do
      input <- getInput
      when (onceThenFire $ iFaster input) $ modify $ \v -> v { vDinoPos = forceNextFrameIndex dinoAnimations dinoPos }
      when (onceThenFire $ iSlower input) $ modify $ \v -> v { vDinoPos = forcePrevFrameIndex dinoAnimations dinoPos }
    Mode'Playback -> do
      accel <- gets vAccel
      let accelScalar = scalarToSeconds accel
      let dinoPos' = Animate.stepPosition dinoAnimations dinoPos (frameDeltaSeconds * (Seconds accelScalar))
      modify $ \v -> v { vDinoPos = dinoPos' }

unsafeForceNextFrameIndex :: V.Vector (Animate.Frame loc delay) -> Animate.FrameIndex -> Animate.FrameIndex
unsafeForceNextFrameIndex frames idx = if idx + 1 >= len then 0 else idx + 1
  where len = V.length frames

unsafeForcePrevFrameIndex :: V.Vector (Animate.Frame loc delay) -> Animate.FrameIndex -> Animate.FrameIndex
unsafeForcePrevFrameIndex frames idx = if idx <= 0 then len - 1 else idx - 1
  where len = V.length frames

forceNextFrameIndex :: Enum key => Animate.Animations key loc delay -> Animate.Position key delay -> Animate.Position key delay
forceNextFrameIndex a p = p { Animate.pFrameIndex = unsafeForceNextFrameIndex frames (Animate.pFrameIndex p) }
  where
    frames = Animate.framesByAnimation a (Animate.pKey p)

forcePrevFrameIndex :: Enum key => Animate.Animations key loc delay -> Animate.Position key delay -> Animate.Position key delay
forcePrevFrameIndex a p = p { Animate.pFrameIndex = unsafeForcePrevFrameIndex frames (Animate.pFrameIndex p) }
  where
    frames = Animate.framesByAnimation a (Animate.pKey p)


updateSpeed :: (S m, HasInput m) => m ()
updateSpeed = do
  input <- getInput
  mode <- gets vMode
  case mode of
    Mode'Stepper -> return ()
    Mode'Playback ->  do
      let up = onceThenFire (iFaster input) 
      let down = onceThenFire (iSlower input)
      let reset = isPressed (iAccelReset input)
      let change s
            | reset = Scalar'None
            | up && not down = incrementScalar 18 s
            | down && not up = decrementScalar 9 s
            | otherwise = s
      modify $ \v -> v { vAccel = change (vAccel v) }

onceThenFire :: KeyState Int -> Bool
onceThenFire ks = isPressed ks || (isHeld ks && counterGreater 20 ks)

counterGreater :: Int -> KeyState Int -> Bool  
counterGreater n ks = case ksCounter ks of
  Nothing -> False
  Just counter -> counter > n

updateInfo :: (S m, HasInput m) => m ()
updateInfo = do
  input <- getInput
  when (onceThenFire $ iInfo input) $ modify $ \v -> v { vInfoShown = not (vInfoShown v) }

updateScale :: (S m, HasInput m) => m ()
updateScale = do
  input <- getInput
  let up = onceThenFire (iScaleUp input) || iScaleMouseUp input > 0
  let down = onceThenFire (iScaleDown input) || iScaleMouseDown input > 0
  let reset = isPressed (iScaleReset input) || isPressed (iMouseMiddleClick input)
  let increment = replicate (max 1 (iScaleMouseUp input)) (incrementScalar 90)
  let decrement = replicate (max 1 (iScaleMouseDown input)) (decrementScalar 9)
  let change s
        | reset = Scalar'None
        | up && not down = fapply increment s
        | down && not up = fapply decrement s
        | otherwise = s
  modify $ \v -> v { vScale = change (vScale v) }

fapply :: [a -> a] -> a -> a
fapply xs a = case xs of [] -> a; (y:ys) -> fapply ys (y a)

updateOrigin :: (R m, S m, HasInput m) => m ()
updateOrigin = do
  input <- getInput
  -- Move with keys
  let d = 5
  when (onceThenFire $ iUp input) $ modify $ \v -> v { vCenter = vCenter v + V2 0 (-d) }
  when (onceThenFire $ iDown input) $ modify $ \v -> v { vCenter = vCenter v + V2 0 d }
  when (onceThenFire $ iLeft input) $ modify $ \v -> v { vCenter = vCenter v + V2 (-d) 0 }
  when (onceThenFire $ iRight input) $ modify $ \v -> v { vCenter = vCenter v + V2 d 0 }
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
  accel <- gets vAccel
  mode <- gets vMode
  infoShown <- gets vInfoShown
  dinoAnimations <- getDinoAnimations
  let dinoLoc = Animate.currentLocation dinoAnimations dinoPos
  scale <- gets vScale
  let scalar = scalarToSpriteScale scale
  drawDino outline scalar dinoLoc (x, y)
  case origin of
    Nothing -> return ()
    Just origin' -> drawCrosshair (x,y) origin'
  -- HUD
  when infoShown $ do
    settings <- asks cSettings
    drawText (0, lineSpacing * 0) ("Mode:  " `mappend` if mode == Mode'Playback then "Playback" else "Stepper")
    drawText (0, lineSpacing * 1) ("File:  " `mappend` toText (sJSON settings))
    drawText (0, lineSpacing * 2) ("Scale: " `mappend` toText (asScaleString scale))
    drawText (0, lineSpacing * 3) ("Accel: " `mappend` toText (asSpeedString accel))
    drawText (0, lineSpacing * 4) $ toText $ concat ["Pos: Frame ", show $ Animate.pFrameIndex dinoPos, " (", show $ Animate.pCounter dinoPos, ")"]
  where
    lineSpacing = 14