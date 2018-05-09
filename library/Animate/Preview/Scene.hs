module Animate.Preview.Scene where

import qualified Animate
import qualified Data.Vector as V
import Control.Monad (when)
import Control.Monad.Reader (asks)
import Control.Monad.State (modify, gets)
import Data.Text.Conversions (toText, fromText)
import KeyState
import Linear

import Animate.Preview.Animation
import Animate.Preview.Config
import Animate.Preview.Renderer
import Animate.Preview.Input
import Animate.Preview.Frame
import Animate.Preview.State
import Animate.Preview.ManagerInput
import Animate.Preview.Color
import Animate.Preview.Logger
import Animate.Preview.Scalar
import Animate.Preview.Mode
import Animate.Preview.Loader

class Monad m => Scene m where
  sceneStep :: m ()
  default sceneStep :: (R m, S m, Renderer m, HasInput m, Logger m, Loader m) => m ()
  sceneStep = do
    updateMode
    updateReload
    toggleVisuals
    updateOrigin
    updateKeyFrame
    updateSpeed
    updateScale
    updateInfo
    updateAnimation
    drawScene

updateKeyFrame :: (S m, HasInput m) => m ()
updateKeyFrame = do
  input <- getInput
  loaded' <- gets vLoaded 
  case loaded' of
    Nothing -> return ()
    Just loaded -> do
      let animations = (Animate.ssAnimations . lSpriteSheet) loaded
      when (isPressed $ iNextKeyFrame input) $ modify $ \v ->
        v { vPos = Animate.initPosition $ unsafeNextKey (Animate.pKey $ vPos v) animations }
      when (isPressed $ iPrevKeyFrame input) $ modify $ \v ->
        v { vPos = Animate.initPosition $ unsafePrevKey (Animate.pKey $ vPos v) animations }

unsafeNextKey :: Int -> Animate.Animations a b c -> Int
unsafeNextKey n (Animate.Animations a) = (1 + n) `mod` (V.length a)

unsafePrevKey :: Int -> Animate.Animations a b c -> Int
unsafePrevKey n (Animate.Animations a) = (n - 1) `mod` (V.length a)

updateMode :: (S m, HasInput m) => m ()
updateMode = do
  input <- getInput
  when (onceThenFire $ iMode input) $ modify $ \v -> v { vMode = if vMode v == Mode'Playback then Mode'Stepper else Mode'Playback }

updateReload :: (S m, HasInput m, Loader m) => m ()
updateReload = do
  input <- getInput
  when (isPressed $ iReload input) $ do
    _loaded <- load
    return ()

updateAnimation :: (R m, S m, Renderer m, HasInput m) => m ()
updateAnimation = do
  mode <- gets vMode
  loaded' <- gets vLoaded 
  case loaded' of
    Nothing -> return ()
    Just loaded -> do
      let animations = (Animate.ssAnimations . lSpriteSheet) loaded
      pos <- gets vPos
      case mode of
        Mode'Stepper -> do
          input <- getInput
          when (onceThenFire $ iFaster input) $ modify $ \v -> v { vPos = forceNextFrameIndex animations pos }
          when (onceThenFire $ iSlower input) $ modify $ \v -> v { vPos = forcePrevFrameIndex animations pos }
        Mode'Playback -> do
          accel <- gets vAccel
          let accelScalar = scalarToSeconds accel
          let pos' = Animate.stepPosition animations pos (frameDeltaSeconds * (Seconds accelScalar))
          modify $ \v -> v { vPos = pos' }

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
  pos <- gets vPos
  origin <- gets vOrigin
  outline <- gets vOutline
  accel <- gets vAccel
  mode <- gets vMode
  infoShown <- gets vInfoShown
  scale <- gets vScale
  loaded' <- gets vLoaded 
  case loaded' of
    Nothing -> return ()
    Just loaded -> do
      let animations = (Animate.ssAnimations . lSpriteSheet) loaded
      let frames = Animate.framesByAnimation animations (Animate.pKey pos)
      let frameDelay = Animate.fDelay $ frames V.! (Animate.pFrameIndex pos)
      let framesLen = V.length frames
      let totalSeconds = sum $ map Animate.fDelay (V.toList frames)
      let loc = Animate.currentLocation animations pos
      let scalar = scalarToSpriteScale scale
      drawAniSprite (lSpriteSheet loaded) outline scalar loc (x, y)
      when infoShown $ do
        let keyName = lIntToText loaded (Animate.pKey pos)
        drawText (ofsX, ofsY + lineSpacing * 4) $ toText $ concat ["Anim:"]
        drawText (ofsX, ofsY + lineSpacing * 5) $ toText $ concat ["  Key: \"", fromText keyName, "\" ", show $ Animate.pKey pos, " / ", show $ lTotalKeys loaded - 1]
        drawText (ofsX, ofsY + lineSpacing * 6) $ toText $ concat ["  Time: ", show totalSeconds]
        drawText (ofsX, ofsY + lineSpacing * 7) $ toText $ concat ["Pos:"]
        drawText (ofsX, ofsY + lineSpacing * 8) $ toText $ concat ["  Frame: ", show $ Animate.pFrameIndex pos,  " / ", show $ framesLen - 1]
        drawText (ofsX, ofsY + lineSpacing * 9) $ toText $ concat ["  Time: ", show $ Animate.pCounter pos, " / ", show $ frameDelay]
  case origin of
    Nothing -> return ()
    Just origin' -> drawCrosshair (x,y) origin'
  -- HUD
  when infoShown $ do
    settings <- asks cSettings
    drawText (ofsX, ofsY + lineSpacing * 0) ("Mode: " `mappend` if mode == Mode'Playback then "Playback" else "Stepper")
    drawText (ofsX, ofsY + lineSpacing * 1) ("File: " `mappend` toText (sJSON settings))
    drawText (ofsX, ofsY + lineSpacing * 2) ("Scale: " `mappend` toText (asScaleString scale))
    drawText (ofsX, ofsY + lineSpacing * 3) ("Accel: " `mappend` toText (asSpeedString accel))
  where
    lineSpacing = 12
    ofsX = 6
    ofsY = 6