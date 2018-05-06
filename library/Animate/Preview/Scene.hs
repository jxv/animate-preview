module Animate.Preview.Scene where

import qualified Animate
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..), asks)
import Control.Monad.State (MonadState(..), modify, gets)
import KeyState
import Linear

import Animate.Preview.Config
import Animate.Preview.Renderer
import Animate.Preview.Input
import Animate.Preview.Frame
import Animate.Preview.Dino
import Animate.Preview.State
import Animate.Preview.ManagerInput
import Animate.Preview.Color

class Monad m => Scene m where
  sceneStep :: m ()
  default sceneStep :: (R m, S m, Renderer m, HasInput m) => m ()
  sceneStep = do
    updateScene
    drawScene

updateScene :: (R m, S m, Renderer m, HasInput m) => m ()
updateScene = do
  input <- getInput
  dinoAnimations <- getDinoAnimations
  dinoPos <- gets vDinoPos
  let dinoPos' = Animate.stepPosition dinoAnimations dinoPos frameDeltaSeconds
  let toggleOrigin = isPressed (iOrigin input)
  let toggleOutline = isPressed (iOutline input)
  modify $ (\v -> v
    { vDinoPos = dinoPos'
    , vOrigin = (if toggleOrigin then toggleColors else id) (vOrigin v)
    , vOutline = (if toggleOutline then toggleColors else id) (vOutline v)
    })

drawScene :: (R m, S m, Renderer m, HasInput m) => m ()
drawScene = do
  V2 x y <- asks (sCenter . cSettings)
  dinoPos <- gets vDinoPos
  origin <- gets vOrigin
  outline <- gets vOutline
  dinoAnimations <- getDinoAnimations
  let dinoLoc = Animate.currentLocation dinoAnimations dinoPos

  drawDino outline dinoLoc (x, y)

  case origin of
    Nothing -> return ()
    Just origin' -> drawCrosshair (x,y) origin'