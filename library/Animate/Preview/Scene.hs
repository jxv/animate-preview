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

class Monad m => Scene m where
  sceneStep :: m ()
  default sceneStep :: (R m, S m, Renderer m, HasInput m) => m ()
  sceneStep = do
    input <- getInput
    updateScene
    drawScene

updateScene :: (R m, S m, Renderer m, HasInput m) => m ()
updateScene = do
  dinoAnimations <- getDinoAnimations
  dinoPos <- gets vDinoPos
  let dinoPos' = Animate.stepPosition dinoAnimations dinoPos frameDeltaSeconds
  modify $ (\v -> v { vDinoPos = dinoPos' })

drawScene :: (R m, S m, Renderer m, HasInput m) => m ()
drawScene = do
  V2 x y <- asks (sCenter . cSettings)
  dinoPos <- gets vDinoPos
  dinoAnimations <- getDinoAnimations
  let dinoLoc = Animate.currentLocation dinoAnimations dinoPos
  let outline = True
  let crosshair = True
  drawDino outline dinoLoc (x, y)
  when crosshair $ drawCrosshair (x, y)