module Animate.Preview.ManagerInput where

import qualified SDL
import Control.Monad.State
import KeyState

import Animate.Preview.Input
import Animate.Preview.SDLInput
import Animate.Preview.State

class Monad m => HasInput m where
  updateInput :: m ()
  default updateInput :: (HasInput m, SDLInput m) => m ()
  updateInput = do
    input <- getInput
    events <- pollEventPayloads
    mousePos <- getMousePos
    mouseClick <- getMouseClick
    setInput (stepControl events mouseClick input) { iMousePos = mousePos }

  setInput :: Input -> m ()
  default setInput :: S m => Input -> m ()
  setInput input = modify (\v -> v { vInput = input })

  getInput :: m Input
  default getInput :: S m => m Input
  getInput = gets vInput

stepControl :: [SDL.EventPayload] -> Bool -> Input -> Input
stepControl events mouseClick i = i
  { iSpace = next 1 [SDL.KeycodeSpace] (iSpace i)
  , iUp = next 1 [SDL.KeycodeUp, SDL.KeycodeK] (iUp i)
  , iDown = next 1 [SDL.KeycodeDown, SDL.KeycodeL] (iDown i)
  , iEscape = next 1 [SDL.KeycodeEscape] (iEscape i)
  , iReload = next 1 [SDL.KeycodeR] (iReload i)
  , iOrigin = next 1 [SDL.KeycodeT] (iOrigin i)
  , iOutline = next 1 [SDL.KeycodeO] (iOutline i)
  , iBackground = next 1 [SDL.KeycodeB] (iBackground i)
  , iMouseClick = updateKeyState 1 (iMouseClick i) mouseClick
  , iCenterOrigin = next 1 [SDL.KeycodeC] (iCenterOrigin i)
  , iQuit = elem SDL.QuitEvent events
  }
  where
    next = nextKeystate events

nextKeystate :: [SDL.EventPayload] -> Int -> [SDL.Keycode] -> KeyState Int -> KeyState Int
nextKeystate events count keycodes keystate
  | or $ map pressed keycodes = pressedKeyState
  | or $ map released keycodes = releasedKeyState
  | otherwise = maintainKeyState count keystate
  where
    released keycode = or $ map (keycodeReleased keycode) events
    pressed keycode = or $ map (keycodePressed keycode) events