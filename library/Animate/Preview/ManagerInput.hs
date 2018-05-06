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
  { iSpace = next [SDL.KeycodeSpace] iSpace
  , iUp = next [SDL.KeycodeUp, SDL.KeycodeK] iUp
  , iDown = next [SDL.KeycodeDown, SDL.KeycodeL] iDown
  , iEscape = next [SDL.KeycodeEscape] iEscape
  , iReload = next [SDL.KeycodeR] iReload
  , iOrigin = next [SDL.KeycodeT] iOrigin
  , iOutline = next [SDL.KeycodeO] iOutline
  , iBackground = next [SDL.KeycodeB] iBackground
  , iMouseClick = updateKeyState 1 (iMouseClick i) mouseClick
  , iCenterOrigin = next [SDL.KeycodeC] iCenterOrigin
  , iFaster = next [SDL.KeycodeSemicolon] iFaster
  , iSlower = next [SDL.KeycodeJ] iSlower
  , iQuit = elem SDL.QuitEvent events
  }
  where
    next xs f = nextKeystate events 1 xs (f i)

nextKeystate :: [SDL.EventPayload] -> Int -> [SDL.Keycode] -> KeyState Int -> KeyState Int
nextKeystate events count keycodes keystate
  | or $ map pressed keycodes = pressedKeyState
  | or $ map released keycodes = releasedKeyState
  | otherwise = maintainKeyState count keystate
  where
    released keycode = or $ map (keycodeReleased keycode) events
    pressed keycode = or $ map (keycodePressed keycode) events