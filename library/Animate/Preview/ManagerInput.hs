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
    setInput (stepControl events input)

  setInput :: Input -> m ()
  default setInput :: S m => Input -> m ()
  setInput input = modify (\v -> v { vInput = input })

  getInput :: m Input
  default getInput :: S m => m Input
  getInput = gets vInput

stepControl :: [SDL.EventPayload] -> Input -> Input
stepControl events Input{iSpace,iUp,iDown,iEscape,iReload} = Input
  { iSpace = next 1 [SDL.KeycodeSpace] iSpace
  , iUp = next 1 [SDL.KeycodeUp, SDL.KeycodeK] iUp
  , iDown = next 1 [SDL.KeycodeDown, SDL.KeycodeL] iDown
  , iEscape = next 1 [SDL.KeycodeEscape] iEscape
  , iReload = next 1 [SDL.KeycodeR] iReload
  , iQuit = elem SDL.QuitEvent events
  }
  where
    next count keycodes keystate
      | or $ map pressed keycodes = pressedKeyState
      | or $ map released keycodes = releasedKeyState
      | otherwise = maintainKeyState count keystate
    released keycode = or $ map (keycodeReleased keycode) events
    pressed keycode = or $ map (keycodePressed keycode) events
