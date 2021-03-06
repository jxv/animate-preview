module Animate.Preview.SDLInput where

import qualified SDL
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (gets)
import Linear

import Animate.Preview.State

keycodePressed :: SDL.Keycode -> SDL.EventPayload -> Bool
keycodePressed keycode event = case event of
  SDL.KeyboardEvent SDL.KeyboardEventData{keyboardEventKeysym = SDL.Keysym{keysymKeycode = code}, keyboardEventKeyMotion = motion, keyboardEventRepeat } ->
    code == keycode &&
    motion == SDL.Pressed &&
    not keyboardEventRepeat
  _ -> False

keycodeReleased :: SDL.Keycode -> SDL.EventPayload -> Bool
keycodeReleased keycode event = case event of
  SDL.KeyboardEvent SDL.KeyboardEventData{keyboardEventKeysym = SDL.Keysym{keysymKeycode = code}, keyboardEventKeyMotion = motion, keyboardEventRepeat } ->
    code == keycode &&
    motion == SDL.Released &&
    not keyboardEventRepeat
  _ -> False

class Monad m => SDLInput m where
  pollEventPayloads :: m [SDL.EventPayload]
  default pollEventPayloads :: MonadIO m => m [SDL.EventPayload]
  pollEventPayloads = liftIO $ map SDL.eventPayload <$> SDL.pollEvents

  getMousePos :: m (V2 Int)
  default getMousePos :: (S m, MonadIO m) => m (V2 Int)
  getMousePos = do
    drawSize <- gets vDrawSize
    winSize <- gets vWinSize
    SDL.P pos <- liftIO SDL.getAbsoluteMouseLocation
    let pos' = div <$> ((*) <$> drawSize <*> (fmap fromIntegral pos)) <*> winSize
    return $ fmap fromIntegral pos'

  getMouseClick :: m Bool
  default getMouseClick :: MonadIO m => m Bool
  getMouseClick = do
    test <- liftIO SDL.getMouseButtons
    return $ test (SDL.ButtonExtra 0)
    -- There's an offby one bug in `sdl2` which doesn't test SDL.ButtonLeft as expected
    -- It has been since fixed but not uploaded to hackage: https://github.com/haskell-game/sdl2/commit/eb7accdc2ae6349f262e758a93e5c62ee75d0e14

  getMouseMiddleClick :: m Bool
  default getMouseMiddleClick :: MonadIO m => m Bool
  getMouseMiddleClick = do
    test <- liftIO SDL.getMouseButtons
    return $ test (SDL.ButtonExtra 1) -- Same comment as above
