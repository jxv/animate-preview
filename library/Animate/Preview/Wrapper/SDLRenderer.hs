module Animate.Preview.Wrapper.SDLRenderer where

import qualified SDL
import Data.StateVar (($=))
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.C.Types
import SDL.Vect
import Control.Monad (void)

class Monad m => SDLRenderer m where
  presentRenderer :: SDL.Renderer -> m ()
  clearRenderer :: SDL.Renderer -> m ()
  queryTexture :: SDL.Texture -> m SDL.TextureInfo
  drawTexture :: SDL.Renderer -> SDL.Texture -> Maybe (SDL.Rectangle CInt)-> Maybe (SDL.Rectangle CInt) -> m ()

updateWindowSurface' :: MonadIO m => SDL.Window -> m ()
updateWindowSurface' window = liftIO $ SDL.updateWindowSurface window

presentRenderer' :: MonadIO m => SDL.Renderer -> m ()
presentRenderer' = SDL.present

clearSurface' :: MonadIO m => SDL.Surface -> m ()
clearSurface' screen = liftIO $ SDL.surfaceFillRect screen Nothing (V4 0 0 0 0)

drawTexture' :: MonadIO m => SDL.Renderer -> SDL.Texture -> Maybe (SDL.Rectangle CInt) -> Maybe (SDL.Rectangle CInt) -> m ()
drawTexture' renderer tex maybeClip maybeLoc = SDL.copy renderer tex maybeClip maybeLoc

clearRenderer' :: MonadIO m => SDL.Renderer -> m ()
clearRenderer' ren = do
  SDL.rendererDrawColor ren $= V4 0x00 0x00 0x00 0xff
  liftIO $ void $ SDL.clear ren

queryTexture' ::  MonadIO m => SDL.Texture -> m SDL.TextureInfo
queryTexture' = SDL.queryTexture
