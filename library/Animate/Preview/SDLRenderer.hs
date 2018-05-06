module Animate.Preview.SDLRenderer where

import qualified SDL
import Data.StateVar (($=))
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.C.Types
import SDL.Vect
import Control.Monad (void)

class Monad m => SDLRenderer m where
  presentRenderer :: SDL.Renderer -> m ()
  default presentRenderer :: MonadIO m => SDL.Renderer -> m ()
  presentRenderer = SDL.present

  clearRenderer :: SDL.Renderer -> m ()
  default clearRenderer :: MonadIO m => SDL.Renderer -> m ()
  clearRenderer ren = do
    SDL.rendererDrawColor ren $= V4 0x00 0x00 0x00 0xff
    liftIO $ void $ SDL.clear ren

  queryTexture :: SDL.Texture -> m SDL.TextureInfo
  default queryTexture ::  MonadIO m => SDL.Texture -> m SDL.TextureInfo
  queryTexture = SDL.queryTexture

  drawTexture :: SDL.Renderer -> SDL.Texture -> Maybe (SDL.Rectangle CInt)-> Maybe (SDL.Rectangle CInt) -> m ()
  default drawTexture :: MonadIO m => SDL.Renderer -> SDL.Texture -> Maybe (SDL.Rectangle CInt) -> Maybe (SDL.Rectangle CInt) -> m ()
  drawTexture renderer tex maybeClip maybeLoc = SDL.copy renderer tex maybeClip maybeLoc
