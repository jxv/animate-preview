module Animate.Preview.Resource where

import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified SDL.Font as Font
import qualified SDL.Image as Image
import qualified Animate
import qualified SDL.Raw.Video as Raw
import qualified SDL.Internal.Numbered as Numbered
import Data.Text (Text)
import Data.Text.Conversions (toText)
import Data.StateVar (($=))
import SDL.Vect
import System.IO.Error (catchIOError)
import Paths_animate_preview (getDataFileName)

data Resources = Resources
  { rFont :: Font.Font
  }

-- | Produce a new 'SDL.Surface' based on an existing one, but
-- optimized for blitting to the specified 'SDL.PixelFormat'.
convertSurface :: SDL.Surface -> SDL.PixelFormat -> IO SDL.Surface
convertSurface (SDL.Surface s _) pixFmt = do
  fmt <- Raw.allocFormat (Numbered.toNumber pixFmt)
  surface <- SDL.Surface <$> Raw.convertSurface s fmt 0 <*> pure Nothing
  surface <$ Raw.freeFormat fmt

loadSurface' :: FilePath -> Maybe Animate.Color -> IO (Maybe SDL.Surface)
loadSurface' path alpha = do
  surface0' <- catchIOError
    (Just <$> Image.load path)
    (const $ return Nothing)
  case surface0' of
    Nothing -> return Nothing
    Just surface0 -> do
      surface <- convertSurface surface0 SDL.RGBA8888
      SDL.freeSurface surface0
      case alpha of
        Just (r,g,b) -> SDL.surfaceColorKey surface $= (Just $ V4 r g b 0x00)
        Nothing -> return ()
      return (Just surface)

alphaColorDef :: Animate.Color
alphaColorDef = (0xff,0x00,0xff)

createText :: SDL.Renderer -> Font.Font -> Text -> IO SDL.Texture
createText ren font text = do
  Font.setHinting font Font.None
  Font.setOutline font 2
  outline <- Font.solid font (V4 0 0 0 0) text
  Font.setOutline font 0
  inline <- Font.solid font (V4 255 255 255 255) text
  _ <- SDL.surfaceBlit inline Nothing outline (Just $ SDL.P $ V2 2 2)
  tex <- SDL.createTextureFromSurface ren outline
  SDL.freeSurface inline
  SDL.freeSurface outline
  return tex

loadResources :: SDL.Renderer -> IO Resources
loadResources _renderer = do
  fileName <- getDataFileName "resource/ProggyClean.ttf"
  font <- Font.load fileName 16
  return Resources
    { rFont = font
    }

freeResources :: Resources -> IO ()
freeResources r = do
  Font.free (rFont r)
