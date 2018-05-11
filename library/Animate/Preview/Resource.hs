module Animate.Preview.Resource where

import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Image as Image
import qualified Animate
import qualified SDL.Raw.Video as Raw
import qualified SDL.Internal.Numbered as Numbered
import qualified Data.Map as Map
import Control.Monad (filterM)
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Data.StateVar (($=))
import SDL.Vect
import System.IO.Error (catchIOError)
import Paths_animate_preview (getDataFileName)

data Resources = Resources
  { rFont :: Font.Font
  , rGlyphMap :: Map.Map Char Glyph
  , rGlyphSize :: Int
  }

data Glyph = Glyph
  { gChar :: Char
  , gMetrics :: (Int, Int, Int, Int, Int)
  , gTexture :: SDL.Texture
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

createText :: Bool -> SDL.Renderer -> Font.Font -> Text -> IO SDL.Texture
createText highDpi ren font text = do
  Font.setHinting font Font.None
  Font.setOutline font (if highDpi then 4 else 2)
  outline <- Font.solid font (V4 0 0 0 0) text
  Font.setOutline font 0
  inline <- Font.solid font (V4 255 255 255 255) text
  _ <- SDL.surfaceBlit inline Nothing outline (Just $ SDL.P (if highDpi then 4 else 2))
  tex <- SDL.createTextureFromSurface ren outline
  SDL.freeSurface inline
  SDL.freeSurface outline
  return tex

createGlyph :: Bool -> SDL.Renderer -> Font.Font -> Char -> IO (Maybe Glyph)
createGlyph highDpi ren font ch = do
  metrics' <- Font.glyphMetrics font ch
  --
  case metrics' of
    Nothing -> return Nothing
    Just metrics@(xmin, xmax, _ymin, _ymax, _adv) -> do
      if (xmax - xmin > 0) || ch == ' '
        then do
          Font.setHinting font Font.None
          Font.setOutline font (if highDpi then 4 else 2)
          outline <- Font.solidGlyph font (V4 0 0 0 0) ch
          Font.setOutline font 0
          inline <- Font.solidGlyph font (V4 255 255 255 255) ch
          _ <- SDL.surfaceBlit inline Nothing outline (Just $ SDL.P (if highDpi then 4 else 2))
          tex <- SDL.createTextureFromSurface ren outline
          SDL.freeSurface inline
          SDL.freeSurface outline
          --
          return . Just $ Glyph ch metrics tex
        else return Nothing

loadResources :: Bool -> SDL.Renderer -> IO Resources
loadResources highDpi ren = do
  fileName <- getDataFileName "resource/ProggyClean.ttf"
  let glyphSize = if highDpi then 32 else 16
  font <- Font.load fileName glyphSize
  putStrLn "Font cache: filtering glyphs"
  availableChars <- filterM (Font.glyphProvided font) [minBound..maxBound]
  putStrLn "Font cache: creating textures"
  glyphs <- mapM (createGlyph highDpi ren font) (' ':availableChars)
  let glyphs' = map (\g -> (gChar g, g)) (catMaybes glyphs)
  return Resources
    { rFont = font
    , rGlyphMap = Map.fromList glyphs'
    , rGlyphSize = glyphSize
    }

freeResources :: Resources -> IO ()
freeResources r = do
  Font.free (rFont r)
