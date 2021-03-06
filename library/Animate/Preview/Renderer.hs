module Animate.Preview.Renderer where

import qualified Animate
import qualified SDL
import qualified SDL.Primitive as Gfx
import qualified Data.Map as Map
import SDL.Vect
import Control.Monad.Reader (asks)
import Control.Monad.State (modify)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (fromMaybe)
import Foreign.C.Types (CFloat(..), CInt)
import Data.Text (Text)
import Data.Text.Conversions (fromText)
import Data.StateVar (get, ($=))
import Data.List (foldl')

import Animate.Preview.Config
import Animate.Preview.Resource
import Animate.Preview.Animation
import Animate.Preview.Color
import Animate.Preview.SDLRenderer
import Animate.Preview.State

class Monad m => Renderer m where
  updateScreenInfo :: m ()
  default updateScreenInfo :: (R m, S m, MonadIO m) => m ()
  updateScreenInfo = do
    w <- asks cWindow
    winSize <- liftIO $ get (SDL.windowSize w)
    drawSize <- liftIO $ SDL.glGetDrawableSize w
    modify $ \v -> v { vDrawSize = fromIntegral <$> drawSize, vWinSize = fromIntegral <$> winSize }

  clearScreen :: m ()
  default clearScreen :: (SDLRenderer m, R m) => m ()
  clearScreen = do
    renderer <- asks cRenderer
    clearRenderer renderer

  drawScreen :: m ()
  default drawScreen :: (SDLRenderer m, R m) => m ()
  drawScreen = do
    renderer <- asks cRenderer
    presentRenderer renderer

  drawBackground :: V2 Int -> Mono -> m ()
  default drawBackground :: (SDLRenderer m, R m, MonadIO m) => V2 Int -> Mono -> m ()
  drawBackground (V2 w h) mono = do
    renderer <- asks cRenderer
    let (lite, dark) = fromMono mono
    let indices = do
          x <- [0..(w `div` 10)]
          y <- [0..(h `div` 10)]
          let x0 = x * 10
              x1 = x * 10 + 10
          let y0 = y * 10
              y1 = y * 10 + 10
          let color = if (x + y) `mod` 2 == 0 then lite else dark
          return (V2 x0 y0, V2 x1 y1, color) 
    flip mapM_ indices $ \(a,b,c) ->
      liftIO $ Gfx.fillRectangle renderer (fromIntegral <$> a) (fromIntegral <$> b) c
  
  drawAniSprite :: Animate.SpriteSheet Int SDL.Texture Seconds -> Maybe Color -> Float -> Animate.SpriteClip Int -> (Int, Int) -> m ()
  default drawAniSprite :: (SDLRenderer m, R m, MonadIO m) => Animate.SpriteSheet Int SDL.Texture Seconds -> Maybe Color -> Float -> Animate.SpriteClip Int -> (Int, Int) -> m ()
  drawAniSprite as = drawSprite (const as)

  drawCrosshair :: (Int, Int) -> Color -> m ()
  default drawCrosshair :: (MonadIO m, R m) => (Int, Int) -> Color -> m ()
  drawCrosshair (x,y) color = do
    ren <- asks cRenderer
    let radius = 8
    let diameter = fromIntegral $ radius * 2
    liftIO $ do
      let color' = fromColor color
      Gfx.horizontalLine ren (fromIntegral <$> V2 (x - radius) y) diameter color'
      Gfx.verticalLine ren (fromIntegral <$> V2 x (y - radius)) diameter color'

  drawText :: (Int, Int) -> Text -> m ()
  default drawText :: (SDLRenderer m, R m, MonadIO m) => (Int, Int) -> Text -> m ()
  drawText xy text = do
    glyphMap <- asks (rGlyphMap . cResources)
    glyphSize <- asks (rGlyphSize . cResources)
    let instructions = buildGlyphIntructions glyphSize glyphMap (fromText text)
    let (x,y) = xy
    flip mapM_ instructions $ \(tex, (x',y')) -> do
      let xy' = (x + x', y + y')
      drawTextureSprite (const tex) xy'

buildGlyphIntructions :: Int -> Map.Map Char Glyph -> [Char] -> [(SDL.Texture, (Int, Int))]
buildGlyphIntructions _glyphSize glyphMap str = fst $ foldl' appendGlyph ([], (0,0)) str
  where
    appendGlyph
      :: ([(SDL.Texture, (Int, Int))], (Int,Int))
      -> Char
      -> ([(SDL.Texture, (Int, Int))], (Int,Int)) 
    appendGlyph (glyphs, (x,y)) ch = case Map.lookup ch glyphMap of
      Nothing -> (glyphs, (x,y))
      Just glyph -> let
        tex = gTexture glyph
        (xmin, _xmax, _ymin, _ymax, adv) = gMetrics glyph
        x' = x + xmin
        y' = y
        in ((tex, (x', y')) : glyphs, (x + xmin + adv, y))

drawTextureSprite :: (SDLRenderer m, R m) => (Config -> SDL.Texture) -> (Int, Int) -> m ()
drawTextureSprite getTex (x,y) = do
  renderer <- asks cRenderer
  tex <- asks getTex
  SDL.TextureInfo{textureWidth,textureHeight} <- queryTexture tex
  let dim = V2 textureWidth textureHeight
  drawTexture
    renderer
    tex
    Nothing
    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) dim)

drawSprite :: (SDLRenderer m, R m, MonadIO m) => (Config -> Animate.SpriteSheet key SDL.Texture Seconds) -> Maybe Color -> Float -> Animate.SpriteClip key -> (Int, Int) -> m ()
drawSprite ss outline scalar' clip (x,y) = do
  let scalar = pure (CFloat scalar')
  renderer <- asks cRenderer
  sheet <- asks (Animate.ssImage . ss)
  let scaleDown n = fmap round $ (fmap fromIntegral n) / scalar
  let scaleUp n = fmap round $ (fmap fromIntegral n) * scalar
  let clip'@(SDL.Rectangle _ dim) = rectFromClip clip
  let offset = offsetFromClip clip
  let loc = (+) <$> offset <*> (scaleDown $ V2 x y)
  case outline of
    Nothing -> return ()
    Just outline' -> liftIO $ Gfx.rectangle renderer (scaleUp loc) (scaleUp $ loc + dim) (fromColor outline')
  -- set scale for sprite
  liftIO $ SDL.rendererScale renderer $= scalar
  drawTexture
    renderer
    sheet
    (Just clip')
    (Just $ SDL.Rectangle (SDL.P loc) dim)
  -- reset scale from sprite
  liftIO $ SDL.rendererScale renderer $= (V2 1 1)

getSpriteAnimations :: R m => (Config -> Animate.SpriteSheet key SDL.Texture Seconds) -> m (Animations key)
getSpriteAnimations ss = asks (Animate.ssAnimations . ss)

rectFromClip :: Animate.SpriteClip key -> SDL.Rectangle CInt
rectFromClip Animate.SpriteClip{scX,scY,scW,scH} = SDL.Rectangle (SDL.P (V2 (num scX) (num scY))) (V2 (num scW) (num scH))
  where
    num = fromIntegral

offsetFromClip :: Animate.SpriteClip key -> V2 CInt
offsetFromClip Animate.SpriteClip{scOffset} = fromMaybe
  (V2 0 0)
  ((\(x,y) -> fromIntegral <$> V2 (-x) (-y)) <$> scOffset)
