{-# LANGUAGE TemplateHaskell #-}
module Animate.Preview.Runner where

import qualified Animate
import qualified SDL
import qualified Data.Map as Map
import qualified Data.Vector as V
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (modify, gets)
import Control.Monad.Reader (asks)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text.Conversions (toText)
import KeyState
import System.IO.Error (catchIOError)

import Animate.Preview.Animation
import Animate.Preview.Config
import Animate.Preview.Clock
import Animate.Preview.Logger
import Animate.Preview.Renderer
import Animate.Preview.Input
import Animate.Preview.Frame
import Animate.Preview.Scene
import Animate.Preview.ManagerInput
import Animate.Preview.State
import Animate.Preview.Resource

load :: (R m, S m, MonadIO m, Logger m) => m ()
load = do
  ren <- asks cRenderer
  json <- asks (sJSON . cSettings)
  maybeForceImage <- asks (sSpritesheet . cSettings)
  spriteSheetInfo <- liftIO $ catchIOError
    (Just <$> Animate.readSpriteSheetInfoJSON json)
    (const $ return Nothing)
  case spriteSheetInfo :: Maybe (Animate.SpriteSheetInfo Int Seconds) of
    Nothing -> do
      modify $ \v -> v { vLoaded = Nothing }
      logText $ "Not Loaded: " `mappend` toText json
    Just ssi -> do
      let ssi' = ssi
            { Animate.ssiImage = fromMaybe (Animate.ssiImage ssi) maybeForceImage
            , Animate.ssiAlpha = Nothing <|> Animate.ssiAlpha ssi }
      let animations = Animate.ssiAnimations ssi'
      if Map.null animations
        then do
          modify $ \v -> v { vLoaded = Nothing }
          logText $ "No Animations: " `mappend` toText json
        else do
          tex' <- liftIO $ loadTexture ren (Animate.ssiImage ssi') (Animate.ssiAlpha ssi')
          case tex' of
            Nothing -> do
              modify $ \v -> v { vLoaded = Nothing }
              logText $ "No Image: " `mappend` toText (Animate.ssiImage ssi')
            Just tex -> do
              let intToText i = fst $ Map.elemAt i animations
              let textToInt t = Map.lookup t $ Map.fromList $ zip (map fst $ Map.toList animations) [0..]
              let animations' = Animate.Animations $ V.fromList $ do
                    (_  , frames) <- Map.toList animations
                    return $ V.fromList $ do
                      (frameIdx, delay) <- frames
                      return $ Animate.Frame (Animate.ssiClips ssi' !! frameIdx) delay
              let spriteSheet = Animate.SpriteSheet animations' tex
              let loaded = Loaded textToInt intToText spriteSheet
              modify $ \v -> v { vLoaded = Just loaded }
              logText $ "Loaded: " `mappend` toText json
  where
    loadTexture r path c = do
      s' <- loadSurface' path c
      case s' of
        Nothing -> return Nothing
        Just s -> do
          t <- SDL.createTextureFromSurface r s
          SDL.freeSurface s
          return (Just t)

mainLoop :: (R m, S m, Logger m, Clock m, Renderer m, HasInput m, Scene m) => m ()
mainLoop = do
  winSize <- asks cWinSize
  background <- gets vBackground
  updateInput
  input <- getInput
  clearScreen
  drawBackground winSize background
  sceneStep
  drawScreen
  delayMilliseconds frameDeltaMilliseconds
  let quit = iQuit input || ksStatus (iEscape input) == KeyStatus'Pressed
  unless quit mainLoop