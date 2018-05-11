module Animate.Preview.Loader where

import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified SDL
import Control.Monad.IO.Class (MonadIO(..))
import Control.Applicative ((<|>))
import Control.Monad.Reader (asks)
import Control.Concurrent (modifyMVar_, readMVar)
import Data.Text.Conversions (toText)
import Data.Maybe (fromMaybe)
import System.IO.Error

import qualified Animate
import Animate.Preview.Animation
import Animate.Preview.Resource
import Animate.Preview.State
import Animate.Preview.Config
import Animate.Preview.Logger

class Monad m => Loader m where
  load :: m Bool
  default load :: (R m, MonadIO m, Logger m) => m Bool
  load = do
    ren <- asks cRenderer
    json <- asks (sJSON . cSettings)
    maybeForceImage <- asks (sSpritesheet . cSettings)
    spriteSheetInfo <- liftIO $ catchIOError
      (Just <$> Animate.readSpriteSheetInfoJSON json)
      (const $ return Nothing)
    case spriteSheetInfo :: Maybe (Animate.SpriteSheetInfo Int Seconds) of
      Nothing -> do
        setLoaded Nothing
        logText $ "Not Loaded: " `mappend` toText json
        return False
      Just ssi -> do
        let ssi' = ssi
              { Animate.ssiImage = fromMaybe (Animate.ssiImage ssi) maybeForceImage
              , Animate.ssiAlpha = Nothing <|> Animate.ssiAlpha ssi }
        let animations = Animate.ssiAnimations ssi'
        if Map.null animations
          then do
            setLoaded Nothing
            logText $ "No Animations: " `mappend` toText json
            return False
          else do
            tex' <- liftIO $ loadTexture ren (Animate.ssiImage ssi') (Animate.ssiAlpha ssi')
            case tex' of
              Nothing -> do
                setLoaded Nothing
                logText $ "No Image: " `mappend` toText (Animate.ssiImage ssi')
                return False
              Just tex -> do
                let intToText i = fst $ Map.elemAt i animations
                let textToInt t = Map.lookup t $ Map.fromList $ zip (map fst $ Map.toList animations) [0..]
                let animations' = Animate.Animations $ V.fromList $ do
                      (_  , frames) <- Map.toList animations
                      return $ V.fromList $ do
                        (frameIdx, delay) <- frames
                        return $ Animate.Frame (Animate.ssiClips ssi' !! frameIdx) delay
                let spriteSheet = Animate.SpriteSheet animations' tex
                let totalKeys = V.length $ Animate.unAnimations animations'
                let loaded = Loaded textToInt intToText spriteSheet totalKeys
                setLoaded (Just loaded)
                logText $ "Loaded: " `mappend` toText json
                return True
    where
      loadTexture r path c = do
        s' <- loadSurface' path c
        case s' of
          Nothing -> return Nothing
          Just s -> do
            t <- SDL.createTextureFromSurface r s
            SDL.freeSurface s
            return (Just t)

setLoaded :: (R m, MonadIO m) => Maybe Loaded -> m ()
setLoaded l = do
  m <- asks cLoaded
  liftIO $ modifyMVar_ m (\_ -> return l)

getLoaded :: (R m, MonadIO m) => m (Maybe Loaded)
getLoaded = do
  m <- asks cLoaded
  liftIO $ readMVar m
