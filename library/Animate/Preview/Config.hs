{-# LANGUAGE ConstraintKinds #-}
module Animate.Preview.Config where

import qualified SDL
import qualified Animate
import Data.Text (Text)
import Control.Monad.Reader (MonadReader)
import Control.Concurrent

import Animate.Preview.Animation
import Animate.Preview.Resource

data Settings = Settings
  { sTarget :: String
  , sSpritesheet :: Maybe String
  }

data Loaded = Loaded
  { lTextToInt :: Text -> Maybe Int
  , lIntToText :: Int -> Text
  , lSpriteSheet :: Animate.SpriteSheet Int SDL.Texture Seconds
  , lTotalKeys :: Int
  }

data Current = Current
  { cPos :: Animate.Position Int Seconds
  , cKeyName :: Text
  } deriving (Show, Eq)

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cResources :: Resources
  , cHighDpi :: Bool
  , cSettings :: Settings
  , cCurrent :: MVar (Maybe Current)
  , cLoaded :: MVar (Maybe Loaded)
  , cFps :: Int
  , cFrameDeltaSeconds :: Seconds
  , cReload :: MVar Bool
  }

type R m = MonadReader Config m