{-# LANGUAGE ConstraintKinds #-}
module Animate.Preview.Config where

import qualified SDL
import Linear
import Control.Monad.Reader (MonadReader)
import Data.Word (Word32)

import Animate.Preview.Resource

data Settings = Settings
  { sJSON :: String
  , sSpritesheet :: Maybe String
  , sScale :: Float
  , sCenter :: V2 Int
  }

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cResources :: Resources
  , cWinSize :: V2 Int
  , cOrgWinSize :: V2 Int
  , cHighDpi :: Bool
  , cSettings :: Settings
  }

type R m = MonadReader Config m