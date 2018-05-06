{-# LANGUAGE ConstraintKinds #-}
module Animate.Preview.Config where

import qualified SDL
import Linear
import Control.Monad.Reader (MonadReader)

import Animate.Preview.Resource

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cResources :: Resources
  , cWinSize :: V2 Int
  }

type R m = MonadReader Config m