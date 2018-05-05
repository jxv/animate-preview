module Animate.Preview.Config
  ( Config(..)
  , Resources(..)
  ) where

import qualified SDL
import Linear

import Animate.Preview.Resource

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cResources :: Resources
  , cWinSize :: V2 Int
  }
