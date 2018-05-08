{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
module Animate.Preview.State where

import qualified SDL
import qualified Animate
import Data.Text (Text)
import Control.Lens
import Control.Monad.State (MonadState)
import Linear

import Animate.Preview.Animation
import Animate.Preview.Dino
import Animate.Preview.Input
import Animate.Preview.Color
import Animate.Preview.Scalar
import Animate.Preview.Mode

data Loaded = Loaded
  { lsTextToInt :: Text -> Maybe Int
  , lsIntToText :: Int -> Text
  , lsSpriteSheet :: Animate.SpriteSheet Int SDL.Texture Seconds
  }

data Vars = Vars
  { vInput :: Input
  , vDinoPos :: Animate.Position DinoKey Seconds
  , vBackground :: Mono
  , vOrigin :: Maybe Color
  , vOutline :: Maybe Color
  , vCenter :: V2 Int
  , vAccel :: Scalar
  , vScale :: Scalar
  , vInfoShown :: Bool
  , vMode :: Mode
  , vPos :: Animate.Position Int Seconds
  , vLoaded :: Maybe Loaded
  }

initVars :: V2 Int -> Vars
initVars center = Vars
  { vInput = initInput
  , vDinoPos = Animate.initPosition DinoKey'Idle
  , vBackground = Mono'Gray
  , vOrigin = Just Color'Red
  , vOutline = Just Color'Green
  , vCenter = center
  , vAccel = Scalar'None
  , vScale = Scalar'None
  , vInfoShown = True
  , vMode = Mode'Playback
  , vPos = Animate.initPosition 0
  , vLoaded = Nothing
  }

makeClassy ''Vars

type S m = MonadState Vars m