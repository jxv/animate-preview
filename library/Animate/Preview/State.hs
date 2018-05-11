{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
module Animate.Preview.State where

import qualified SDL
import qualified Animate
import Data.Text (Text)
import Control.Lens
import Control.Concurrent (putMVar)
import Control.Monad.State (MonadState, gets)
import Linear
import Control.Concurrent

import Animate.Preview.Animation
import Animate.Preview.Input
import Animate.Preview.Color
import Animate.Preview.Scalar
import Animate.Preview.Mode

data Vars = Vars
  { vInput :: Input
  , vBackground :: Mono
  , vOrigin :: Maybe Color
  , vOutline :: Maybe Color
  , vCenter :: V2 Int
  , vAccel :: Scalar
  , vScale :: Scalar
  , vInfoShown :: Bool
  , vMode :: Mode
  }

initVars :: V2 Int -> Vars
initVars center = Vars
  { vInput = initInput
  , vBackground = Mono'Gray
  , vOrigin = Just Color'Red
  , vOutline = Just Color'Green
  , vCenter = center
  , vAccel = Scalar'None
  , vScale = Scalar'None
  , vInfoShown = True
  , vMode = Mode'Playback
  }

makeClassy ''Vars

type S m = MonadState Vars m