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
  , vOriginColor :: Maybe Color
  , vOutlineColor :: Maybe Color
  , vOrigin :: V2 Int
  , vAccel :: Scalar
  , vScale :: Scalar
  , vInfoShown :: Bool
  , vMode :: Mode
  , vDrawSize :: V2 Int
  , vWinSize :: V2 Int
  }

initVars :: V2 Int -> V2 Int -> Vars
initVars winSize drawSize = Vars
  { vInput = initInput
  , vBackground = Mono'Gray
  , vOriginColor = Just Color'Red
  , vOutlineColor = Just Color'Green
  , vOrigin = div <$> drawSize <*> 2
  , vAccel = Scalar'None
  , vScale = Scalar'None
  , vInfoShown = True
  , vMode = Mode'Playback
  , vWinSize = winSize
  , vDrawSize = drawSize
  }

makeClassy ''Vars

type S m = MonadState Vars m