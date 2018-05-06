{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
module Animate.Preview.State where

import qualified Animate
import Control.Lens
import Control.Monad.State (MonadState)

import Animate.Preview.Animation
import Animate.Preview.Dino
import Animate.Preview.Input
import Animate.Preview.Color

data Vars = Vars
  { vInput :: Input
  , vDinoPos :: Animate.Position DinoKey Seconds
  , vOrigin :: Maybe Color
  , vOutline :: Maybe Color
  } deriving (Show, Eq)

initVars :: Vars
initVars = Vars
  { vInput = initInput
  , vDinoPos = Animate.initPosition DinoKey'Idle
  , vOrigin = Just Color'Red
  , vOutline = Just Color'Green
  }

makeClassy ''Vars

type S m = MonadState Vars m