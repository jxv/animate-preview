{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
module Animate.Preview.State where

import qualified Animate
import Control.Lens
import Control.Monad.State (MonadState)

import Animate.Preview.Animation
import Animate.Preview.Dino
import Animate.Preview.Input

data Vars = Vars
  { vInput :: Input
  , vDinoPos :: Animate.Position DinoKey Seconds
  } deriving (Show, Eq)

initVars :: Vars
initVars = Vars initInput (Animate.initPosition DinoKey'Idle)

makeClassy ''Vars

type S m = MonadState Vars m