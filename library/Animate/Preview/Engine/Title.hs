{-# LANGUAGE TemplateHaskell #-}
module Animate.Preview.Engine.Title where

import qualified Animate
import Control.Lens

import Animate.Preview.Engine.Types
import Animate.Preview.Engine.Dino

data TitleVars = TitleVars
  { tvDinoPos :: Animate.Position DinoKey Seconds
  } deriving (Show, Eq)

makeClassy ''TitleVars

initTitleVars :: TitleVars
initTitleVars = TitleVars (Animate.initPosition DinoKey'Idle)