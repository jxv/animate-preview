{-# LANGUAGE TemplateHaskell #-}
module Animate.Preview.State where

import Control.Lens

import Animate.Preview.Engine.Input
import Animate.Preview.Engine.Title

data Vars = Vars
  { vTitle :: TitleVars
  , vInput :: Input
  } deriving (Show, Eq)

initVars :: Vars
initVars = Vars initTitleVars initInput

instance HasTitleVars Vars where
  titleVars = lens vTitle (\v s -> v { vTitle = s })

makeClassy ''Vars
