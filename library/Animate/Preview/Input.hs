module Animate.Preview.Input where

import KeyState
import Linear

data Input = Input
  { iSpace :: KeyState Int
  , iUp :: KeyState Int
  , iDown :: KeyState Int
  , iLeft :: KeyState Int
  , iRight :: KeyState Int
  , iScaleReset :: KeyState Int
  , iScaleUp :: KeyState Int
  , iScaleDown :: KeyState Int
  , iEscape :: KeyState Int
  , iReload :: KeyState Int
  , iOrigin :: KeyState Int
  , iOutline :: KeyState Int
  , iBackground :: KeyState Int
  , iMousePos :: V2 Int
  , iMouseClick :: KeyState Int
  , iMouseMiddleClick :: KeyState Int
  , iCenterOrigin :: KeyState Int
  , iAccelReset :: KeyState Int
  , iFaster :: KeyState Int
  , iSlower :: KeyState Int
  , iQuit :: Bool
  } deriving (Show, Eq)

initInput :: Input
initInput = Input a a a a a a a a a a a a a (V2 0 0) a a a a a a False
  where
    a = initKeyState
