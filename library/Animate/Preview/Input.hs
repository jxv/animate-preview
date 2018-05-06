module Animate.Preview.Input where

import KeyState
import Linear

data Input = Input
  { iSpace :: KeyState Int
  , iUp :: KeyState Int
  , iDown :: KeyState Int
  , iEscape :: KeyState Int
  , iReload :: KeyState Int
  , iOrigin :: KeyState Int
  , iOutline :: KeyState Int
  , iBackground :: KeyState Int
  , iMousePos :: V2 Int
  , iMouseClick :: KeyState Int
  , iQuit :: Bool
  } deriving (Show, Eq)

initInput :: Input
initInput = Input a a a a a a a a (V2 0 0) a False
  where
    a = initKeyState
