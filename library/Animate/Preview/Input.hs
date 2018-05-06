module Animate.Preview.Input where

import KeyState

data Input = Input
  { iSpace :: KeyState Int
  , iUp :: KeyState Int
  , iDown :: KeyState Int
  , iEscape :: KeyState Int
  , iReload :: KeyState Int
  , iOrigin :: KeyState Int
  , iOutline :: KeyState Int
  , iQuit :: Bool
  } deriving (Show, Eq)

initInput :: Input
initInput = Input a a a a a a a False
  where
    a = initKeyState
