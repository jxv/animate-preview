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
import Animate.Preview.Input
import Animate.Preview.Color
import Animate.Preview.Scalar
import Animate.Preview.Mode

data Loaded = Loaded
  { lTextToInt :: Text -> Maybe Int
  , lIntToText :: Int -> Text
  , lSpriteSheet :: Animate.SpriteSheet Int SDL.Texture Seconds
  , lTotalKeys :: Int
  }

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
  , vCurrent :: Maybe Current
  , vLoaded :: Maybe Loaded
  }

data Current = Current
  { cPos :: Animate.Position Int Seconds
  , cKeyName :: Text
  } deriving (Show, Eq)

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
  , vCurrent = Nothing
  , vLoaded = Nothing
  }

makeClassy ''Vars

type S m = MonadState Vars m