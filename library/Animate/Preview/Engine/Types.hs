module Animate.Preview.Engine.Types where

import qualified Animate

import Data.Aeson (FromJSON, ToJSON)

type Animations key = Animate.Animations key (Animate.SpriteClip key) Seconds
type DrawSprite key m = Animate.SpriteClip key -> (Int, Int) -> m ()

newtype Seconds = Seconds Float
  deriving (Show, Eq, Num, ToJSON, FromJSON, Fractional, Ord)

clamp :: (Fractional a, Ord a) => a -> a -> a -> a
clamp cur min' max' = if cur > max' then max' else (if cur < min' then min' else cur)
