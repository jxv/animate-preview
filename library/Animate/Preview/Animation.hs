module Animate.Preview.Animation where

import qualified Animate
import Text.Printf (printf)

import Data.Aeson (FromJSON, ToJSON)

type Animations key = Animate.Animations key (Animate.SpriteClip key) Seconds

newtype Seconds = Seconds Float
  deriving (Eq, Num, ToJSON, FromJSON, Fractional, Ord)

instance Show Seconds where
  show (Seconds s) = printf "%.2f" s ++ "s"

clamp :: (Fractional a, Ord a) => a -> a -> a -> a
clamp cur min' max' = if cur > max' then max' else (if cur < min' then min' else cur)
