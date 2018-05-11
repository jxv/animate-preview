module Animate.Preview.Frame where

frameDeltaSeconds :: Fractional a => a
frameDeltaSeconds = 0.016667

frameDeltaMilliseconds :: Int
frameDeltaMilliseconds = 16

msps, fps :: Int
msps = 1000
fps = 60