module Animate.Preview.Frame where

frameDeltaSeconds :: Fractional a => a
frameDeltaSeconds = 0.016667 * 2

frameDeltaMilliseconds :: Int
frameDeltaMilliseconds = 33

msps, fps :: Int
msps = 1000
fps = 30