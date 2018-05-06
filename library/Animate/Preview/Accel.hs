module Animate.Preview.Accel where

data Accel
  = Accel'Normal
  | Accel'Slow Int
  | Accel'Fast Int
  deriving (Show, Eq)

accelAsSecondScalar :: Accel -> Float
accelAsSecondScalar = \case
  Accel'Normal -> 1
  Accel'Slow n -> 1 - (0.1 * fromIntegral n)
  Accel'Fast n -> 1 + (0.5 * fromIntegral n)

incrementAccel :: Accel -> Accel
incrementAccel = \case
  Accel'Normal -> Accel'Fast 1
  Accel'Fast n -> Accel'Fast (min 8 (n + 1))
  Accel'Slow n -> if n <= 1 then Accel'Normal else Accel'Slow (n - 1)

decrementAccel :: Accel -> Accel
decrementAccel = \case
  Accel'Normal -> Accel'Slow 1
  Accel'Slow n -> Accel'Slow (min 9 (n + 1))
  Accel'Fast n -> if n <= 1 then Accel'Normal else Accel'Fast (n - 1)