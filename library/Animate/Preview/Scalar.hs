module Animate.Preview.Scalar where

data Scalar
  = Scalar'None
  | Scalar'Down Int
  | Scalar'Up Int
  deriving (Show, Eq)

scalarToSeconds :: Scalar -> Float
scalarToSeconds = \case
  Scalar'None -> 1
  Scalar'Down n -> 1 - (0.1 * fromIntegral n)
  Scalar'Up n -> 1 + (0.5 * fromIntegral n)

scalarToSpriteScale :: Scalar -> Float
scalarToSpriteScale = \case
  Scalar'None -> 1
  Scalar'Down n -> 1 - (0.1 * fromIntegral n)
  Scalar'Up n -> 1 + (0.1 * fromIntegral n)

incrementScalar :: Int -> Scalar -> Scalar
incrementScalar upper = \case
  Scalar'None -> Scalar'Up 1
  Scalar'Up n -> Scalar'Up (min upper (n + 1))
  Scalar'Down n -> if n <= 1 then Scalar'None else Scalar'Down (n - 1)

decrementScalar :: Int -> Scalar -> Scalar
decrementScalar lower = \case
  Scalar'None -> Scalar'Down 1
  Scalar'Down n -> Scalar'Down (min lower (n + 1))
  Scalar'Up n -> if n <= 1 then Scalar'None else Scalar'Up (n - 1)