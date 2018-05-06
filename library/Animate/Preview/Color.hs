module Animate.Preview.Color where

import Linear
import Data.Word (Word8)

data Mono
  = Mono'Black
  | Mono'Gray
  | Mono'White
  deriving (Show, Eq, Enum, Bounded)

toggleMono :: Mono -> Mono
toggleMono = \case
  Mono'White -> Mono'Black
  m -> succ m

fromMono :: Mono -> (V4 Word8, V4 Word8)
fromMono = \case
  Mono'White -> (V4 0xff 0xff 0xff 0xff, V4 0xdd 0xdd 0xdd 0xff)
  Mono'Gray -> (V4 0x88 0x88 0x88 0xff, V4 0x66 0x66 0x66 0xff)
  Mono'Black -> (V4 0x22 0x22 0x22 0xff, V4 0x00 0x00 0x00 0xff)

data Color
  = Color'Red
  | Color'Green
  | Color'Blue
  | Color'Magenta
  | Color'Yellow
  | Color'Cyan
  deriving (Show, Eq, Enum, Bounded)

fromColor :: Color -> V4 Word8
fromColor = \case
  Color'Red -> V4 0xff 0x00 0x00 0xff
  Color'Green -> V4 0x00 0xff 0x00 0xff
  Color'Blue -> V4 0x00 0x00 0xff 0xff
  Color'Magenta -> V4 0xff 0x00 0xff 0xff
  Color'Yellow -> V4 0xff 0xff 0x00 0xff
  Color'Cyan -> V4 0x00 0xff 0xff 0xff

toggleColors :: Maybe Color -> Maybe Color
toggleColors Nothing = Just Color'Red
toggleColors (Just c) = case c of
    Color'Cyan -> Nothing
    _ -> Just $ succ c