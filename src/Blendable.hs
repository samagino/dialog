module Blendable (Blendable, blend) where

import Foreign.C.Types (CInt)
import Data.Word (Word8)
import SDL.Vect (V2(..), V4(..))

class Blendable a where
  blend :: (RealFrac b) => a -> a -> b -> a

instance Blendable CInt where
  blend a b r = let r' = 1 - r
                   in round $ r' * (fromIntegral a) + r * (fromIntegral b)

instance Blendable Word8 where
  blend a b r = let r' = 1 - r
                   in round $ r' * (fromIntegral a) + r * (fromIntegral b)

instance (Blendable a) => Blendable (V2 a) where
  blend (V2 a_x a_y) (V2 b_x b_y) r = V2 (blend a_x b_x r) (blend a_y b_y r)

instance (Blendable a) => Blendable (V4 a) where
  blend (V4 a_1 a_2 a_3 a_4) (V4 b_1 b_2 b_3 b_4) r =
    V4 (blend a_1 b_1 r) (blend a_2 b_2 r) (blend a_3 b_3 r) (blend a_4 b_4 r)
