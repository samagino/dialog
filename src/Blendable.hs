module Blendable (Blendable, blend) where

import Foreign.C.Types (CInt)
import SDL.Vect (V2(..))

class Blendable a where
  blend :: (RealFrac b) => a -> a -> b -> a

instance Blendable CInt where
  blend a b r = let r' = 1 - r
                   in round $ r' * (fromIntegral a) + r * (fromIntegral b)

instance (Blendable a) => Blendable (V2 a) where
  blend (V2 a_x a_y) (V2 b_x b_y) r = V2 (blend a_x b_x r) (blend a_y b_y r)
