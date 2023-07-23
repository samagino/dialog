module Waves (sinwave 
             ,coswave
             ,elipse
             ,boxish) where

import SDL.Vect (V2, V2(..))

wave :: (Floating a, RealFrac a, Integral b, Integral c, Integral d)
     => (a -> a) -- a trig function (or any function that will map Z to [-1,1])
     -> b        -- desired bandwidth to expand into
     -> b        -- desired length of cycle fed into trig function
     -> c        -- curent position in cycle (the 'x' value)
     -> d        -- current position on band (the 'y' value)
wave f bandwidth cyclelength =
  let toRadians  = (((2 * pi) / (fromIntegral cyclelength)) *)
      recenter   = (/ 2) . (1 +)
      factorBand = ((fromIntegral bandwidth) *)
   in floor . factorBand . recenter . f . toRadians . fromIntegral

sinwave :: (Integral a, Integral b, Integral c) => a -> a -> b -> c
sinwave = wave sin

coswave :: (Integral a, Integral b, Integral c) => a -> a -> b -> c
coswave = wave cos

elipse :: (Integral a, Integral b)
       => (V2 a) -- width/height to stay within
       -> a      -- length of cycle (larger for slower movement)
       -> b      -- current position in cycle
       -> (V2 a) -- current position in elipse
elipse box cyclelength i =
  let w = (\(V2 x _) -> coswave x cyclelength i) box
      h = (\(V2 _ y) -> sinwave y cyclelength i) box
   in (V2 w h)

boxish :: (Integral a, Integral b)
       => (V2 a) -- width/height to stay within
       -> a      -- length of cycle (larger for slower movement)
       -> b      -- current position in cycle
       -> (V2 a) -- current position in box type thing
boxish box cyclelength i =
  let ctrapwave = wave (sin . (* pi) . (/ 2) . cos)
      strapwave = wave (sin . (* pi) . (/ 2) . sin)
      w = (\(V2 x _) -> ctrapwave x cyclelength i) box
      h = (\(V2 _ y) -> strapwave y cyclelength i) box
   in (V2 w h)

