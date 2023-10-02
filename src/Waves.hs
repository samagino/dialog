module Waves (sinwaveI 
             ,coswaveI
             ,ellipsewaveV2
             ,rectwaveV2) where

import SDL.Vect (V2, V2(..))

-- I means Integral, it means the function yeilds an Integral
-- maybe I'll make variants with F for floating some time
waveI :: (RealFloat a, Integral b, Integral c, Integral d)
     => (a -> a) -- a trig function (or any function that will map Z to [-1,1]
                 --                  over a period of length 2pi
     -> b        -- amplitude
     -> b        -- period
     -> c        -- curent position in cycle (the 'x' value)
     -> d        -- current position on band (the 'y' value)
waveI f amplitude period =
  let toRadians  = (((2 * pi) / (fromIntegral period)) *)
      recenter   = (/ 2) . (1 +)
      factorBand = ((fromIntegral amplitude) *)
   in round . factorBand . recenter . f . toRadians . fromIntegral

sinwaveI :: (Integral a, Integral b, Integral c) => a -> a -> b -> c
sinwaveI = waveI sin

coswaveI :: (Integral a, Integral b, Integral c) => a -> a -> b -> c
coswaveI = waveI cos

ellipsewaveV2 :: (Integral a, Integral b)
       => (V2 a) -- width/height to stay within
       -> a      -- period (larger = slower movement)
       -> b      -- current position in cycle
       -> (V2 a) -- current position in ellipse
ellipsewaveV2 box period i =
  let w = (\(V2 x _) -> coswaveI x period i) box
      h = (\(V2 _ y) -> sinwaveI y period i) box
   in (V2 w h)

rectwaveV2 :: (Integral a, Integral b)
       => (V2 a) -- width/height to stay within
       -> a      -- period (larger = slower movement)
       -> b      -- current position in cycle
       -> (V2 a) -- current position in rectangle type thing
rectwaveV2 box period i =
  let squarish_coswave = waveI (sin . (* pi) . (/ 2) . cos)
      squarish_sinwave = waveI (sin . (* pi) . (/ 2) . sin)
      w = (\(V2 x _) -> squarish_coswave x period i) box
      h = (\(V2 _ y) -> squarish_sinwave y period i) box
   in (V2 w h)

