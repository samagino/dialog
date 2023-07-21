{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

module Lib (windowPrompt) where

import Foreign.C.Types (CInt)
import Data.Text       (Text, pack, singleton)
import Control.Monad   (mapM)

import SDL
import SDL.Font      qualified as F
import SDL.Primitive qualified as P

width, height :: CInt
(width, height) = (640, 400)

wave :: (Floating a, RealFrac a, Integral b, Integral c, Integral d)
     => (a -> a) -- a trig function (or any function that will map a cycle to a band)
     -> b        -- desired bandwidth to expand into
     -> b        -- desired length of cycle fed into trig function
     -> c        -- curent position in cycle (the 'x' value)
     -> d        -- current position on band (the 'y' value)
wave func bandwidth cyclelength =
  let toRadians  = (((2 * pi) / (fromIntegral cyclelength)) *)
      recenter   = (/ 2) . (1 +)
      factorBand = ((fromIntegral bandwidth) *)

   in floor . factorBand . recenter . func . toRadians . fromIntegral

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

windowPrompt :: String -> IO Bool
windowPrompt question = do
  initializeAll
  F.initialize

  window <- createWindow (pack $ "prompt : " ++ question) defaultWindow { windowBorder = False, windowInitialSize = V2 width height }
  renderer <- createRenderer window (-1) defaultRenderer
  font <- F.load "/usr/share/fonts/TTF/Crimson-Roman.ttf" 42

  response <- promptLoop renderer font question 0

  destroyWindow window
  F.quit
  quit

  return response


renderPrompt :: (Integral a) 
             => Renderer 
             -> F.Font 
             -> String
             -> a
             -> IO ()
renderPrompt renderer font prompt i =
  do surfs <- mapM ((F.blended font (V4 0 0 255 255)) . singleton) prompt
     dims  <- mapM surfaceDimensions surfs

     let bounds = map (\d -> (V2 width (height `div` 3)) - d - (V2 9 0)) dims
         points = map (\(b, j) -> (V2 0 (height `div` 6)) + (elipse b 500 (i + 250 + (j * 6)))) 
                      (zip bounds [0..])
         boxes = map (\(p,d) -> (Rectangle (P p) d))
                     (zip points dims)

     textures <- mapM (createTextureFromSurface renderer) surfs

     mapM_ (\(t,b) -> copy renderer t Nothing (Just b)) (zip textures boxes)

     mapM_ freeSurface surfs
     mapM_ destroyTexture textures

renderY :: (Integral a)
        => Renderer
        -> F.Font
        -> a
        -> IO ()
renderY renderer font i =
  do surf <- F.blended font (V4 0 255 127 (1 + (sinwave 255 200 i))) "y"
                                        -- ^-- 0 alpha means opaque, so add 1
     dim  <- surfaceDimensions surf

     let box = (Rectangle (P (V2 (width `div` 4)      
                                 ((height `div` 4) * 3)))
                          dim)

     tex <- createTextureFromSurface renderer surf

     copy renderer tex Nothing (Just box)
    
     freeSurface surf
     destroyTexture tex

renderN :: (Integral a)
        => Renderer
        -> F.Font
        -> a
        -> IO ()
renderN renderer font i =
  do surf <- F.blended font (V4 255 0 127 (1 + (sinwave 255 200 (i + 100)))) "n"
                                        -- ^-- 0 alpha means opaque, so add 1
     dim  <- surfaceDimensions surf

     let box = (Rectangle (P (V2 ((width `div` 4) * 3)
                                 ((height `div` 4) * 3)))
                          dim)

     tex <- createTextureFromSurface renderer surf

     copy renderer tex Nothing (Just box)

     freeSurface surf
     destroyTexture tex

promptLoop :: Renderer -> F.Font -> String -> Int -> IO Bool
promptLoop renderer font prompt i = do
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer

  renderPrompt renderer font prompt i
  renderY renderer font i
  renderN renderer font i

  -- box demarcating elipse boundaries
  P.rectangle renderer (V2 0 (height `div` 6))
                       (V2 (width - 9) ((height `div` 6) + (height `div` 3))) 
                       (V4 0 0 255 255)

  present renderer

  delay 25

  events <- pollEvents
  let isKeyPress keycode event =
        case eventPayload event of
          KeyboardEvent kbe ->
            keyboardEventKeyMotion kbe == Pressed &&
            keysymKeycode (keyboardEventKeysym kbe) == keycode
          _ -> False
      yPressed = any (isKeyPress KeycodeY) events
      nPressed = any (isKeyPress KeycodeN) events

  -- TODO: add mouse support (click y/n to do what y/n does)
  -- TODO: add TAB/ENTER support (use rectangle to demarcate current selection)
  --       also move focus to one option on hover
  -- y means do the command given as argument
  -- n means don't do that command
  if yPressed then return True
  else if nPressed then return False
  else (promptLoop renderer font prompt (i + 1))
