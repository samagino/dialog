{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

module Lib (windowPrompt) where

import Foreign.C.Types (CInt)
import Data.Text       (Text, pack, singleton)
import Control.Monad   (mapM)

import SDL
import SDL.Font      qualified as F
import SDL.Primitive qualified as P

import Selection (Selection, Selection(..), toggle)
import Waves (sinwave, coswave, elipse, boxish)

width, height :: CInt
(width, height) = (640, 400)

-- Position
type Pos = V2 CInt
-- Dimension
type Dim = Pos

windowPrompt :: String -> IO Bool
windowPrompt question = do
  initializeAll
  F.initialize

  window <- createWindow (pack $ "prompt : " ++ question) defaultWindow { windowBorder = False, windowInitialSize = V2 width height }
  renderer <- createRenderer window (-1) defaultRenderer
  font <- F.load "/usr/share/fonts/TTF/Crimson-Roman.ttf" 42

  response <- promptLoop renderer font question Y 0

  destroyWindow window
  F.quit
  quit

  pure response


renderPrompt :: (Integral a) 
             => Renderer 
             -> F.Font 
             -> String
             -> a
             -> IO ()
renderPrompt renderer font prompt i =
  do surfs <- mapM ((F.blended font (V4 0 0 255 255)) . singleton) prompt
     dims  <- mapM surfaceDimensions surfs

     let bounds = map (\d -> (V2 (width - 9) (height `div` 3)) - d) dims
         points = map (\(b, j) -> (V2 0 (height `div` 6)) + (elipse b 500 (i + 250 + (j * 6)))) 
                      (zip bounds [0..])
         boxes = map (\(p, d) -> (Rectangle (P p) d))
                     (zip points dims)

     textures <- mapM (createTextureFromSurface renderer) surfs

     mapM_ (\(t,b) -> copy renderer t Nothing (Just b)) (zip textures boxes)
     -- box demarcating elipse boundaries
     P.rectangle renderer (V2 0 (height `div` 6))
                          (V2 (width - 9) ((height `div` 6) + (height `div` 3))) 
                          (V4 0 0 255 255)


     mapM_ freeSurface surfs
     mapM_ destroyTexture textures

renderY :: (Integral a)
        => Renderer
        -> F.Font
        -> Selection
        -> a
        -> IO Dim
renderY renderer font sel i =
  do surf <- F.blended font (V4 0 255 127 (1 + (sinwave 255 200 i))) "y"
                                        -- ^-- 0 alpha means opaque, so add 1
     dim  <- surfaceDimensions surf

     let pos = V2 (width   `div` 4) 
                  ((height `div` 4) * 3)
         box = (Rectangle (P pos) dim)

     tex <- createTextureFromSurface renderer surf

     copy renderer tex Nothing (Just box)

     freeSurface surf
     destroyTexture tex

     pure dim

renderN :: (Integral a)
        => Renderer
        -> F.Font
        -> Selection
        -> a
        -> IO Dim
renderN renderer font sel i =
  do surf <- F.blended font (V4 255 0 127 (1 + (sinwave 255 200 (i + 100)))) "n"
                                        -- ^-- 0 alpha means opaque, so add 1
     dim  <- surfaceDimensions surf

     let pos = V2 ((width  `div` 4) * 3)
                  ((height `div` 4) * 3)
         box = (Rectangle (P pos) dim)

     tex <- createTextureFromSurface renderer surf

     copy renderer tex Nothing (Just box)

     freeSurface surf
     destroyTexture tex

     pure dim

renderSelection :: (Integral a)
                => Renderer
                -> Selection
                -> Dim
                -> Dim
                -> a
                -> IO ()
renderSelection renderer sel yDim nDim i =
  do let margin = 5 :: CInt
     case sel of
       Y -> let circleBounds = yDim + V2 (margin * 2) (margin * 2)
                pos = V2 (width   `div` 4) 
                         ((height `div` 4) * 3)
                posInBox1 = boxish circleBounds 100 i
                posInBox2 = boxish circleBounds 100 (i + 5)
                posInBox3 = boxish circleBounds 100 (i + 50)
                posInBox4 = boxish circleBounds 100 (i + 55)
            in do P.fillCircle renderer (pos + posInBox1 - (V2 margin margin)) 3 (V4 0 0 255 255)
                  P.fillCircle renderer (pos + posInBox2 - (V2 margin margin)) 3 (V4 0 0 255 255)
                  P.fillCircle renderer (pos + posInBox3 - (V2 margin margin)) 3 (V4 0 255 127 255)
                  P.fillCircle renderer (pos + posInBox4 - (V2 margin margin)) 3 (V4 0 255 127 255)
       N -> let circleBounds = nDim + V2 (margin * 2) (margin * 2)
                pos = V2 ((width  `div` 4) * 3)
                         ((height `div` 4) * 3)
                posInBox1 = boxish circleBounds 100 i
                posInBox2 = boxish circleBounds 100 (i + 5)
                posInBox3 = boxish circleBounds 100 (i + 50)
                posInBox4 = boxish circleBounds 100 (i + 55)
            in do P.fillCircle renderer (pos + posInBox1 - (V2 margin margin)) 3 (V4 0 0 255 255)
                  P.fillCircle renderer (pos + posInBox2 - (V2 margin margin)) 3 (V4 0 0 255 255)
                  P.fillCircle renderer (pos + posInBox3 - (V2 margin margin)) 3 (V4 255 0 127 255)
                  P.fillCircle renderer (pos + posInBox4 - (V2 margin margin)) 3 (V4 255 0 127 255)


promptLoop :: Renderer  -- renderer to display things onto
           -> F.Font    -- font to display with
           -> String    -- the prompt to display
           -> Selection -- option (either y or n) currently selected
           -> Int       -- iterator
           -> IO Bool   -- true if y is selected, false if n is
promptLoop renderer font prompt sel i = do
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer

  renderPrompt renderer font prompt i
  yDim <- renderY renderer font sel i
  nDim <- renderN renderer font sel i
  renderSelection renderer sel yDim nDim i


  present renderer

  delay 25

  events <- pollEvents
  let isKeyPress keycode event =
        case eventPayload event of
          KeyboardEvent kbe ->
            keyboardEventKeyMotion kbe == Pressed &&
            keysymKeycode (keyboardEventKeysym kbe) == keycode
          _ -> False
      yPressed     = any (isKeyPress KeycodeY) events
      nPressed     = any (isKeyPress KeycodeN) events
      tabPressed   = any (isKeyPress KeycodeTab) events
      enterPressed = any (isKeyPress KeycodeReturn) events

  -- TODO: add mouse support (click y/n to do what y/n does)
  --       also change selection on hover (i.e. if mouse hovers 
  --       over y, move selection to y)
  -- y means do the command given as argument
  -- n means don't do that command
  if yPressed || enterPressed && sel == Y then pure True
  else if nPressed || enterPressed && sel == N then pure False
  else if tabPressed then (promptLoop renderer font prompt (toggle sel) (i + 1))
  else (promptLoop renderer font prompt sel (i + 1))
