{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

module PromptWindow (promptWindow) where

import Foreign.C.Types (CInt)
import Data.Text       (Text, pack, singleton)
import Control.Monad   (mapM)

import SDL
import SDL.Font      qualified as F
import SDL.Primitive qualified as P

import PromptState (SelectionState, toggle, step, selY, selN)
import Selection (Selection(..))
import Waves (sinwaveI, coswaveI, ellipsewaveV2, rectwaveV2)
import Blendable (blend)

width, height :: CInt
(width, height) = (640, 400)

-- Position
type Pos = V2 CInt
-- Dimension
type Dim = Pos

promptWindow :: String -> IO Bool
promptWindow question =
  do initializeAll
     F.initialize

     window <- createWindow (pack $ "prompt : " ++ question) defaultWindow { windowBorder = False
                                                                           , windowInitialSize = V2 width height }
     renderer <- createRenderer window (-1) defaultRenderer
     font <- F.load "/usr/share/fonts/TTF/Crimson-Roman.ttf" 42

     response <- promptLoop renderer font question (Y, 1, 0) 0

     destroyWindow window
     F.quit
     quit

     pure response

promptLoop :: Renderer    -- renderer to display things onto
           -> F.Font      -- font to display with
           -> String      -- the prompt to display
           -> SelectionState -- state (besides iterator)
           -> Int         -- iterator
           -> IO Bool     -- true if y is selected, false if n is
promptLoop renderer font prompt state t =
  do rendererDrawColor renderer $= V4 0 0 0 255
     clear renderer

     renderPrompt renderer font prompt t
     yDim <- renderY renderer font t
     nDim <- renderN renderer font t
     renderSelection renderer state yDim nDim t

     present renderer

     -- ~60 frames per second
     delay 16

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
     if yPressed || enterPressed && (selY state) then pure True
       else if nPressed || enterPressed && (selN state) then pure False
       else if tabPressed then (promptLoop renderer font prompt (toggle state) (t + 1))
       else (promptLoop renderer font prompt (step state) (t + 1))

renderPrompt :: (Integral a)
             => Renderer
             -> F.Font
             -> String
             -> a
             -> IO ()
renderPrompt renderer font prompt i =
  do surfs <- mapM ((F.blended font (V4 0 0 255 255)) . singleton) prompt
     dims  <- mapM surfaceDimensions surfs

     let bounds = map (\d -> (V2 (width - 5) (height `div` 3)) - d) dims
         points = map (\(b, j) -> (V2 0 (height `div` 6)) + (ellipsewaveV2 b 600 (i + 250 + (j * 6))))
                      (zip bounds [0..])
         rects = map (\(p, d) -> (Rectangle (P p) d))
                     (zip points dims)

     textures <- mapM (createTextureFromSurface renderer) surfs

     mapM_ (\(t,b) -> copy renderer t Nothing (Just b)) (zip textures rects)
     -- box demarcating ellipse boundaries
     P.rectangle renderer (V2 0 (height `div` 6))
                          (V2 (width - 5) ((height `div` 6) + (height `div` 3)))
                          (V4 0 0 255 255)


     mapM_ freeSurface surfs
     mapM_ destroyTexture textures

yPos, nPos :: V2 CInt
yPos = V2 (width   `div` 4)
          ((height `div` 4) * 3)
nPos = V2 ((width  `div` 4) * 3)
          ((height `div` 4) * 3)

renderY :: (Integral a)
        => Renderer
        -> F.Font
        -> a
        -> IO Dim
renderY renderer font t =
  do surf <- F.blended font (V4 0 255 127 (1 + (sinwaveI 255 300 t))) "y"
                                        -- ^-- 0 alpha means opaque, so add 1
     dim  <- surfaceDimensions surf

     let box = (Rectangle (P yPos) dim)

     tex <- createTextureFromSurface renderer surf

     copy renderer tex Nothing (Just box)

     freeSurface surf
     destroyTexture tex

     pure dim

renderN :: (Integral a)
        => Renderer
        -> F.Font
        -> a
        -> IO Dim
renderN renderer font t =
  do surf <- F.blended font (V4 255 0 127 (1 + (sinwaveI 255 300 (t + 100)))) "n"
                                        -- ^-- 0 alpha means opaque, so add 1
     dim  <- surfaceDimensions surf

     let box = (Rectangle (P nPos) dim)

     tex <- createTextureFromSurface renderer surf

     copy renderer tex Nothing (Just box)

     freeSurface surf
     destroyTexture tex

     pure dim

-- jesus christ this shit sucks
-- TODO: do state better
--       - make this function not have 3 case expressions
--       - fix box jumping when selection switches in middle of transition
--         - maybe track blendFactor rather than/in addition to t of last selection
-- TODO: implement Colour to blend colors and stuff
renderSelection :: Renderer
                -> SelectionState
                -> Dim -- dimensions of displayed "y"
                -> Dim -- dimensions of displayed "n"
                -> Int
                -> IO ()
renderSelection renderer (s, r, _) yDim nDim t =
  let margins = pure 5 :: Dim
      padding = V2 5 0 :: Dim
      blue = (V4 0 0 255 255)
      cyan = (V4 0 255 127 255)
      magenta = (V4 255 0 127 255)
      box1RelPos = rectwaveV2 margins 60 t
      box2RelPos = rectwaveV2 margins 60 (t + 30)

      (pos, dim, color) = case s of
        Y -> (blend nPos yPos r, blend nDim yDim r, blend magenta cyan r)
        N -> (blend yPos nPos r, blend yDim nDim r, blend cyan magenta r)
   in do P.rectangle renderer (pos + box1RelPos - padding - margins) (pos + box1RelPos + dim + padding) blue
         P.rectangle renderer (pos + box2RelPos - padding - margins) (pos + box2RelPos + dim + padding) color
