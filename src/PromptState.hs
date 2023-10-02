{-# LANGUAGE ImportQualifiedPost #-}

module PromptState (SelectionState
                   ,toggle
                   ,step
                   ,selY
                   ,selN) where

-- this file describes the internal state variables of the Prompt program
-- so like, not 

import Selection qualified as S

type PromptState = (SelectionState, Int)
type SelectionState = (S.Selection, Float, Float)

selY :: SelectionState -> Bool
selY (s, _, _) = s == S.Y

selN :: SelectionState -> Bool
selN (s, _, _) = s == S.N

toggle :: SelectionState -> SelectionState
toggle (s, r, v) = (S.toggle s, 1 - r, -v)

step :: SelectionState -> SelectionState
step (s, r, v) = (s, r', v')
  where
    truncate_r x | x < 0.0 = 0.0 | x > 1.0 = 1.0 | True = x
    truncate_v x | r >= 1.0 = 0.0 | r <= 0.0 = 0.0001 | True = x
    a = 0.001 :: Float
    v' = truncate_v (v + a)
    r' = truncate_r (r + v')
