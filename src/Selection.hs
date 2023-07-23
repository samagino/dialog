-- This module provides the Selection type and the like one 
--      function it needs (or 2 including ==)
module Selection (Selection(..), toggle) where

data Selection = Y | N
  deriving (Eq, Show)

toggle :: Selection -> Selection
toggle Y = N
toggle N = Y
