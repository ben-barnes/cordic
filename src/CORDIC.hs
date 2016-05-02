{- Provides the CORDIC algorithm.
 - Fixed-point math is used throughout, with bit shifting used as per the
 - original algorithm.
 - Two multiplications are performed as the final step to scale the result.
 -}

module CORDIC 
(
  cordic
) where

import Data.Bits ( shift )
import Numeric.Fixed
import Util

-- |(index, remainder, (x, y)) used in fold
type State = (Int,  Fixed, (Fixed, Fixed))

-- |Initialise (x, y) and index, execute fold, scale result
-- |Parameter `a` is the angle in radians, `n` is the number of iterations
-- |The result is a pair ( cos a, sin a )
cordic :: Fixed -> Int -> (Fixed, Fixed)
cordic a n = let
    initial = ( 0, a, (1, 0) )
    (i, _, (c, s)) = foldl step initial $ take n alistF
    k = klistF !! i
  in ( k * c, k * s )

-- |Core of the algorithm - generates next (x, y) from current
step :: State -> Fixed -> State
step (i, a, v) d
  | a > 0 = ( i', a - d, mult i   1  v )
  | a < 0 = ( i', a + d, mult i (-1) v )
  | otherwise = ( i', a, v )
    where i' = i + 1

-- |Multiplies 'vector' (x, y) by i'th rotation matrix
mult :: Int -> Fixed -> (Fixed, Fixed) -> (Fixed, Fixed)
mult i sign (x, y) = let
    sign' = if sign < 0
      then negate
      else id
    x' = x - sign' ( shiftFixed y ( -i ))
    y' = sign' ( shiftFixed x ( -i )) + y
  in (x', y')

-- |Shift function which handles wrapping/unwrapping fixed-point values
shiftFixed :: Fixed -> Int -> Fixed
shiftFixed n i = Fixed ( shift ( getFixed n ) i )
