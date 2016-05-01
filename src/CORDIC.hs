module CORDIC where

import Data.Bits ( shift )
import Numeric.Fixed

-- Create a list of angles with tangent ratios [1,  1/(2^n)]
angleDeltas :: Int -> [Fixed]
angleDeltas n = [ toFixed $ atan ( 1 / 2 ^ e ) | e <- [ 0 .. n ] ]

-- Generate a list of n + 1 scaling factors
-- Limited to 8 factors by fixed-point precision
klist :: Int -> [Fixed]
klist n = let l = min n 7 in
    reverse $ foldr (\i ks -> k i * head ks : ks) [k 0] [l, l - 1 .. 1]

-- Scaling factor k at iteration i
k :: Int -> Fixed
k i = 1 / sqrt ( 1 + 1 / ( 2 ** ( 2 * fromIntegral i )))

-- 'Matrix' multiplication step
mult :: Int -> Fixed -> Fixed -> Fixed -> (Fixed, Fixed)
mult i sign x y = let
    shiftFixed n i = Fixed ( shift ( getFixed n ) i )
    sign' = if sign < 0
      then negate
      else id
    x' = x - sign' ( shiftFixed y ( -i ))
    y' = sign' ( shiftFixed x ( -i )) + y
  in (x', y')

