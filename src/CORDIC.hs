module CORDIC where

import Data.Bits ( shift )
import Numeric.Fixed

-- Create a list of angles with tangent ratios [1,  1/(2^n)]
angleDeltas :: Int -> [Double]
angleDeltas n = [ atan ( 1 / 2 ^ e ) | e <- [ 0 .. n ] ]

-- An infinite list of scaling factors
klist :: [Double]
klist = klist' 1 ( k 0 )

-- Recursive generator for scaling factors
klist' :: Int -> Double -> [Double]
klist' i n = n : klist' ( i + 1 ) (  k i * n )

-- Scaling factor k at iteration i
k :: Int -> Double
k i = 1 / sqrt ( 1 +  2 ^^ (( -2 ) * i ))

-- 'Matrix' multiplication step
mult :: Int -> Fixed -> (Fixed, Fixed) -> (Fixed, Fixed)
mult i sign (x, y) = let
    sign' = if sign < 0
      then negate
      else id
    x' = x - sign' ( shiftFixed y ( -i ))
    y' = sign' ( shiftFixed x ( -i )) + y
  in (x', y')

-- Shift function which handles wrapping/unwrapping fixed-point values
shiftFixed :: Fixed -> Int -> Fixed
shiftFixed n i = Fixed ( shift ( getFixed n ) i )
