{- Provides constants used by the CORDIC algorithm.
 - `alistF` is a list of angles [ atan 1, atan (1/2), atan (1/4, ... ]
 - `klistF` is a list of the scaling constants for each iteration
 - Traditionally these would have been hard-coded for performance; they are
 - generated programmatically here for simplicity.
 -}

module Util
(
  alistF,
  klistF
) where

import Numeric.Fixed

-- |Fixed-point cast of alist
alistF = map toFixed alist

-- |Fixed-point cast of klist
klistF = map toFixed klist

-- |Infinite list of angles with tangent ratios [1,  1/(2^i)]
alist :: [Double]
alist = [ atan ( 1 / 2 ^ e ) | e <- [ 0 .. ] ]

-- |Infinite list of scaling factors
klist :: [Double]
klist = klist' 1 ( k 0 )

-- |Recursive generator for scaling factors
klist' :: Int -> Double -> [Double]
klist' i n = n : klist' ( i + 1 ) (  k i * n )

-- |Scaling factor k at iteration i
k :: Int -> Double
k i = 1 / sqrt ( 1 +  2 ^^ (( -2 ) * i ))
