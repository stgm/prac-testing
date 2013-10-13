{-
  Lab Session Software Testing 2013, Week 6
  Tuba Kaya Chomette, Sander Leer, Martijn Stegeman
  13 October 2013
-}
module Benchmark where

import Week6

import Criterion.Main
import Criterion.Config
import Data.Bits

{-
    To actually run this benchmark, you need to install Criterion by running

        cabal install criterion

    This has a huge amount of dependencies, so it is not advised to actually
    do this.
-}

{-
    See the plot in report/plot.pdf

    The linear relation of expM's exponent size and its running time is clear
    from the right half of the plot. There seems to be a sublinear
    (logarithmic) relation between exM's exponent size and its running time.
-}

exMfast :: Integer -> Integer -> Integer -> Integer
exMfast b e m = exM' b e m 1 where
  exM' _ 0 _ r = r
  exM' b e m r | odd e      = exM' (b*b `mod` m) (Data.Bits.shiftR e 1) m (r*b `mod` m)
               | otherwise  = exM' (b*b `mod` m) (Data.Bits.shiftR e 1) m r

main = defaultMainWith defaultConfig (return ()) [
         -- bgroup "exM" [
         --   bench "exM 1" $ whnf (\ x -> exM 7919 x 257) 1
         -- , bench "exM 10" $ whnf (\ x -> exM 7919 x 257) 10
         -- , bench "exM 100" $ whnf (\ x -> exM 7919 x 257) 100
         -- , bench "exM 1000" $ whnf (\ x -> exM 7919 x 257) 1000
         -- , bench "exM 10000" $ whnf (\ x -> exM 7919 x 257) 10000
         -- , bench "exM 100000" $ whnf (\ x -> exM 7919 x 257) 100000
         -- , bench "exM 1000000" $ whnf (\ x -> exM 7919 x 257) 1000000
         -- , bench "exM 10000000" $ whnf (\ x -> exM 7919 x 257) 10000000
         -- , bench "exM 100000000" $ whnf (\ x -> exM 7919 x 257) 100000000
         -- ],
         -- bgroup "expM" [
         --   bench "expM 1" $ whnf (\ x -> expM 7919 x 257) 1
         -- , bench "expM 10" $ whnf (\ x -> expM 7919 x 257) 10
         -- , bench "expM 100" $ whnf (\ x -> expM 7919 x 257) 100
         -- , bench "expM 1000" $ whnf (\ x -> expM 7919 x 257) 1000
         -- , bench "expM 10000" $ whnf (\ x -> expM 7919 x 257) 10000
         -- , bench "expM 100000" $ whnf (\ x -> expM 7919 x 257) 100000
         -- , bench "expM 1000000" $ whnf (\ x -> expM 7919 x 257) 1000000
         -- , bench "expM 10000000" $ whnf (\ x -> expM 7919 x 257) 10000000
         -- ],
         bgroup "exMfast" [
           bench "exMfast 1" $ whnf (\ x -> exMfast 7919 x 257) 1
         , bench "exMfast 10" $ whnf (\ x -> exMfast 7919 x 257) 10
         , bench "exMfast 100" $ whnf (\ x -> exMfast 7919 x 257) 100
         , bench "exMfast 1000" $ whnf (\ x -> exMfast 7919 x 257) 1000
         , bench "exMfast 10000" $ whnf (\ x -> exMfast 7919 x 257) 10000
         , bench "exMfast 100000" $ whnf (\ x -> exMfast 7919 x 257) 100000
         , bench "exMfast 1000000" $ whnf (\ x -> exMfast 7919 x 257) 1000000
         , bench "exMfast 10000000" $ whnf (\ x -> exMfast 7919 x 257) 10000000
         , bench "exMfast 100000000" $ whnf (\ x -> exMfast 7919 x 257) 100000000
         ]
       ]
