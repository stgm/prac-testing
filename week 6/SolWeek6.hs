{-
  Lab Session Software Testing 2013, Week 6
  Tuba Kaya Chomette, Sander Leer, Martijn Stegeman
  13 October 2013
-}
module SolWeek6
where

import Week6
import Lab6

import Data.Bits
import Data.List
import Control.Monad


--
-- Question 1 - time spent: 2 hours
--
{-
  Based on example pseudocode in Applied Cryptography by Bruce Schneier.
    function modular_pow(base, exponent, modulus)
      result := 1
      while exponent > 0
        if (exponent mod 2 == 1):
          result := (result * base) mod modulus
        exponent := exponent >> 1
        base = (base * base) mod modulus
      return result
  
  Sources:
    www.khanacademy.org/math/applied-math/cryptography/modarithmetic/a/fast-modular-exponentiation
    en.wikipedia.org/wiki/Modular_exponentiation
    Schneier, Bruce (1996). Applied Cryptography: Protocols, Algorithms, and Source Code in C, Second Edition (2nd ed.). Wiley
    
  Remarks:
    Profiling using Criterion package showed that the following code implementations are faster
      1) using "odd e" instead of "e `mod` 2 == 1"
      2) using "Data.Bits.shiftR e 1" instead of "e `div` 2"
      
    Actually surprised by the second remark because would expect any (decent) compiler to recognize
    a division by 2 on an integer to be equal as a bit shift right with 1.
-}
exMfast :: Integer -> Integer -> Integer -> Integer
exMfast b e m = exM' b e m 1 where
  exM' _ 0 _ r = r
  exM' b e m r | odd e      = exM' (b*b `mod` m) (Data.Bits.shiftR e 1) m (r*b `mod` m)
               | otherwise  = exM' (b*b `mod` m) (Data.Bits.shiftR e 1) m r


--
-- Question 2 --> see Benchmark.hs
--			   


--
-- Question 3 - time spent: 2 hours
--			   
{-
    Function composites is modelled after the primes function in Week6. The sieve used returns a
    list of all integers between primes, which should indeed be all composite numbers (non-primes).
-}
composites :: [Integer]
composites = unsieve [2..] where
             unsieve (x:y:ys) = [(x+1)..(y-1)] ++ unsieve (filter (\ m -> rem m x /= 0) (y:ys))

  
--
-- Question 4 - time spent: 5 hours
--
{-
  Experimental setup:
    For each number from a list of specific numbers (composites, Carmichael number) test it using
    a primality checker (Fermat's Little Theorem or Miller-rabin Test).
    When a number is found that is (probably) prime, store it in a result list.
    Repeat the experiment several times and return the least number found.

  Result:
    A probabilistic algorithms for primality testing uses a random value to check the primality.
    It has a 50% chance of returning a positive answer (the result is composite) and 50% chance of returning a false positive answer (the result is 'probably' prime).
    The probability of returning a false positive decreases as the number of checks using a random
    number are increased; k is increased. The probability of returning a false positive is equal
    to .5^k. So the probability of returning a correct answer is equal to 1-.5^k; so for k = 5 the
    probability is 0.96875 ~ 97%.
-}

-- return a list of least composite numbers that fools Fermat's primality check for k = 1..5
checkComposite = mapM check [1..5] where
                 check k = checkPrimality primeF k composites


{-
  General function to check a list of numbers for primality and return the smallest number that
  tests as a (probably) prime.
  
  Arguments:
    f:  function to check for primality
    k:  number to pass to f which determines the number of candidates to test
    ns: list of numbers to check for primality
-}
checkPrimality :: (Int -> Integer -> IO Bool) -> Int -> [Integer] -> IO Integer
checkPrimality f k ns = lowestPrime experimentRuns f k ns [] where
                        -- create a list of all numbers that are classified as (probably) prime
                        -- and return the lowest number of this list
                        lowestPrime 0 _ _ _  xs = return (head $ sort xs)
                        lowestPrime n f k ns xs = do
                          x <- checkList f k ns
                          lowestPrime (n-1) f k ns ([x] ++ xs)

                        -- check the list of numbers for primality and
                        -- return the first numbers that returns (probably) prime
                        checkList f k (n:ns) = do
                          b <- f k n
                          if b then do
                            return n
                          else
                            checkList f k ns

                        -- number of times to repeat the experiment
                        experimentRuns = 100


--
-- Question 5 | Time spent: 1 hour
--
{-
    The function checkCarmichaelF below uses the first 10 Carmichael numbers to repeatedly check
    the effectiveness of the primeF tests. It counts the amount of positives returned by the
    primality test.
    We use a fixed value of k=5 for the probability used in the primality test functions.

    checkCarmichaelF will return something along the likes of 
        [74,97,98,98,99,98,100,100,99,99]
    which means that primeF will almost *ALWAYS* mark Carmichael numbers as prime.
-}
checkCarmichaelF = mapM countFP (take 10 carmichael) where
                   countFP = countFalsePos primeF 5 experimentRuns
                   experimentRuns = 100

{-
    General function countFalsePos uses the same setup as checkPrimality: it will count the amount
    of positives returned by the primality test. Because we are counting, the list of candidates
    should be finite (that's why we only take the first 10 Carmichael numbers in checkCarmichaelF).
-}
countFalsePos :: (Int -> Integer -> IO Bool) -> Int -> Int -> Integer -> IO Int
countFalsePos f k 0 _ = return 0
countFalsePos f k x n = do
    r <- f k n
    c <- countFalsePos f k (x-1) n
    if r then return (1 + c)
    else return c


--
-- Question 6 - time spent: 2 minutes
--
{-
    The function checkCarmichaelMR uses the first 10 Carmichael numbers to repeatedly check
    the effectiveness of the primeMR tests. It counts the amount of positives returned by the
    primality test.
    We use a fixed value of k=5 for the probability used in the primality test functions.

    checkCarmichaelMR will return something along the likes of 
        [0,0,2,0,1,0,0,0,0,0]
    which means that primeMR will almost *NEVER* mark Carmichael numbers as prime.
-}
checkCarmichaelMR = mapM countFP (take 10 carmichael) where
                    countFP = countFalsePos primeMR 5 experimentRuns
                    experimentRuns = 100

				
--
-- Question 7 | Time spent: 1 hours
--
-- we first defined a function that finds mersenne primes using primeMR
-- we send the list of prime number to look through
findMersennePrimes :: [Integer] -> IO [(Integer,Integer)]
findMersennePrimes ns = do
						bases <- filterM (\n -> primeMR 5 (2^n - 1)) ns
						return (zip bases $ map (\n -> 2^n - 1) bases)
	
-- these are the first 21 Mersenne primes
mersennePrimes = [(2,m1),(3,m2),(5,m3),(7,m4),(13,m5),(17,m6),(19,m7),(31,m8),(61,m9),(89,m10)
				,(107,m11),(127,m12),(521,m13),(607,m14),(1279,m15),(2203,m16),(2281,m17),(3217,m18)
				,(4253,m19),(4423,m20),(9689,m21)]

-- We defined some functions to check which Mersenne primes are found by our function (findMersennePrimes), when we send different lists of prime numbers to it
 	
-- finds m1 to m14
findMersennePrimes1 = do 
					result <- findMersennePrimes $take 200 primes
					return result

-- finds m15 to m18							
findMersennePrimes2 = do 
					result <- findMersennePrimes $drop 200 $take 500 primes 
					return result

-- finds m19							
findMersennePrimes3 = do 
					result <- findMersennePrimes $drop 500 $take 600 primes 
					return result

-- finds m20							
findMersennePrimes4 = do 
					result <- findMersennePrimes $drop 600 $take 700 primes 
					return result	

-- can not find any more							
findMersennePrimes5 = do 
					result <- findMersennePrimes $drop 900 $take 1000 primes 
					return result