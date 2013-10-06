{-
  Lab Session Software Testing 2013, Week 5
  Tuba Kaya Chomette, Sander Leer, Martijn Stegeman
  6 October 2013
-}
module Week5Sol_Q1_Q2
where

import Data.List
import Week5
import System.Random
import Control.Monad

--
-- Question 1 | Time spent: 1 hour
--

mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)	

lengthProp :: Ord a => [a] -> [a] -> Bool
lengthProp xs ys= (length xs) == (length ys)

sublistProp1:: Ord a => [a] -> [a] -> Bool
sublistProp1 xs ys = sublist xs ys

permutation :: Eq a => [a] -> [a] -> Bool
permutation xs ys = and [elem x ys | x <- xs]

mergeSrtA ::  Ord a => [a] -> [a]
mergeSrtA = assert1 lengthProp 
			$ assert1 sublistProp1 
			$ assert1 permutation 
			$ post1 sorted mergeSrt

--
-- Question 2 | Time spent: 2 hours
--

split :: [a] -> ([a],[a])
split xs = let n = (length xs) `div` 2
				in (take n xs, drop n xs)
				
mergeSrt' :: Ord a =>  [a] -> [a]
mergeSrt' [] = []
mergeSrt' [x] = [x]
mergeSrt' xs | (length xs) < 2 = xs
			 | otherwise = let (b,c) = (Week5Sol_Q1_Q2.split xs) 
							in (merge (mergeSrt' b) (mergeSrt' c))

mergeSrtA' ::  Ord a => [a] -> [a]
mergeSrtA' = 	assert1 lengthProp 
				$ assert1 sublistProp1  
				$ post1 (\ ys -> (sorted ys)) mergeSrt'

-- some random testing
-- works with the assertions

autoTest :: Int -> ([Int] -> [Int]) -> [[Int]] -> IO ()
autoTest n _ [] = putStrLn ("  " ++ show n ++ " tests passed")
autoTest n p (f:fs) = do putStrLn (show (p f)) 
                         autoTest n p fs 

-- various random functions from previous weeks

getIntList :: IO [Int]
getIntList = do
    r <- newStdGen
    return (randomRs (0,15) r)

randomInts :: Int -> IO [Int]
randomInts n = liftM (take n) getIntList

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomInts :: Int -> IO [[Int]]
getRandomInts 0 = return []
getRandomInts x = do r <- Week5Sol_Q1_Q2.getRandomInt 100 >>= randomInts
                     rs <- getRandomInts (x-1)
                     return (r:rs)

main = do xxs <- getRandomInts 1000
          autoTest 1000 mergeSrtA' xxs