module Week3Sol

where 

import Techniques
import Week3
import Week2Solutions

import Data.List
import Data.Char
import System.Random
import Control.Monad
import Data.List


--
-- Question 3 - time spent: XXX hours
--
takeM :: Monad m => Int -> m [a] -> m [a]
takeM x = liftM (take x)

getIntList :: IO [Int]
getIntList = liftM (randomRs (0,15)) getStdGen

randomInts :: Int -> IO [Int]
randomInts x = takeM x getIntList


--
-- Question 4 - time spent: XXX hours
--
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = xs `elem` permutations ys

--
-- Question 5 - time spent: XXX hours
--
-- generate a random list and check if all permutations are deemed.. permutations
randomTestPerm :: IO Bool
randomTestPerm = do
    x <- randomInts 3
    return (and (map (isPermutation x) (drop 1 (permutations x))))

-- generate a random list, add an extra number in the list and check if these two lists are never permutations of each other
randomTestPerm2 :: IO Bool
randomTestPerm2 = do
    x <- randomInts 3
    return ((not (isPermutation x (0:x))) && (not (isPermutation x (x++[0]))))


--
-- Question 6 - time spent: XXX hours
--
checkCNFs :: IO ()
checkCNFs = testForms 50 checkCnf

{-
Results

All random generated formulas pass as correct when converted to CNF formulas. But the random generated formulas also contains formulas with disjunctions or conjunctions without any arguments (e.g. +() or *()). That's to say: an AND or OR operator without arguments. This should not be a valid formula (or valid CNF). The origin of this problem lies within the random formula generator; it generates conjunctions and disjunctions with 0..5 arguments. This should clearly be [2..]. Also, the parser and data structure for formula's allow for an empty list on the number of arguments for disjunctions and conjunctions.
-}


--
-- Question 7 - time spent: XXX hours
--
getRandomBool :: IO Bool
getRandomBool = do
                  n <- getRandomInt 1
                  return (n == 1)

getRandomAtomName :: IO String
getRandomAtomName = do
                      n <- getRandomInt 25
                      return [chr (n + ord 'A')]

getRandomTermName :: IO String
getRandomTermName = do
                      n <- getRandomInt 25
                      return [chr (n + ord 'a')]

getRandomTerm :: IO Term
getRandomTerm = do
                  b <- getRandomBool
                  s <- getRandomTermName
                  if b then do
                    return (V s)
                  else do
                    t <- getRandomTerms 2
                    return (F s t)

getRandomTerms :: Int -> IO [Term]
getRandomTerms 0  = return []
getRandomTerms d  = do
                      t <- getRandomTerm
                      ts <- getRandomTerms (d-1)
                      return (t:ts)
                        
getRandomFormula :: Int -> IO Formula
getRandomFormula 0  = do
                        s <- getRandomAtomName
                        t <- getRandomTerms 2
                        return (Atom s t)
getRandomFormula d  = do
                        n <- getRandomInt 8
                        case n of 
                          0 -> do
                            s <- getRandomAtomName
                            t <- getRandomTerms 2
                            return (Atom s t)
                          1 -> do
                            t1 <- getRandomTerm
                            t2 <- getRandomTerm
                            return (Eq t1 t2)
                          2 -> do
                            f <- getRandomFormula (d-1)
                            return (Neg f) 
                          3 -> do
                            f1 <- getRandomFormula (d-1)
                            f2 <- getRandomFormula (d-1)
                            return (Impl f1 f2)
                          4 -> do
                            f1 <- getRandomFormula (d-1)
                            f2 <- getRandomFormula (d-1)
                            return (Equi f1 f2)
                          5 -> do
                            m  <- getRandomInt 5
                            fs <- getRandomFormulas (d-1) m
                            return (Conj fs)
                          6 -> do
                            m  <- getRandomInt 5 
                            fs <- getRandomFormulas (d-1) m
                            return (Disj fs)
                          7 -> do
                            s <- getRandomTermName
                            f <- getRandomFormula (d-1)
                            return (Forall s f)
                          8 -> do
                            s <- getRandomTermName
                            f <- getRandomFormula (d-1)
                            return (Exists s f)

getRandomFormulas :: Int -> Int -> IO [Formula]
getRandomFormulas _ 0 = return []
getRandomFormulas d n = do 
                          f <- getRandomFormula d
                          fs <- getRandomFormulas d (n-1) 
                          return (f:fs)

getRandomFrms :: IO Formula
getRandomFrms = do
                  d <- getRandomInt 2
                  getRandomFormula d



