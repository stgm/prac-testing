module Week3Sol

where 

import Techniques
import Week3
import Week2Solutions

import System.Random
import Control.Monad
import Data.List
import Data.Char

getIntList :: IO [Int]
getIntList = do
    r <- newStdGen
    return (randomRs (0,15) r)

randomInts :: Int -> IO [Int]
randomInts n = liftM (take n) getIntList

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = xs `elem` permutations ys

randomTestPerm :: IO Bool
randomTestPerm = do
    x <- randomInts 3
    return (and (map (isPermutation x) (drop 1 (permutations x))))

randomTestPerm2 :: IO Bool
randomTestPerm2 = do
    x <- randomInts 3
    return ((not (isPermutation x (0:x))) && (not (isPermutation x (x++[0]))))

{-- 
    VVZ:
        Basically your solution relies on random formulae generation (which was
        given in Techniques.pdf) and a checkCnf function of last week (which is
        still in ruins several weeks after I have explicitly warned you about
        it)

        In other words, you apply someone else's technique to test incorrect
        implementation with incorrect test function.

        This is not anywhere near a correct solution.
-}

checkCNFs :: IO ()
checkCNFs = testForms 50 checkCnf

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
