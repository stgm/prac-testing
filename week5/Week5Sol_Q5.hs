{-
  Lab Session Software Testing 2013, Week 5
  Tuba Kaya Chomette, Sander Leer, Martijn Stegeman
  6 October 2013
-}
module Week5Sol_Q5
where
import Week5Sol_Q3_NRC
import Week5Sol_Q4_RandomSudokuNRC
import Control.Monad

post1 :: (b -> Bool) -> (a -> b) -> a -> b
post1 p f x = if p (f x) then f x
              else error "post1"

-- Test postconditions for generated solved NRC Sudoku
randomSudoku' :: IO Node
randomSudoku' = do [(sol, cons)] <- (rsolveNs [emptyN])
                   if (consistent sol && noBlanks sol) then
                       return (sol, cons)
                   else
                       return (error "un-sudoku generated!")

noBlanks :: Sudoku -> Bool
noBlanks s = and [ s (r,c) /= 0 | r <- positions, c <- positions]

-- Test postconditions for generated problem from the solved NRC Sudoku

genProblem' :: Node -> IO Node
genProblem' n = do x <- genProblem n
                   if (minimal x) && (uniqueSol x) && (valuesNotChanged n x) then
                       return x
                   else
                       return (error "not minimal, sorry, bug!")

getS :: Node -> Sudoku
getS (s, _) = s

minimal :: Node -> Bool
minimal n = and [not $ uniqueSol (eraseN n pos) | pos <- fp]
  where fp = filledPositions (getS n)

valuesNotChanged :: Node -> Node -> Bool
valuesNotChanged (s,_) (sp,_) = and [ (s pos) == (sp pos) | pos <- fp]
     where fp = filledPositions sp

-- Test postconditions for unique solution of the given problem
solveNs' :: [Node] -> [Node]
solveNs' = post1 (\ [(s, cons)] -> consistent s) $ 
           post1 (\ [(s, cons)] -> noBlanks s) solveNs

solveShowNs' :: [Node] -> IO[()]
solveShowNs' ns = sequence $ fmap showNode (solveNs' ns)

-- Test everything starting from "generate a solved NRC sudoku", "generate problem from it" and "solve the problem"	
main = do r <- randomSudoku'
          showNode r
          s <- genProblem' r
          showNode s
          solveShowNs' [s]
