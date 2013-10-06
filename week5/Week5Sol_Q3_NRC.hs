{-
  Lab Session Software Testing 2013, Week 5
  Tuba Kaya Chomette, Sander Leer, Martijn Stegeman
  6 October 2013
-}
module Week5Sol_Q3_NRC
where

import Data.List


{-
  Question 3 - time spent: 4 hours

  The extra constraint for NRC sudokus can be defined as follows:
    every subgrid[i,j] with i,j ranging over 2..4 and 6..8 should contain each number in {1...9}

  In the source code this is implemented in the function 'consistent' which validates if a given
  sudoku is consistent with the defined constraints. And the function 'prune' which removes
  solutions based on the constraints from the 'solutions space'.
  The effects of changing these two functions then propagates into the rest of the code.
-}
type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]

positions, values :: [Int]
positions = [1..9]
values    = [1..9] 

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

--UPDATED: definition of NRC subgrids
blocksNRC :: [[Int]]
blocksNRC = [[2..4],[6..8]]

showDgt :: Value -> String
showDgt 0 = " "
showDgt d = show d

--UPDATED: show extra 'markers' for NRC subgrids
showRowNRC :: [Value] -> IO()
showRowNRC [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showDgt a1) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showDgt a2) ; putChar ' '
     putStr (showDgt a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showDgt a4) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showDgt a5) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showDgt a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showDgt a7) ; putChar ' '
     putStr (showDgt a8) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showDgt a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

--UPDATED: show extra 'markers' for NRC subgrids
showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showDgt a1) ; putChar ' '
     putChar ' '         ; putChar ' '
     putStr (showDgt a2) ; putChar ' '
     putStr (showDgt a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showDgt a4) ; putChar ' '
     putChar ' '         ; putChar ' '
     putStr (showDgt a5) ; putChar ' '
     putChar ' '         ; putChar ' '
     putStr (showDgt a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showDgt a7) ; putChar ' '
     putStr (showDgt a8) ; putChar ' '
     putChar ' '         ; putChar ' '
     putStr (showDgt a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

--UPDATED: show extra 'markers' for NRC subgrids
showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+---------+-----------+---------+")
    showRow as
    putStrLn ("|   +-----|---+   +---|-----+   |")
    showRowNRC bs
    showRowNRC cs
    putStrLn ("+---------+-----------+---------+")
    showRowNRC ds
    putStrLn ("|   +-----|---+   +---|-----+   |")
    showRow es
    putStrLn ("|   +-----|---+   +---|-----+   |")
    showRowNRC fs
    putStrLn ("+---------+-----------+---------+")
    showRowNRC gs
    showRowNRC hs
    putStrLn ("|   +-----|---+   +---|-----+   |")
    showRow is
    putStrLn ("+---------+-----------+---------+")

type Sudoku = (Row,Column) -> Value

sud2grid :: Sudoku -> Grid
sud2grid s = 
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c) 
  where 
  pos :: [[a]] -> (Row,Column) -> a 
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

bl :: Int -> [Int]
bl x = concat $ filter (elem x) blocks 

subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) = 
  [ s (r',c') | r' <- bl r, c' <- bl c ]

--UPDATED: get the whole block of a NRC subgrid
blNRC :: Int -> [Int]
blNRC x = concat $ filter (elem x) blocksNRC

--UPDATED: pick the NRC subgrid of a position in a sudoku
subGridNRC :: Sudoku -> (Row,Column) -> [Value]
subGridNRC s (r,c) = 
  [ s (r',c') | r' <- blNRC r, c' <- blNRC c ]

freeInSeq :: [Value] -> [Value]
freeInSeq seq = values \\ seq 

freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r = 
  freeInSeq [ s (r,i) | i <- positions  ]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c = 
  freeInSeq [ s (i,c) | i <- positions ]

freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

--UPDATED: free values in a NRC subgrid
freeInSubgridNRC :: Sudoku -> (Row,Column) -> [Value]
freeInSubgridNRC s (r,c) = freeInSeq (subGridNRC s (r,c))

freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) = 
  (freeInRow s r) 
   `intersect` (freeInColumn s c) 
   --UPDATED: include the available positions from a NRC subgrid
   `intersect` (freeInSubgridNRC s (r,c)) 
   `intersect` (freeInSubgrid s (r,c)) 

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

rowInjective :: Sudoku -> Row -> Bool
rowInjective s r = injective vs where 
   vs = filter (/= 0) [ s (r,i) | i <- positions ]

colInjective :: Sudoku -> Column -> Bool
colInjective s c = injective vs where 
   vs = filter (/= 0) [ s (i,c) | i <- positions ]

subgridInjective :: Sudoku -> (Row,Column) -> Bool
subgridInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (subGrid s (r,c))

--UPDATED: check (the non-zero values on) the NRC subgrids for injectivity
{-
  QUESTION: In exercise 3 they say that NRC subgrids "should yield a surjective function"
            After looking up the definition of 'surjectivity' and 'injectivity' I'm wondering what's
            the implication for the Haskell implementation. It looks like it still needs to check
            that every number in the list (NRC subgrid) is not duplicated (e.g. the same as the
            injective function).
-}
subgridNRCInjective :: Sudoku -> (Row,Column) -> Bool
subgridNRCInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (subGridNRC s (r,c))

consistent :: Sudoku -> Bool
consistent s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | 
                    r <- [1,4,7], c <- [1,4,7]]
               --UPDATED: a NRC sudoku is consistent if every NRC subgrid is injective
               --        (contains no duplicate numbers)
                ++
               [ subgridNRCInjective s (r,c) |
                    r <- [2,6], c <- [2,6]]

extend :: Sudoku -> (Row,Column,Value) -> Sudoku
extend s (r,c,v) (i,j) | (i,j) == (r,c) = v
                       | otherwise      = s (i,j)

type Constraint = (Row,Column,[Value])

type Node = (Sudoku,[Constraint])

showNode :: Node -> IO()
showNode = showSudoku . fst

solved  :: Node -> Bool
solved = null . snd

extendNode :: Node -> Constraint -> [Node]
extendNode (s,constraints) (r,c,vs) = 
   [(extend s (r,c,v),
     sortBy length3rd $ 
         prune (r,c,v) constraints) | v <- vs ]

length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = 
  compare (length zs) (length zs')

prune :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
prune _ [] = []
prune (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
  --UPDATED: prune values that are no longer possible in NRC subgrids
  | sameblockNRC (r,c) (x,y) = 
        (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : prune (r,c,v) rest
  | otherwise = (x,y,zs) : prune (r,c,v) rest

sameblock :: (Row,Column) -> (Row,Column) -> Bool
sameblock (r,c) (x,y) = bl r == bl x && bl c == bl y 

--UPDATED: check if two coordinates are in the same NRC subgrid
sameblockNRC :: (Row,Column) -> (Row,Column) -> Bool
sameblockNRC (r,c) (x,y) = blNRC r == blNRC x && blNRC c == blNRC y 

initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in 
              if (not . consistent) s then [] 
              else [(s, constraints s)]

openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,  
                            c <- positions, 
                            s (r,c) == 0 ]

constraints :: Sudoku -> [Constraint] 
constraints s = sortBy length3rd 
    [(r,c, freeAtPos s (r,c)) | 
                       (r,c) <- openPositions s ]

search :: (node -> [node]) 
       -> (node -> Bool) -> [node] -> [node]
search succ goal [] = []
search succ goal (x:xs) 
  | goal x    = x : search succ goal xs
  | otherwise = search succ goal ((succ x) ++ xs)

solveNs :: [Node] -> [Node]
solveNs = search succNode solved 

succNode :: Node -> [Node]
succNode (s,[]) = []
succNode (s,p:ps) = extendNode (s,ps) p 

solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr)

solveShowNs :: [Node] -> IO[()]
solveShowNs ns = sequence $ fmap showNode (solveNs ns)

example1 :: Grid
example1 = [[5,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example2 :: Grid
example2 = [[0,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example3 :: Grid
example3 = [[1,0,0,0,3,0,5,0,4],
            [0,0,0,0,0,0,0,0,3],
            [0,0,2,0,0,5,0,9,8], 
            [0,0,9,0,0,0,0,3,0],
            [2,0,0,0,0,0,0,0,7],
            [8,0,3,0,9,1,0,6,0],
            [0,5,1,4,7,0,0,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,4,0,0,0,9,7,0,0]]

example4 :: Grid
example4 = [[1,2,3,4,5,6,7,8,9],
            [2,0,0,0,0,0,0,0,0],
            [3,0,0,0,0,0,0,0,0],
            [4,0,0,0,0,0,0,0,0],
            [5,0,0,0,0,0,0,0,0],
            [6,0,0,0,0,0,0,0,0],
            [7,0,0,0,0,0,0,0,0],
            [8,0,0,0,0,0,0,0,0],
            [9,0,0,0,0,0,0,0,0]]

example5 :: Grid
example5 = [[1,0,0,0,0,0,0,0,0],
            [0,2,0,0,0,0,0,0,0],
            [0,0,3,0,0,0,0,0,0],
            [0,0,0,4,0,0,0,0,0],
            [0,0,0,0,5,0,0,0,0],
            [0,0,0,0,0,6,0,0,0],
            [0,0,0,0,0,0,7,0,0],
            [0,0,0,0,0,0,0,8,0],
            [0,0,0,0,0,0,0,0,9]]

--UPDATED: example NRC sudoku's
-- NRC, Saturday Nov 26, 2005
-- source: http://homepages.cwi.nl/~jve/courses/13/testing2013/lab/Lab5.pdf
example6 :: Grid
example6 = [[0,0,0,3,0,0,0,0,0],
            [0,0,0,7,0,0,3,0,0],
            [2,0,0,0,0,0,0,0,8],
            [0,0,6,0,0,5,0,0,0],
            [0,9,1,6,0,0,0,0,0],
            [3,0,0,0,7,1,2,0,0],
            [0,0,0,0,0,0,0,3,1],
            [0,8,0,0,4,0,0,0,0],
            [0,0,2,0,0,0,0,0,0]]

-- NRC sudoku with 14-clues
-- source: http://homepages.cwi.nl/~aeb/games/sudoku/nrc.html
example14 :: Grid
example14= [[0,0,0,0,0,0,0,8,0],
            [0,0,0,5,0,0,0,0,0],
            [0,0,6,0,0,0,0,0,0],
            [0,4,0,0,2,0,0,0,0],
            [0,0,0,0,4,6,0,0,0],
            [1,0,0,0,0,0,0,3,0],
            [0,0,0,8,0,0,0,9,2],
            [0,0,0,0,0,0,0,0,0],
            [5,6,0,0,0,0,0,0,0]]

-- NRC sudoku with 13-clues
-- source: http://homepages.cwi.nl/~aeb/games/sudoku/nrc.html
example13 :: Grid
example13= [[0,0,0,5,0,0,0,0,8],
            [0,6,0,0,1,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,7,0,0,0,0,0],
            [0,0,0,0,0,0,0,3,0],
            [4,0,0,0,0,0,0,0,0],
            [0,0,1,0,5,9,0,0,0],
            [0,0,0,0,0,0,8,0,0],
            [2,0,0,0,0,6,0,0,0]]

-- NRC sudoku with 12-clues
-- source: http://homepages.cwi.nl/~aeb/games/sudoku/nrc.html
example12 :: Grid
example12= [[0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,9,8,0,0,3],
            [0,0,0,2,0,0,0,0,0],
            [0,0,8,0,0,0,1,0,0],
            [0,0,0,0,0,0,0,6,0],
            [0,0,0,0,0,0,0,4,0],
            [0,0,3,0,0,0,0,0,0],
            [6,0,0,0,5,0,0,2,0]]

-- NRC sudoku with 11-clues
-- source: http://homepages.cwi.nl/~aeb/games/sudoku/nrc.html
example11 :: Grid
example11= [[0,0,0,0,0,0,0,0,0],
            [0,1,0,0,0,0,0,0,3],
            [0,0,0,0,0,8,0,0,0],
            [0,0,0,0,0,9,0,4,0],
            [0,6,0,0,0,0,0,0,0],
            [0,0,5,0,0,0,0,0,0],
            [8,0,4,3,0,0,0,0,0],
            [0,0,0,0,0,0,0,2,0],
            [0,0,0,0,0,0,0,0,0]]
