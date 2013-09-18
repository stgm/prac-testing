-- Group_D1_1  Sander Leer, Tuba Kaya Chomette, Martijn Stegeman

module Sol1 where

import GS

-- exercise 1.9
maxInt :: [Int] -> Int
maxInt []     = error "empty list"
maxInt [x]    = x
maxInt (x:xs) = max x (maxInt xs)

-- exercise 1.10
removeFst :: Eq a => a -> [a] -> [a]
removeFst _ []                 = []
removeFst n (x:xs) | n == x    = xs
                   | otherwise = x : removeFst n xs

-- exercise 1.13
count :: Char -> String -> Int
count c []                 = 0
count c (x:xs) | c == x    = 1 + count c xs
               | otherwise = count c xs

-- exercise 1.14
blowup :: String -> String
blowup xs = blowupFrom xs 1

blowupFrom :: String -> Int -> String
blowupFrom [] _     = []
blowupFrom (x:xs) n = blowupChar x n ++ blowupFrom xs (n+1)

blowupChar :: Char -> Int -> String
blowupChar c 0 = ""
blowupChar c n = [c] ++ blowupChar c (n-1)

-- exercise 1.15
srtString :: [String] -> [String]
srtString [] = []
srtString xs = m : srtString (removeFst m xs)
               where m = minString xs

minString :: [String] -> String
minString [] = error "empty list given"
minString [s] = s
minString (x:xs) = min x (minString xs)

-- exercise 1.17
subString :: String -> String -> Bool
subString _ [] = False
subString xs (y:ys) | prefix xs (y:ys) = True
                    | otherwise        = subString xs ys

-- exercise 1.20
lengths :: [[a]] -> [Int]
lengths xs = map length xs

-- exercise 1.21
sumLengths :: [[a]] -> Int
sumLengths xs = sum (map length xs)
