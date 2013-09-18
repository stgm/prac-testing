-- Group_D1_1  Sander Leer, Tuba Kaya Chomette, Martijn Stegeman

module Sol2 where

import GS
import TAMO

-- exercise 2.13
check1a = logEquiv1 (\ _ -> not True) (\ _ -> False)
check1b = logEquiv1 (\ _ -> not False) (\ _ -> True)
check2 = logEquiv1 (\ p -> p ==> False) (\ p -> not p)
check3a = logEquiv1 (\ p -> p || True) (\ p -> True)
check3b = logEquiv1 (\ p -> p && False) (\ p -> False)
check4a = logEquiv1 (\ p -> p || False) (\ p -> p)
check4b = logEquiv1 (\ p -> p && True) (\ p -> p)
check5 = logEquiv1 (\ p -> p || not p) (\ p -> True)
check6 = logEquiv1 (\ p -> p && not p) (\ p -> False)

-- exercise 2.15
logContradiction1 :: (Bool -> Bool) -> Bool
logContradiction1 bf = not (bf True) && not (bf False)

logContradiction2 :: (Bool -> Bool -> Bool) -> Bool
logContradiction2 bf = and [not (bf p q) | p <- [True,False], q <- [True,False]]

logContradiction3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
logContradiction3 bf = and [ not (bf p q r)  | 	p <- [True,False],
						q <- [True,False],
						r <- [True,False]]

-- exercises 2.51 - 2.53
unique :: (a -> Bool) -> [a] -> Bool
unique p xs = length (filter p xs) == 1

parity :: [Bool] -> Bool
parity xs = even (length (filter (== True) xs))

evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p xs = parity (map p xs)
