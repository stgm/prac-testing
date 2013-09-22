module Week2Solutions

where

import Week2
import Data.List


--
-- Question 1 - time spent: 1.5 hours
--

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
    | any (==0) [x,y,z]                                        = NoTriangle
-- VVZ: incorrect, counter-example: triangle 1 2 1000
	| x == y && y == z                                         = Equilateral
	| x^2 + y^2 == z^2 || x^2 + z^2 == y^2 || z^2 + y^2 == x^2 = Rectangular
-- VVZ: correct, but slightly inefficient, you could've written another function that works with sorted triples
	| x == y || y == z || z == x                               = Isosceles
	| otherwise                                                = Other

--
-- Question 1 check cases
--

checkTriangle =
            checkNoTriangle &&
            checkEquilateral &&
            checkRectangular &&
            checkIsosceles &&
            checkOther

-- VVZ: kinda correct, but 40 lines of code for something that could have been generated by a Haskell oneliner is somewhat inefficient
checkNoTriangle =
	triangle 0 0 0    == NoTriangle &&
	triangle 0 0 1    == NoTriangle &&
	triangle 0 1 0    == NoTriangle &&
	triangle 1 0 0    == NoTriangle &&
	triangle 1 1 0    == NoTriangle &&
	triangle 1 0 1    == NoTriangle &&
	triangle 0 1 1    == NoTriangle &&
	triangle (-1) 1 1 == NoTriangle &&
	triangle 1 (-1) 1 == NoTriangle &&
	triangle 1 1 (-1) == NoTriangle &&
	triangle 1 1 1    /= NoTriangle

checkEquilateral =
	triangle 1 1 0 /= Equilateral &&
	triangle 0 0 0 /= Equilateral &&
	triangle 1 2 3 /= Equilateral &&
	triangle 1 1 2 /= Equilateral &&
	triangle 1 2 1 /= Equilateral &&
	triangle 2 1 1 /= Equilateral &&
	triangle 1 1 1 == Equilateral &&
	triangle 2 2 2 == Equilateral &&
	triangle 3 3 3 == Equilateral

checkRectangular =
	triangle 0 0 0 /= Rectangular &&
	triangle 1 1 2 /= Rectangular &&
	triangle 3 4 5 == Rectangular &&
	triangle 4 3 5 == Rectangular &&
	triangle 5 4 3 == Rectangular &&
	triangle 5 12 13 == Rectangular

checkIsosceles =
	triangle 1 1 1 /= Isosceles &&
	triangle 2 2 2 /= Isosceles &&
	triangle 1 1 2 == Isosceles &&
	triangle 1 2 1 == Isosceles &&
	triangle 2 1 1 == Isosceles

-- VVZ: Y U NO have positive tests?
-- VVZ: btw, this is where generation could have helped you enormously: within triangles with sides between 1 and 5 there only two
-- VVZ: which are correct but belong to "other"
checkOther =
	triangle 0 0 0 /= Other &&
	triangle 1 1 1 /= Other &&
	triangle 3 4 5 /= Other &&
	triangle 1 1 2 /= Other

--
-- Question 2 - Time spent: 2 hours
--

contradiction :: Form -> Bool
-- VVZ: correct, but a shorter notation would be "contradiction = not . satisfiable"
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

--
-- Question 2 check cases
--

checkQuestion2 = 
            checkContradiction && 
            checkTautology && 
            checkEntails && 
            checkEquiv 

-- these formulas are proved during the workshop
form_Contradiction = Cnj [p, Neg p]
form_Tautology = Dsj [p, Neg p]
form1_Implication = Cnj [p, Neg p]
form2_Implication = Dsj [p, Neg p]
form1_Equivalence = Neg (Cnj [Neg p,Neg q])
form2_Equivalence = Dsj [p, q]

checkContradiction =
	contradiction form_Contradiction &&
	not(contradiction form_Tautology) &&
	not(contradiction form1) &&
	not(contradiction form2) &&
	not(contradiction form3)

checkTautology =
	tautology form_Tautology &&
	not(tautology form_Contradiction) 

checkEntails =
	not(entails form_Tautology form_Contradiction) &&
	entails form1_Implication form2_Implication

checkEquiv =
	equiv form1_Equivalence form2_Equivalence &&
	not(equiv form1_Equivalence form_Contradiction) &&
            -- equivalence theorem formulas (see Theorem 2.10, Haskell Road to Logic, p.46)
            equiv (p) (Neg (Neg p)) && -- 1
            equiv (p) (Cnj [p, p]) && -- 2a
            equiv (p) (Dsj [p, p]) && -- 2b
            equiv (Impl p q) (Dsj [Neg p, q]) && -- 3a
            equiv (Neg (Impl p q)) (Cnj [p, Neg q]) && -- 3b
            equiv (Impl (Neg p) (Neg q)) (Impl q p) && -- 4a
            equiv (Impl p (Neg q)) (Impl q (Neg p)) && -- 4b
            equiv (Impl (Neg p) q) (Impl (Neg q) p) && -- 4c
            equiv (Equiv p q) (Cnj [(Impl p q), (Impl q p)]) && -- 5a
            equiv (Equiv p q) (Dsj [(Cnj [p, q]), (Cnj [Neg p, Neg q])]) && -- 5b
            equiv (Cnj [p, q]) (Cnj [q, p]) && -- 6a
            equiv (Dsj [p, q]) (Dsj [q, p]) && -- 6b
            equiv (Neg (Cnj [p, q])) (Dsj [Neg p, Neg q]) && -- 7a
            equiv (Neg (Dsj [p, q])) (Cnj [Neg p, Neg q]) && -- 7b
            equiv (Cnj [p, Cnj [q, r]]) (Cnj [Cnj [p, q], r]) && -- 8a
            equiv (Dsj [p, Dsj [q, r]]) (Dsj [Dsj [p, q], r]) && -- 8b
            equiv (Cnj [p, Dsj [q, r]]) (Dsj [Cnj [p, q], Cnj [p, r]]) && -- 9a
            equiv (Dsj [p, Cnj [q, r]]) (Cnj [Dsj [p, q], Dsj [p, r]]) -- 9b

--
-- Question 3
--

-- precondition: input is arrow-free and in nnf
-- VVZ: incorrect, missing lots of cases. the simplest counterexample is from the first from above: (Neg (Neg p))
-- GROUP: according to the precondition the input must be arrow-free and in nnf, so no (Neg (Neg p)) can not occur anymore for the input and hence we don’t have to check it here anymore (same goes for De Morgan law which is already handled in nnf function)

cnf:: Form -> Form 
cnf (Prop x) = Prop x
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj [f1,f2]) = dist f1 f2

-- VVZ: treating of disjunction is incorrect: the Haskell implementation allows for three and more elements in a clause
-- GROUP: Fixed this by adding the following line:
cnf (Dsj (f:fs))  = dist f (cnf (Dsj fs))
-- VVZ: looks to me like it should be
-- VVZ: cnf (Dsj (f:fs)) = dist (cnf f) (cnf (Dsj fs))

cnf f	          = f

-- precondition: input is in cnf
-- VVZ: incorrect, works correctly only on two-element lists, which is not reflected by the type (hence, the spec and the program don't agree)
-- VVZ: also, a binary version could have been simplified (you don't need to use map if you know there are only two elements)
-- GROUP : Fixed by making dist function accept two Form parameters

dist:: Form -> Form -> Form
dist (Cnj [f1,f2]) f3 = Cnj [dist f1 f3, dist f2 f3]
dist f1 (Cnj [f2,f3]) = Cnj [dist f1 f2, dist f1 f3]
dist f1 f2 	      = Dsj [f1,f2]

-- function to convert any form to cnf
-- VVZ: incorrect, counterexample: cnf (Cnj [Cnj [p,q], q])

-- GROUP: 
-- Is it not correct that the result is the same as the form for the counterexample you give here? This example form is already in CNF, isn't it?
-- Or do you mean that the nested conjunctions need to be flattened out? So the cnf results in Cnj [p,q,q] or Cnj [p,q]?? 
-- Because the grammar of CNF does not - according to us - indicate this. 
-- VVZ: The grammar of CNF is slightly different than the data type definition in Haskell:
-- VVZ: it needs to allow nested conjunctions since the conjunction there is binary
-- VVZ: In the implementation, we go for the list-based conjunction/disjunction, which is easier to operate on a computer.
fromAnyFormToCnf:: Form -> Form
fromAnyFormToCnf f = cnf (nnf (arrowfree f))

--
-- Question 3 check cases
--
-- VVZ: incorrect!
-- VVZ: only tests if the transformation to CNF preserves the equivalence
-- VVZ: never tests if the result actually conforms to the definition of CNF
-- VVZ: (and it does not for many, try to run "cnf form1_Equivalence")
-- GROUP: Almost fixed.. functionsBoundWithCnj function needs to be still implemented to make the check complete.

checkCnfConvertionSampleForms = map checkCnf [form_Tautology, form1_Implication, form1_Equivalence, form_Contradiction] 

checkCnf :: Form -> Bool
checkCnf f =
	equiv f f'&&
	checkConformityToCNF f'
	where f' = fromAnyFormToCnf f


-- we need to check whether nnf and arrowFree were applied correctly, 
-- also whether functions are bound with conjunctions together, so disjunctions should be pushed to the inside functions 
checkConformityToCNF :: Form -> Bool
checkConformityToCNF f = nnfApplied f' &&
			 arrowFreeApplied f'&&
			 functionsBoundWithCnj f' 
			 where f' = show f
			
-- VVZ: this is a bit hacky: I advise to find another way than to work on pretty-printed representations
-- VVZ: we can talk more about limitations of lexical analysis w.r.t. syntactic one at the next lab
-- VVZ: also, do not confuse infix with prefix (also with affix, postfix and confix ;) )

nnfApplied :: String -> Bool
nnfApplied s | isInfixOf "-*" s = False
			  | isInfixOf "-+" s = False
			  | isInfixOf "-(" s = False
			  | otherwise = True


arrowFreeApplied :: String -> Bool
arrowFreeApplied s | isInfixOf "==>" s = False
			    | isInfixOf "<=>" s = False
			    | otherwise = True

-- Don't know yet how to check this
functionsBoundWithCnj :: String -> Bool
functionsBoundWithCnj s = True
