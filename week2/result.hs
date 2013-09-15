module Week2Solutions where
import Week2

--
-- Question 1 - time spent: 1.5 hours
--

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
    | any (==0) [x,y,z]                                        = NoTriangle
	| x == y && y == z                                         = Equilateral
	| x^2 + y^2 == z^2 || x^2 + z^2 == y^2 || z^2 + y^2 == x^2 = Rectangular
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

checkOther =
	triangle 0 0 0 /= Other &&
	triangle 1 1 1 /= Other &&
	triangle 3 4 5 /= Other &&
	triangle 1 1 2 /= Other

--
-- Question 2 - Time spent: 2 hours
--

contradiction :: Form -> Bool
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
cnf:: Form -> Form 
cnf (Prop x) = Prop x
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj fs) = dist (map cnf fs)
cnf f	     = f

-- precondition: input is in cnf
dist:: [Form] -> Form
dist [(Cnj fs), f2] = Cnj (map (\x -> dist [x,f2]) fs)
dist [f1, (Cnj fs)] = Cnj (map (\x -> dist [f1,x]) fs)
dist fs 	    = Dsj fs

-- function to convert any form to cnf
fromAnyFormToCnf:: Form -> Form
fromAnyFormToCnf f = cnf (nnf (arrowfree f))

--
-- Question 3 check cases
--

checkCnf =
	normEquiv form1 &&
	normEquiv form2 &&
	normEquiv form3 && 
	normEquiv form_Contradiction && 
	normEquiv form_Tautology && 
	normEquiv form1_Implication && 
	normEquiv form2_Implication && 
	normEquiv form1_Equivalence && 
	normEquiv form2_Equivalence


normEquiv :: Form -> Bool
normEquiv fs = equiv (fromAnyFormToCnf fs) fs

