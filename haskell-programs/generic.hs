{-

this is just a little program to get an idea of how type classes work and 
also to give me an idea of how I would implement a generic math system.
haskell is such an elegant language. I really like type classes.


-}

class MathObject a where
    add :: a -> a -> a
    sub :: a -> a -> a
    mul :: a -> a -> a

data Frac = Frac (Int, Int) deriving (Show, Eq)

data Vector = Vector [Int] deriving (Show, Eq)

data Matrix = Matrix [[Int]] deriving (Show, Eq)

data Poly = Poly [Int] deriving (Show, Eq)


instance MathObject Frac where
    add (Frac (num, denom)) (Frac (num2, denom2))  = addFractions (Frac (num, denom)) (Frac (num2, denom2))
    sub (Frac (num, denom)) (Frac (num2, denom2))  = subFractions (Frac (num, denom)) (Frac (num2, denom2))
    mul (Frac (num, denom)) (Frac (num2, denom2))  = mulFractions (Frac (num, denom)) (Frac (num2, denom2))

instance MathObject Vector where
    add = pointWise (+)
    sub = pointWise (-)
    mul = pointWise (*)

instance MathObject Matrix where
    add = matrixArith (+)
    sub = matrixArith (-)
    mul = matrixArith (*)

instance MathObject Poly where 
    add = polyArith (+)
    sub = polyArith (-)
    mul = polyArith (*)
-------------   
-- Fractions
---------------
addFractions :: Frac -> Frac -> Frac
addFractions (Frac (n, d)) (Frac (n2, d2))=
    (Frac (numerator, denominator)) where 
        numerator = (n*d2) + (n2*d)
        denominator = d*d2

subFractions :: Frac -> Frac -> Frac
subFractions (Frac (n, d)) (Frac (n2, d2)) =
    (Frac (numerator, denominator)) where 
        numerator = (n*d2) - (n2*d)
        denominator = d*d2

mulFractions :: Frac -> Frac -> Frac
mulFractions (Frac (n, d)) (Frac (n2, d2)) =
    (Frac (numerator, denominator)) where
        numerator = n*n2
        denominator = d*d2

--------------
-- vectors
---------------
pointWise :: (Int -> Int -> Int) -> (Vector -> Vector -> Vector)
pointWise f (Vector v1) (Vector v2) = Vector $ zipWith f v1 v2

---------------
-- matrix
---------------
matrixArith :: (Int -> Int -> Int) -> (Matrix -> Matrix -> Matrix)
matrixArith f (Matrix m1) (Matrix m2) = 
    Matrix $ compute m1 m2 where 
        compute = zipWith (zipWith f)

-------------
---- polynomials 
--------------

polyArith :: (Int -> Int -> Int) -> (Poly -> Poly -> Poly)
polyArith f (Poly p1) (Poly p2) =
    Poly $ zipWith f p1 p2

evalPolynomial :: Poly -> Int -> Int
evalPolynomial p1 x =
    sum terms where
        terms = evalTerms p1 x

evalTerms :: Poly -> Int -> [Int]
evalTerms (Poly p1) x =
    case p1 of
        [] -> []
        _ -> (x ^ len) * head p1:rest where 
            len = (length p1) - 1
            rest = evalTerms (Poly poly) x where 
                 poly = tail p1

derive :: Poly -> Poly
derive p1 =
    Poly exp where 
        exp = deriveTerms p1

deriveTerms :: Poly -> [Int]
deriveTerms (Poly p1) =
  if length p1 == 1 
    then [] 
    else derivedPoly where
            derivedPoly = (head p1) * exponent:rest where 
                exponent = (length p1) - 1
                rest = deriveTerms (Poly poly) where 
                    poly = tail p1