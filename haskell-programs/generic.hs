{-

this is just a little program to get an idea of how type classes work and 
also to give me an idea of how I would implement a generic math system.
haskell is such an elegant language. I really like type classes.


-}

class MathObject a where
    add :: a -> a -> a
    sub :: a -> a -> a
    mul :: a -> a -> a


type Fraction = (Int, Int)

data Frac = Frac Fraction deriving (Show, Eq)

data Vector = Vector [Int] deriving (Show, Eq)

data Matrix = Matrix [[Int]] deriving (Show, Eq)


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


pointWise :: (Int -> Int -> Int) -> (Vector -> Vector -> Vector)
pointWise f (Vector v1) (Vector v2) = Vector $ zipWith f v1 v2


matrixArith :: (Int -> Int -> Int) -> (Matrix -> Matrix -> Matrix)
matrixArith f (Matrix m1) (Matrix m2) = 
    Matrix $ add m1 m2 where 
        add = zipWith (zipWith f)
