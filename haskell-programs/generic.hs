class MathObject a where
    add :: a -> a -> a
    sub :: a -> a -> a
    mul :: a -> a -> a


type Fraction = (Int, Int)
type Vector = [Int]
type Matrix = [[Int]]

data Frac = Frac Fraction deriving (Show, Eq)

data Vec = Vec Vector deriving (Show, Eq)


instance MathObject Frac where
    add (Frac (num, denom)) (Frac (num2, denom2))  = addFractions (Frac (num, denom)) (Frac (num2, denom2))
    sub (Frac (num, denom)) (Frac (num2, denom2))  = subFractions (Frac (num, denom)) (Frac (num2, denom2))
    mul (Frac (num, denom)) (Frac (num2, denom2))  = mulFractions (Frac (num, denom)) (Frac (num2, denom2))

instance MathObject Vec where 
    add (Vec v1) (Vec v2) = addVecs (Vec v1) (Vec v2)
    sub (Vec v1) (Vec v2) = subVecs (Vec v1) (Vec v2)
    mul (Vec v1) (Vec v2) = mulVecs (Vec v1) (Vec v2)


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

addVecs :: Vec -> Vec -> [Int]
addVecs (Vec []) (Vec vec2)= (Vec [])
addVecs (Vec vec) (Vec []) = (Vec [])
addVecs (Vec (x:xs)) (Vec (y:ys)) = e1+e2:rest where
  e1 = x
  e2 = y
  rest = addVecs (Vec xs) (Vec ys)

subVecs :: Vec -> Vec -> [Int]
subVecs (Vec []) (Vec v2) = (Vec [])
subVecs (Vec (x:xs)) (Vec (y:ys)) = x-y:rest where
  rest = subVecs (Vec xs) (Vec ys)

mulVecs :: Vec -> Vec -> [Vec]
mulVecs (Vec []) (Vec vec2) = (Vec [])
mulVecs (Vec (x:xs)) (Vec (y:ys)) = e1 * e2:rest where
  e1 = x
  e2 = y
  rest = mulVecs (Vec xs) (Vec ys)
