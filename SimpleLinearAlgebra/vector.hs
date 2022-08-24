
-- vectors

-- add vector to scalar
type Vector = [Int]
type Scalar = Int

addScalar :: Vector -> Scalar -> Vector
addScalar vec scalar = map (+scalar) vec

mulVecScalar :: Vector -> Scalar -> Vector
mulVecScalar vec scalar = map (*scalar) vec

-- add vector by a vector
addVecs :: Vector -> Vector -> Vector
addVecs [] vec2 = []
addVecs vec [] = []
addVecs (x:xs) (y:ys) = e1+e2:rest where
  e1 = x
  e2 = y
  rest = addVecs xs ys

subVecs :: Vector -> Vector -> Vector
subVecs [] v2 = []
subVecs (x:xs) (y:ys) = x-y:rest where
  rest = subVecs xs ys

multiplyVecs :: Vector -> Vector -> Vector
multiplyVecs [] vec2 = []
multiplyVecs (x:xs) (y:ys) = e1 * e2:rest where
  e1 = x
  e2 = y
  rest = multiplyVecs xs ys

--- matrices

type Matrix = [[Int]]

addMatrices :: Matrix -> Matrix -> Matrix
addMatrices [] m1 = []
addMatrices (x:xs) (y:ys) = addVecs x y:rest where
  rest = addMatrices xs ys

subMatrices :: Matrix -> Matrix -> Matrix
subMatrices [] m2 = []
subMatrices (x:xs) (y:ys) = subVecs x y:rest where
  rest = subMatrices xs ys

mulMxScalar :: Matrix -> Scalar -> Matrix
mulMxScalar [] s = []
mulMxScalar (x:xs) s = mulVecScalar x s:rest where
  rest = mulMxScalar xs s
