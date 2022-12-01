data XExpression = ExpNumber Int | ExpVar Char | ExpE Char Char Int deriving (Eq,Show)

-- example
e :: XExpression
e = ExpE 'x' '+' 2

-- derive
derive :: XExpression -> XExpression
derive (ExpNumber n) = ExpNumber 0
derive (ExpVar v) = ExpNumber 1
derive (ExpE v op n) = ExpNumber 1

-- selectors
getVar :: XExpression -> Char
getVar (ExpE v _ _) = v

getOp :: XExpression -> Char
getOp (ExpE _ op _) = op

getNum :: XExpression -> Int
getNum (ExpE _ _ i) = i


