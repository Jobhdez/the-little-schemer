{-

this is a tiny symbolic differentiator system. as you can see, it covers
the product and quotient rules, and addition and subtraction of derivatives.
this is nothing serious --- I built this to pick up haskell :-)

-}

data Expr = PNum Int | PVar Char | CExpr Expr Expr Expr deriving (Eq,Show)


-- differentiation:
-- `derivative` takes the derivative of EXPR with respect to x
derivative :: Expr -> Expr
derivative (PNum n) = PNum 0
derivative (PVar v) = PNum 1
derivative (CExpr v (PVar op) n) = 
    if op == '+'
        then CExpr (derivative v) (PVar op) (derivative n)
        else if op == '-' 
            then CExpr (derivative v) (PVar op) (derivative n)
        else if op == '/' 
            then CExpr (CExpr (CExpr (derivative v) (PVar '*') n) (PVar '+') (CExpr v (PVar '*') (derivative n))) (PVar '/') (CExpr n (PVar '^') (PNum 2))
        else if op == '^' 
            then CExpr (CExpr n (PVar '*') v) (PVar '^') (PNum ((getNum n) - 1))
        else CExpr (CExpr v (PVar '*') (derivative n)) (PVar '+') (CExpr n (PVar '*') (derivative v))

getNum :: Expr -> Int
getNum x = 
    case x of
        PNum i -> i

-- simplification
-- `simplify`, well, simplifies the output of `derivative`
simplify :: Expr -> Expr
simplify (PNum n) = (PNum n)
simplify (PVar v) = (PVar v)
simplify (CExpr (PVar v) (PVar op) (PNum n)) =
    if op == '+' 
        then if n == 0 
            then (PVar v)
            else CExpr (PVar v) (PVar op) (PNum n)
    else if op == '-' 
        then if n == 0 then (PVar v) else CExpr (PVar v) (PVar op) (PNum n)
    else if op == '*' then if n == 0 then PNum 0 else CExpr (PVar v) (PVar op) (PNum n)
    else if op == '^' then if n == 0 then (PNum 1) else if n == 1 then (PVar v) else (CExpr (PVar v) (PVar op) (PNum n))
    else
        if n == 1 then (PVar v) else CExpr (PVar v) (PVar op) (PNum n)
simplify (CExpr (PNum n) (PVar op) (PVar v)) =
    if op == '+' 
        then if n == 0 
            then (PVar v)
            else CExpr (PVar v) (PVar op) (PNum n)
    else if op == '-' 
        then if n == 0 then (PVar v) else CExpr (PVar v) (PVar op) (PNum n)
    else if op == '*' then if n == 0 then PNum 0 else CExpr (PVar v) (PVar op) (PNum n)
    else
        if n == 1 then (PVar v) else CExpr (PVar v) (PVar op) (PNum n)
simplify (CExpr (PNum n1) (PVar op) (PNum n2)) =
    if op == '+' 
        then if n2 == 0 
            then (PNum n1)
            else if n1 == 0 then (PNum n2)
            else (PNum (n1 + n2))
    else if op == '-' 
        then if n2 == 0 
            then (PNum n1)
            else if n1 == 0 then (PNum (-n2))
            else (PNum (n1 - n2))
    else if op == '^'
        then if n2 == 0 then (PNum 1) else if n2 == 1 then (PNum n1) else PNum (n1^n2)
    else if op == '*' then if n1 == 0 then (PNum 0) else if n2 == 0 then (PNum 0) else PNum (n1 * n2)
    else
        if n1 == 1 then (PNum n2) else if n2 == 1 then (PNum n1) else (PNum (n1 * n2))

simplify (CExpr x op y) =
    CExpr (simplify x) op (simplify y)

-- print Exprs
printExpr :: Expr -> String
printExpr (PNum n) = show n
printExpr (PVar v) = show v
printExpr (CExpr (PNum n) (PVar op) (PNum n2)) =
    show n ++ " " ++ show op ++ " " ++ show n2
printExpr (CExpr (CExpr (PNum n) (PVar op) (PNum n2)) (PVar op2) (PNum n3)) =
    "(" ++ show n ++ " "  ++ show op ++ " " ++ show n2 ++ ")" ++ " " ++ show op2 ++ " " ++ show n3
printExpr (CExpr (CExpr (PVar n) (PVar op) (PNum n2)) (PVar op2) (PNum n3)) =
    "(" ++ show n ++ " "  ++ show op ++ " " ++ show n2 ++ ")" ++ " " ++ show op2 ++ " " ++ show n3