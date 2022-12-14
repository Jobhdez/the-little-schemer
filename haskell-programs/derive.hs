data Expr = PNum Int | PVar Char | CExpr Expr Expr Expr deriving (Eq,Show)


-- derive
derive :: Expr -> Expr
derive (PNum n) = PNum 0
derive (PVar v) = PNum 1
derive (CExpr v (PVar op) n) = 
    if op == '+'
        then CExpr (derive v) (PVar op) (derive n)
        else if op == '-' 
            then CExpr (derive v) (PVar op) (derive n)
        else if op == '/' 
            then CExpr (CExpr (CExpr (derive v) (PVar '*') n) (PVar '+') (CExpr v (PVar '*') (derive n))) (PVar '/') (CExpr n (PVar '^') (PNum 2))
        else CExpr (CExpr v (PVar '*') (derive n)) (PVar '+') (CExpr n (PVar '*') (derive v))
    

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


