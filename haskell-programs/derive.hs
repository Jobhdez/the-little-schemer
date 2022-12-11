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
        else CExpr (CExpr v (PVar '*') (derive n)) (PVar '+') (CExpr n (PVar '*') (derive v))
    




