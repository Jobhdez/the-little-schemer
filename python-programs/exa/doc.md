# the Ltup language

## Concrete Syntax of Ltup
```
exp ::= int | (- exp) | (+ exp exp) | (- exp exp)
     | (let ([var exp]) exp) | (vector type*) | (vector exp*)
     | (vector-length exp) | (vector-ref exp int) | (vector-set! exp int exp)
     | bool | (and exp exp) | (or exp exp) | (not exp) | (cmp exp exp) | (if exp exp exp)
     | (set! var exp) | (begin exp* exp) | (while exp exp)
     
bool ::= #t | #f

cmp ::= eq? | < | <= | > | >=

Ltup ::= exp
```
## abstract syntax of LTup
```
op ::= + | - | vector | vector-length | cmp | and | or | not 
exp ::= (Int int) | (Prim op (exp ...)) | (Var var) | (Let var exp exp)
     | (Bool bool) | (If exp exp exp) | (SetBang var exp) | (Begin exp* exp)
     | (WhileLoop exp exp) | (Prim vector-ref (exp (Int int)))
     | (Prim vector-set! (exp (Int int) exp))

bool ::= #t | #f
cmp ::= eq? | < | <= | > | >=
op ::= cmp | and | or | not
```
