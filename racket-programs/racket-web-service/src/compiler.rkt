#lang racket

(define (diff-compile exp)
  (match exp
    [(diff (addition 'x n))
     (let* ((exp1 (allocate-memory 'x))
            (exp2 (allocate-memory n))
            (poly (initialize-poly 'x))
            (poly2 (initialize-poly n))
            (allocation (string-append (get-poly exp1) " " (get-allocation exp1)))
            (allocation2 (string-append (get-poly exp2) " " (get-allocation exp2))))
       (string-append allocation "\n" allocation "\n" poly "\n"  poly2 "\n" (compiled-addition (get-poly exp1) (get-poly exp2))))]
            
    [(diff (subtraction 'x n))
     (let* ((exp1 (diff-compile-var 'x))
            (exp2 (diff-compile-int n))))]
       
    [(diff (multiplication 'x n))
     (let* ((exp1 (diff-compile-var 'x))
            (exp2 (diff-compile-int n)))
       (string-append "poly e1 = " "*derivative(" exp2 ")" "*" exp1 ";\n"
                      "poly e2 =" "*derivative(" exp1 ")" "*" exp2))]))

