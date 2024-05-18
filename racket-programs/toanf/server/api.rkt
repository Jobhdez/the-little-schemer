#lang racket
(require json)
(require web-server/servlet)
(require web-server/servlet-env)
(require "../parser/pyparser.rkt"
         "../toANF/to-anf.rkt")
(require racket/struct)

(define (parse-json-body req)
  (bytes->jsexpr (request-post-data/raw req)))

(define (get-hash-value h v)
  (hash-ref h v))

(define (post-values req)
  (define get-property
    (curry get-hash-value (parse-json-body req)))
  (define expr (get-property 'exp))
  (define ast (parse-expression expr))
  (define anf (list (syntax->anf ast)))
  (response/jsexpr
   (hasheq 'exp (format "~a" (car (syntax->anf ast))))))

(define-values (dispatch req)
  (dispatch-rules
   [("anfexps") #:method "post" post-values]))

(serve/servlet
 (lambda (req) (dispatch req))
 #:launch-browser? #f
 #:quit? #f
 #:port 8080
 #:servlet-path "/"
 #:servlet-regexp #rx"")
