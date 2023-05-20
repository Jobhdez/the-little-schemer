#lang racket
(provide (all-defined-out))

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUM ID PLUS MINUS MUL DERIVATIVE))
(define-empty-tokens op-tokens (EOF))

(define the-lexer/tokens
  (lexer
   [(eof) 'EOF]
   ["+" (token-PLUS (string->symbol lexeme))]
   ["-" (token-MINUS (string->symbol lexeme))]
   ["*" (token-MUL (string->symbol lexeme))]
   ["Deriv" (token-DERIVATIVE (string->symbol lexeme))]
   [(:+ numeric) (token-NUM (string->number lexeme))]
   [(:: (:or alphabetic #\_)
        (:* (:or alphabetic numeric #\_)))
    (token-ID (string->symbol lexeme))]
   [whitespace (the-lexer/tokens input-port)]))


;;;-------------------------------------
;;; AST
;;;-------------------------------------

(struct diffmodule (expressions))

(struct diff (expression) #:transparent)

(struct addition (e1 e2) #:transparent)

(struct subtraction (e1 e2) #:transparent)

(struct multiplication (e1 e2) #:transparent)

(define the-parser
  (parser
   [start diff]
   [end EOF]
   [error void]
   [tokens value-tokens op-tokens]
   [grammar
    [diff [(DERIVATIVE expr) (diff $2)]]
    [expr [(NUM PLUS NUM) (addition $1 $3)]
          [(NUM MINUS NUM) (subtraction $1 $3)]
          [(NUM MUL NUM) (multiplication $1 $3)]
          [(ID PLUS NUM) (addition $1 $3)]
          [(ID MINUS NUM) (subtraction $1 $3)]
          [(ID MUL NUM) (multiplication $1 $3)]]]))
          
                
