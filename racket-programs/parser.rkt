#lang racket
(provide the-parser)
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))


(define-tokens value-tokens (NUM ID PLUS MINUS ASSIGN PRINT))
(define-empty-tokens op-tokens (EOF))
(define-lex-abbrevs
  (digit (:/ "0" "9")))

(define the-lexer/tokens
  (lexer
   [(eof) 'EOF]
   ["+" (token-PLUS (string->symbol lexeme))]
   ["-" (token-MINUS (string->symbol lexeme))]
   ["=" (token-ASSIGN (string->symbol lexeme))]
   [(:+ numeric) (token-NUM (string->number lexeme))]
   [(:: (:or alphabetic #\_)
        (:* (:or alphabetic numeric #\_)))
    (token-ID (string->symbol lexeme))]
   [whitespace (the-lexer/tokens input-port)]))

(struct pymodule (statements))

(struct pyprint (e1))

(struct pyaddition (e1 e2))

(struct pysubtraction (e1 e2))

(struct pyassignment (var e1))

(define the-parser
  (parser
   [start mod]
   [end EOF]
   [error void]
   [tokens value-tokens op-tokens]
   [grammar
    [mod [(statements) (pymodule $1)]]
    [statements [(NUM PLUS NUM) (pyaddition $1 $3)]
                [(NUM MINUS NUM) (pysubtraction $1 $3)]
                [(ID ASSIGN NUM) (pyassignment $1 $3)]
                [(PRINT NUM) (pyprint $2)]
                [(PRINT ID) (pyprint $2)]]]))
