#lang racket
(require "match.rkt")
;<exp> ::= <int>
;       |  (<op> <exp> <exp>)
;<op> ::= + | - | *
(define (interp exp)
  (match exp
    (,int (guard (integer? int)) int)
    ((,op ,e1 ,e2)
     (guard (memq op '(+ - *)))
     (let* ((v1 (interp e1))
            (v2 (interp e2)))
       (case op
         ((+) (+ v1 v2))
         ((-) (- v1 v2))
         ((*) (* v1 v2)))))))
