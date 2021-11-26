#lang racket
(require "match.rkt" "env.rkt")
;<exp> ::= <var>
;       |  <int>
;       |  (let <var> <exp> <exp>)
;       |  (<op> <exp> <exp>)
;<op> ::= + | - | *
(define (interp exp env)
  (match exp
    (,var (guard (symbol? var)) (apply-env env var))
    (,int (guard (integer? int)) int)
    ((let ,x ,e ,body)
     (let* ((v (interp e env))
            (env^ (extend-env x v env)))
       (interp body env^)))
    ((,op ,e1 ,e2)
     (guard (memq op '(+ - *)))
     (let ((v1 (interp e1 env))
           (v2 (interp e2 env)))
       (case op
         ((+) (+ v1 v2))
         ((-) (- v1 v2))
         ((*) (* v1 v2)))))))