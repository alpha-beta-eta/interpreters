#lang racket
(require "match.rkt" "env.rkt")
;<exp> ::= <var>
;       |  <int>
;       |  (lambda <var> <exp>)
;       |  (let <var> <exp> <exp>)
;       |  (<op> <exp> <exp>)
;       |  (<exp> <exp>)
;<op> ::= + | - | *
(define (interp exp env)
  (match exp
    (,var (guard (symbol? var)) (apply-env env var))
    (,int (guard (integer? int)) int)
    ((lambda ,x ,body)
     (make-closure x body env))
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
         ((*) (* v1 v2)))))
    ((,rator ,rand)
     (let* ((closure (interp rator env))
            (arg (interp rand env)))
       (apply-closure closure arg)))))
(define (make-closure x body env)
  `(closure ,x ,body ,env))
(define (apply-closure closure arg)
  (match closure
    ((closure ,x ,body ,env)
     (interp body (extend-env x arg env)))))
