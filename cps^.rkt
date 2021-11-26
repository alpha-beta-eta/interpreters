#lang racket
(require "match.rkt" "env.rkt")
;<exp> ::= <var>
;       |  (lambda (<var>) <exp>)
;       |  (<exp> <exp>)
(define (interp exp env cont)
  (match exp
    (,var
     (guard (symbol? var))
     (apply-cont cont (apply-env env var)))
    ((lambda (,x) ,body)
     (apply-cont cont (make-closure x body env)))
    ((,rator ,rand)
     (interp rator env (make-rator-cont rand env cont)))))
(define (make-end-cont) '(end-cont))
(define (make-rator-cont rand env cont)
  `(rator-cont ,rand ,env ,cont))
(define (make-rand-cont closure cont)
  `(rand-cont ,closure ,cont))
(define (apply-cont cont val)
  (match cont
    ((end-cont) val)
    ((rator-cont ,rand ,env ,cont)
     (interp rand env (make-rand-cont val cont)))
    ((rand-cont ,closure ,cont)
     (apply-closure closure val cont))))
(define (make-closure x body env)
  `(closure ,x ,body ,env))
(define (apply-closure closure arg cont)
  (match closure
    ((closure ,var ,body ,env)
     (interp body (extend-env var arg env) cont))))