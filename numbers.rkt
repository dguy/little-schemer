#lang racket

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define plus
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else (add1 (plus n (sub1 m)))])))

(define minus
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else (sub1 (minus n (sub1 m)))])))


(define add_tuple
  (lambda (tuple)
    (cond
      [(null? tuple) 0]
      [else (plus (car tuple) (add_tuple (cdr tuple)))])))

(define x
  (lambda (n m)
    (cond
      [(zero? m) 0]
      [else (plus n (x n (sub1 m)))])))
