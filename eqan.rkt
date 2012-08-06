#lang racket
(define (eqan? a b)
  (cond 
    [(and (number? a) (number? b)) (= a b)]
    [else (eq? a b)]))
