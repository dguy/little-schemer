#lang racket
(require "atom.rkt" "numbered.rkt" "numbers.rkt")

(provide value)

(define (value aexp)
  (cond
    [(atom? aexp) aexp]
    [(eq? (car (cdr aexp)) (quote +)) 
     (plus (value (car aexp)) (value (car (cdr (cdr aexp)))))]
    [(eq? (car (cdr aexp)) (quote x)) 
     (x (value (car aexp)) (value (car (cdr (cdr aexp)))))]
    [else 
     (pow (value (car aexp)) (value (car (cdr (cdr aexp)))))]))
    

