#lang racket
(require "atom.rkt" "numbered.rkt" "numbers.rkt")

(provide value)

(define (1st-sub-exp aexp)
  (car (cdr aexp)))

(define (2nd-sub-exp aexp)
  (car (cdr (cdr aexp))))

(define (operator aexp)
  (car aexp))

(define (value nexp)
  (cond 
    [(atom? nexp) nexp]
    [else ((atom-to-function (operator nexp)) (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))]))

(define (atom-to-function atom)
  (cond
    [(eq? 'x atom) x]
    [(eq? '+ atom) plus]
    [else pow]))

(value (list '+ 1 2))
(value (list 'x 3 2))
(value (list '^ 3 2))
