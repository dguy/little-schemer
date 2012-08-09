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
    [(eq? (operator nexp) (quote +))
     (plus (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))]
    [(eq? (operator nexp) (quote x))
     (x (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))]
    [else
     (pow (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))]))

(value (list '+ 1 2))
(value (list 'x 3 2))
(value (list '^ 3 2))
