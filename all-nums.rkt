#lang racket

(define (all-nums lat)
  (cond
    [(null? lat) (list)]
    [(number? (car lat)) (cons (car lat) (all-nums (cdr lat)))]
    [else (all-nums (cdr lat))]))
              
