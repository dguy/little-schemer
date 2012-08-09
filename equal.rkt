#lang racket

(require "atom.rkt" "eqan.rkt")

(provide equal?)

(define (equal? x y)
  (cond
    [(and (atom? x) (atom? y)) (eqan? x y)]
    [(or (atom? x) (atom? y)) #f]
    [else (eqlist x y)]))

(define (eqlist xs ys)
  (cond
    [(and (null? xs) (null? ys)) #t]
    [(or (null? xs) (null? ys)) #f]
    [else (and (equal? (car xs) (car ys)) (eqlist (cdr xs) (cdr ys)))]))
    

