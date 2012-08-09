#lang racket

(require "lat.rkt" "eqan.rkt")

(provide eqlist)

(define (eqlist xs ys)
  (cond
    [(and (null? xs) (null? ys)) #t]
    [(or (null? xs) (null? ys)) #f]
    [(and (atom? (car xs)) (atom? (car ys))) 
     (and (eqan? (car xs) (car ys)) (eqlist (cdr xs) (cdr ys)))]
    [(and (not (atom? (car xs))) (not (atom? (car ys)))) 
     (and (eqlist (car xs) (car ys)) (eqlist (cdr xs) (cdr ys)))]
    [else #f]))
    

