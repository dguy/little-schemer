#lang racket

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (sqrt x)
 (define (try guess)
  (if (good-enough? guess)
    guess
    (try (improve guess))))
 (define (improve guess)
    (average guess (/ x guess)))
 (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.00000001))
 (try 1.0)) 


