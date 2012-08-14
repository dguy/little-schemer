#lang racket

(require "atom.rkt")


(define (keep-looking val sorn lat)
  (cond
    [(number? sorn) (keep-looking val (pick sorn lat) lat)]
    [else (eq? val sorn)]))


(define (pick n lat)
  (define (index x things)
    (cond
      [(= x n) (car things)]
      [else (index (add1 x) (cdr things))]
      ))
  (index 1 lat))


(define (looking val lat)
  (keep-looking val (pick 1 lat) lat))


(looking "a" (list 6 2 4 "a" 5 7 3))
