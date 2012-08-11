#lang racket

(require "atom.rkt")

(define (evens-only* numbers)
  (cond
    [(null? numbers) (list)]
    [(and (atom? (car numbers)) (even? (car numbers))) (cons (car numbers) (evens-only* (cdr numbers)))]
    [(atom? (car numbers)) (evens-only* (cdr numbers))]
    [else (cons (evens-only* (car numbers)) (evens-only* (cdr numbers)))]))
                                                               
