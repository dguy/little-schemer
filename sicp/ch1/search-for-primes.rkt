#lang racket

(provide search-for-primes check-prime)

(require "math.rkt")

(define (check-prime n)
    (cond 
      ((prime? n) (report n) 1)
      (else 0)))

(define (report n)
    (display "**** ")
    (displayln n)
    )

(define (search-for-primes from to)
  (define (next n)
      (+ n 2))
  (define (search n to counter)
    (cond 
      ((and (< counter 3) (<= n to)) (search (next n) to (+ counter (check-prime n))))
      (else (displayln "Finished"))))
  (define (start from)
    (if (even? from)
      (+ from 1)
      from))
  (search (start from) to 0))

