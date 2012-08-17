#lang racket

(provide gcd)

(define (gcd a b)
  (cond
    ((= b 0) a)
    (else (gcd b (remainder a b)))))

