#lang racket

(provide sum)

(define (sum a b term next)
  (if (> a b)
    0
    (+ (term a)
       (sum (next a) b term next))))

