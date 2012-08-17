#lang racket

(provide divisible? square fast-prime? smallest-divisor prime?)

(define (divisible? n by) (= 0 (remainder n by)))

(define (square n) (* n n))

(define (exp-mod base exponent m)
  (cond
    ((= exponent 0) 1)
    ((even? exponent)
     (remainder (square (exp-mod base (/ exponent 2) m)) m))
    (else
      (remainder (* base (exp-mod base (- exponent 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (exp-mod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
    ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))


(define (smallest-divisor n)
  (define (find-divisor n test)
    (cond
      ((> (square test) n) n)
      ((divisible? n test) test)
      (else (find-divisor n (+ 1 test)))))
  (find-divisor n 2))

(define (prime? n) (= n (smallest-divisor n)))
