#lang racket

(provide cube halve double divisible? 
         square fast-prime? smallest-divisor 
         prime? sum sum-cubes sum-ints)

(define (divisible? n by) (= 0 (remainder n by)))
(define (square n) (* n n))
(define (cube n) (* n (square n)))
(define (halve n) (/ n 2))
(define (double n) (+ n n))

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
  (define (next n)
    (if (= 2 n) 
      3
      (+ 2 n)))
  (define (find-divisor n test)
    (cond
      ((> (square test) n) n)
      ((divisible? n test) test)
      (else (find-divisor n (next test)))))
  (find-divisor n 2))

(define (prime? n) (= n (smallest-divisor n)))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b) (sum cube a inc b))
(define (sum-ints a b) (sum identity a inc b))


