#lang racket

(provide cube halve double divisible? 
         square fast-prime? smallest-divisor 
         prime? sum sum-cubes sum-ints
         iterative-sum iter-sum-ints
         product-of-range iter-product-of-range
         factorial sum2 product2 iter-sum2 iter-product2)

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

(define (iterative-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
    (iter a 0))
      

(define (iter-sum-ints a b) (iterative-sum identity a inc b))

(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(define (iterative-product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
    (iter a 1))

(define (product-of-range a b) (product identity a inc b))
(define (iter-product-of-range a b) (iterative-product identity a inc b))
(define (factorial n) (iterative-product identity 1 inc n))

; Generic accumate that can replace sum and product from above.
(define (iterative-accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))

(define (sum2 a b) (accumulate + 0 identity a inc b))
(define (product2 a b) (accumulate * 1 identity a inc b))
(define (iter-sum2 a b) (iterative-accumulate + 0 identity a inc b))
(define (iter-product2 a b) (iterative-accumulate * 1 identity a inc b))

