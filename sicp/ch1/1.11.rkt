#lang racket

(provide recursive-f iterative-f)

(define (recursive-f n)
  (cond 
    ((< n 3) n)
    (else
      (+ (recursive-f (- n 1))
         (* 2 (recursive-f (- n 2)))
         (* 3 (recursive-f (- n 3)))))))

(define (iterative-f n)
  (define (f-iter a b c count)
    (if (= count 0)
      a
      (f-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
  (f-iter 0 1 2 n))


