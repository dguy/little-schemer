#lang racket

(provide recursive-fib iterative-fib)

(define (recursive-fib n)
  (cond 
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (recursive-fib (- n 1))
             (recursive-fib (- n 2))))))


(define (iterative-fib n)
  (define (fib-iter a b count)
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))
