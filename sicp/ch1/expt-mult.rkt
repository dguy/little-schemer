#lang racket

(provide fast-expt square iterative-expt mult cube)


(define (square a) (* a a))
(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (cube a) (* a (square a)))

(define (fast-expt b n)
  (cond 
    ((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))))
    (else (* b (fast-expt b (- n 1))))))

(define (iterative-expt b n)
  (define (iter-expt a b n)
    (cond
      ((= n 0) a)
      ((even? n) (iter-expt a (square b) (/ n 2)))
      (else (iter-expt (* a b) b (- n 1)))))
  (iter-expt 1 b n))



(define (* a b)
  (cond
    ((= b 0) 0)
    ((even? b) (double (* a (halve b)))) 
    (else (+ a (* a (- b 1))))))

(define (mult a b)
  (define (iter-mult product a b)
    (cond
      ((= b 0) product)
      ((even? b) (iter-mult product (double a) (halve b)))
      (else (iter-mult (+ product a) a (- b 1)))))
  (iter-mult 0 a b))
