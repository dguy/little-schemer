#lang racket

(define (compose f g)
    (lambda (x)
      (f (g x))))


(define (repeated f n)
   (define (iter v result)
     (if (< v 1)
       result
       (iter (- v 1) (compose f result))))
   (iter n identity))
