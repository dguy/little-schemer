#lang racket

(provide ackerman)

(define (ackerman x y)
  (cond
    ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else (ackerman (- x 1) 
                    (ackerman x (- y 1))))))
