#lang racket

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (lambda (f)
         (le (lambda(x) ((f f) x))))))))
