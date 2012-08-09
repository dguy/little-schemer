#lang racket
(define (eq?-c a)
  (lambda (x)
    (eq? x a)))
