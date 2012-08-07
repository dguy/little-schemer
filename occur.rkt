#lang racket
(require "eqan.rkt")

(provide occur)

(define (occur x lat)
  (cond
    [(null? lat) 0]
    [(eqan? (car lat) x) (+ 1 (occur x (cdr lat)))]
    [else (occur x (cdr lat))]))
