#lang racket
(require "atom.rkt")

(provide numbered?)

(define (numbered? expression)
  (cond
    [(atom? expression) (number? expression)]
    [else (and (numbered? (car expression)) (numbered? (car (cdr (cdr expression)))))]))
