#lang racket

(require "atom.rkt" "eqan.rkt")

(provide left-most)

(define (left-most things)
  (cond
    [(atom? (car things)) (car things)]
    [else (left-most (car things))]))

