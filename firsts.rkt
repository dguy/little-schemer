#lang racket

(require "atom.rkt")

(provide firsts)

(define (firsts things)
  (cond
    [(null? things) (list)]
    [else (cons (car (car things)) (first (cdr things)))]))
