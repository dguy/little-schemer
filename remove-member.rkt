#lang racket

(require "equal.rkt")

(provide remove-member)

(define (remove-member val things)
  (cond
    [(null? things) (list)]
    [(equal? val (car things)) (cdr things)]
    [else (cons (car things) (remove-member val (cdr things)))]))

