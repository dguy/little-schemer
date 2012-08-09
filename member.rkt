#lang racket
(require "equal.rkt")

(provide member?)

(define (member? value things) 
  (cond
    [(null? things) #f]
    [else (or (equal? value (car things)) (member? value (cdr things)))]))
