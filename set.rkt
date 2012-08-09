#lang racket

(require "member.rkt")

(provide set?)

(define (set? xs)
  (cond
    [(null? xs) #t]
    [else (and (not (member? (car xs) (cdr xs))) (set? (cdr xs)))]))
