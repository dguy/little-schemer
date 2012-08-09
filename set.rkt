#lang racket

(require "member.rkt" "remove-member.rkt")

(provide set? makeset)

(define (set? things)
  (cond
    [(null? things) #t]
    [else (and (not (member? (car things) (cdr things))) (set? (cdr things)))]))


(define (makeset things)
  (cond
    [(null? things) (list)]
    [else (cons (car things) (makeset (multi-remove-member (car things) (cdr things))))]))

