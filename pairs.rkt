#lang racket

(require "atom.rkt")

(provide a-pair?)

(define (a-pair? things)
  (and (not (atom? things))
       (not (null? things))
       (not (null? (cdr things)))
       (null? (cdr (cdr things)))))

(define (first pair)
  (car pair))

(define (second pair)
  (car (cdr pair)))

(define (build-pair one two)
  (cons one (cons two (list))))
