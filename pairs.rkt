#lang racket

(require "atom.rkt" "set.rkt")

(provide a-pair? first second build-pair set-of-pairs? reverse-pair)

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

(define (set-of-pairs? pairs)
  (cond
    [(null? pairs) #t]
    [else (and (a-pair? (car pairs))
       (set? pairs) 
       (set-of-pairs? (cdr pairs)))]))  

(define (reverse-pair pair)
  (build-pair (second pair) (first pair)))
