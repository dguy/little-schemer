#lang racket

(require "atom.rkt" "set.rkt")

(provide a-pair? first second build-pair set-of-pairs? reverse-pair seconds shift)

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

(define (seconds pairs)
  (cond
    [(null? pairs) (list)]
    [else (cons (second (car pairs)) (seconds (cdr pairs)))]))

(define (shift pair)
  (build-pair (first (first pair))
    (build-pair (second (first pair)) (second pair))))

(define (align pair-or-atom)
  (cond
    [(atom? pair-or-atom) pair-or-atom]
    [(a-pair? (first pair-or-atom)) (align (shift pair-or-atom))] 
    [else (build-pair (first pair-or-atom) (align (second pair-or-atom)))]))

(define (length* pair-or-atom)
  (cond
    [(atom? pair-or-atom) 1]
    [else (+ (length* (first pair-or-atom))
             (length* (second pair-or-atom)))]))


(define (weight* pair-or-atom)
  (cond
    [(atom? pair-or-atom) 1]
    [else (+ (* (weight* (first pair-or-atom)) 2)
             (weight* (second pair-or-atom)))]))

(define (shuffle pair-or-atom)
  (cond
    [(atom? pair-or-atom) pair-or-atom]
    [(a-pair? (first pair-or-atom)) (shuffle (reverse-pair pair-or-atom))]
    [else (build-pair (first pair-or-atom) (shuffle (second pair-or-atom)))]))
    
