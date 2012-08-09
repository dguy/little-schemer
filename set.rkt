#lang racket

(require "member.rkt" "remove-member.rkt")

(provide set? makeset subset? eqset? intersect? intersection union diff)

(define (set? things)
  (cond
    [(null? things) #t]
    [else (and (not (member? (car things) (cdr things))) (set? (cdr things)))]))


(define (makeset things)
  (cond
    [(null? things) (list)]
    [else (cons (car things) (makeset (multi-remove-member (car things) (cdr things))))]))


(define (subset? setx sety)
  (cond
    [(null? setx) #t]
    [else (and (member? (car setx) sety) (subset? (cdr setx) sety))]))

(define (eqset? setx sety)
    (and (subset? setx sety) (subset? sety setx)))

(define (intersect? setx sety)
  (cond
    [(null? setx) #f]
    [else (or (member? (car setx) sety) (intersect? (cdr setx) sety))]))

(define (intersection setx sety)
  (cond
    [(null? setx) (list)]
    [(member? (car setx) sety) (cons (car setx) (intersection (cdr setx) sety))]
    [else (intersection (cdr setx) sety)]))

(define (union setx sety)
  (cond
    [(null? setx) sety]
    [(member? (car setx) sety) (union (cdr setx) sety)]
    [else (cons (car setx) (union (cdr setx) sety))]))

(define (diff setx sety)
  (cond
    [(null? setx) (list)]
    [(member? (car setx) sety) (diff (cdr setx) sety)]
    [else (cons (car setx) (diff (cdr setx) sety))]))

(define (intersect-all sets)
  (cond
    [(null? (cdr sets)) (car sets)]
    [else (intersection (car sets) (intersect-all (cdr sets)))])) 
