#lang racket
(require "lat.rkt" "eqan.rkt")

(provide remove-member* insert-after* occur*)

(define (remove-member* a l)
  (cond
    [(null? l) (list)]
    [(atom? (car l))
     (cond
        [(eq? a (car l)) (remove-member* a (cdr l))]
        [else (cons (car l) (remove-member* a (cdr l)))])
       ]
     [else (cons (remove-member* a (car l)) (remove-member* a (cdr l)))]))


(define (insert-after* new old l)
  (cond
    [(null? l) (list)]
    [(atom? (car l))
     (cond
       [(eq? old (car l)) (cons old (cons new (insert-after* new old (cdr l))))]
       [else (cons (car l) (insert-after* new old (cdr l)))])]
    [else (cons (insert-after* new old (car l)) (insert-after* new old (cdr l)))]))

(define (occur* value l)
  (cond
    [(null? l) 0]
    [(atom? (car l))
     (cond
       [(eqan? value (car l)) (add1 (occur* value (cdr l)))]
       [else (occur* value (cdr l))])]
    [else (+ (occur* value (car l)) (occur* value (cdr l)))]))
       
