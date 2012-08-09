#lang racket
(require "lat.rkt" "eqan.rkt")

(provide remove-member* insert-after* occur* substitute* insert-before* member*)

(define (remove-member* a things)
  (cond
    [(null? things) (list)]
    [(atom? (car things))
     (cond
        [(eq? a (car things)) (remove-member* a (cdr things))]
        [else (cons (car things) (remove-member* a (cdr things)))])
       ]
     [else (cons (remove-member* a (car things)) (remove-member* a (cdr things)))]))


(define (insert-after* new old things)
  (cond
    [(null? things) (list)]
    [(atom? (car things))
     (cond
       [(eq? old (car things)) (cons old (cons new (insert-after* new old (cdr things))))]
       [else (cons (car things) (insert-after* new old (cdr things)))])]
    [else (cons (insert-after* new old (car things)) (insert-after* new old (cdr things)))]))

(define (occur* value things)
  (cond
    [(null? things) 0]
    [(atom? (car things))
     (cond
       [(eqan? value (car things)) (add1 (occur* value (cdr things)))]
       [else (occur* value (cdr things))])]
    [else (+ (occur* value (car things (occur* value (cdr things)))))]))
       

(define (substitute* old new things)
  (cond
    [(null? things) (list)]
    [(atom? (car things))
     (cond 
       [(eqan? old (car things)) (cons new (substitute* new old (cdr things)))]
       [else (cons (car things) (substitute* new old (cdr things)))])]
    [else (cons (substitute* new old (car things)) (substitute* new old (cdr things)))]))


(define (insert-before* new old things)
  (cond
    [(null? things) (list)]
    [(atom? (car things))
     (cond
       [(eqan? old (car things)) (cons new (cons old (insert-before* old new (cdr things))))]
       [else (cons (car things) (insert-before* old new (cdr things)))])]
    [else (cons (insert-before* old new (car things)) (insert-before* old new (cdr things)))]))

(define (member* value things)
  (cond
    [(null? things) #f]
    [(atom? (car things))
       (or (eqan? value (car things)) (member* value (cdr things)))]
    [else (or (member* value (car things)) (member* value (cdr things)))]))
       
