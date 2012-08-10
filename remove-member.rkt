#lang racket

(require "equal.rkt" "lat.rkt")

(provide remove-member multi-remove-member)

(define (sequence-remove new lat)
  (cdr lat))

(define remove-member 
  (lambda (val things)
    ((insert equal? sequence-remove) #f val things)))

(define (multi-remove-member pred? val things)
  (cond
    [(null? things) (list)]
    [(pred? val (car things)) (multi-remove-member pred? val (cdr things))]
    [else (cons (car things) (multi-remove-member pred? val (cdr things)))]))
