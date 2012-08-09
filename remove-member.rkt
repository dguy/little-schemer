#lang racket

(require "equal.rkt")

(provide remove-member multi-remove-member)

(define (remove-member val things)
  (cond
    [(null? things) (list)]
    [(equal? val (car things)) (cdr things)]
    [else (cons (car things) (remove-member val (cdr things)))]))


(define (multi-remove-member val things)
  (cond
    [(null? things) (list)]
    [(equal? val (car things)) (multi-remove-member val (cdr things))]
    [else (cons (car things) (multi-remove-member val (cdr things)))]))
