#lang racket

(require "equal.rkt")

(provide remove-member multi-remove-member remove-member-equal)

(define (remove-member pred?)
  (lambda (val things)
    (cond
      [(null? things) (list)]
      [(pred? val (car things)) (cdr things)] 
      [else (cons (car things) ((remove-member pred?) val (cdr things)))])))


(define (remove-member-equal val things)
  ((remove-member equal?) val things))

(define (multi-remove-member pred? val things)
  (cond
    [(null? things) (list)]
    [(pred? val (car things)) (multi-remove-member pred? val (cdr things))]
    [else (cons (car things) (multi-remove-member pred? val (cdr things)))]))
