#lang racket

(require "set.rkt" "firsts.rkt" "pairs.rkt")

(provide fun? reverse-relation full-fun?)

(define (fun? relation)
   (set? (firsts relation))) 

(define (reverse-relation relation)
  (cond
    [(null? relation) (list)]
    [else (cons (reverse-pair (car relation)) (reverse-relation (cdr relation)))]))

(define (full-fun? relation)
  (and (fun? relation) (set? (seconds relation))))
