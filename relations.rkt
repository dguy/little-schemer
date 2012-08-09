#lang racket

(require "set.rkt" "firsts.rkt" "pairs.rkt")

(provide fun? reverse-relation)

(define (fun? relation)
   (set? (firsts relation))) 

(define (reverse-relation relation)
  (cond
    [(null? relation) (list)]
    [else (cons (reverse-pair (car relation)) (reverse-relation (cdr relation)))]))
