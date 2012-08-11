#lang racket

(require "atom.rkt" "equal.rkt")

(define (multi-insert-left-right-collect new left right things collect)
  (cond
    [(null? things) (collect (list) 0 0)]
    [(equal? (car things) left) 
        (multi-insert-left-right-collect new left right (cdr things)
            (lambda (new-things num-left num-right)
              (collect (cons new (cons (car things) new-things)) (add1 num-left) num-right)))]
   [(equal? (car things) right)
        (multi-insert-left-right-collect new left right (cdr things)
            (lambda (new-things num-left num-right)
              (collect (cons (car things) (cons new new-things)) num-left (add1 num-right))))]
   [else (multi-insert-left-right-collect new left right (cdr things)
            (lambda (new-things num-left num-right)
              (collect (cons (car things) new-things) num-left num-right)))]))

(multi-insert-left-right-collect 5 1 2 (list 1 2 3 1 2 3 1 2 3) (lambda (new-things num-left num-right) (cons new-things (cons num-left (cons num-right (list))))))
