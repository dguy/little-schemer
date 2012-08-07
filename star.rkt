#lang racket
(require "lat.rkt")

(provide rember* insert-after*)

(define (rember* a l)
  (cond
    [(null? l) (list)]
    [(atom? (car l))
     (cond
        [(eq? a (car l)) (rember* a (cdr l))]
        [else (cons (car l) (rember* a (cdr l)))])
       ]
     [else (cons (rember* a (car l)) (rember* a (cdr l)))]))


(define (insert-after* new old l)
  (cond
    [(null? l) (list)]
    [(atom? (car l))
     (cond
       [(eq? old (car l)) (cons old (cons new (insert-after* new old (cdr l))))]
       [else (cons (car l) (insert-after* new old (cdr l)))])]
    [else (cons (insert-after* new old (car l)) (insert-after* new old (cdr l)))]))
