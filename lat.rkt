#lang racket

(require "atom.rkt")

(provide list-of-atoms? firsts insert-right multi-insert-right insert-left multi-insert-left substitute multi-substitute subst2 no-nums)

(define list-of-atoms?
  (lambda (lat)
    (cond
      [(null? lat) #t]
      [(atom? (car lat)) (list-of-atoms? (cdr lat))]
      [else #f])))

(define firsts
  (lambda (ll)
    (cond
      [(null? ll) (list)]
      [else (cons (car (car ll)) (firsts(cdr ll)))])))

(define (insert-right pred?)
  (lambda (new value lat)
    (cond
      [(null? lat) (list)]
      [(pred? (car lat) value) (cons (car lat) (cons new (cdr lat)))]
      [else (cons (car lat) ((insert-right pred?) new value (cdr lat)))])))

(define (multi-insert-right pred?) 
  (lambda (new value lat)
    (cond
      [(null? lat) (list)]
      [(pred? (car lat) value) (cons (car lat) (cons new ((multi-insert-right pred?) new value (cdr lat))))]
      [else (cons (car lat) ((multi-insert-right pred?) new value (cdr lat)))])))

(define (insert-left pred?)
  (lambda (new value lat)
    (cond
      [(null? lat) (list)]
      [(pred? (car lat) value) (cons new lat)]
      [else (cons (car lat) ((insert-left pred?) new value (cdr lat)))])))

(define (multi-insert-left pred?)
  (lambda (new value lat)
    (cond
      [(null? lat) (list)]
      [(pred? (car lat) value) (cons new(cons (car lat) ((multi-insert-left pred?) new value (cdr lat))))]
      [else (cons (car lat) ((multi-insert-left pred?) new value (cdr lat)))])))

(define (substitute pred?)
  (lambda (new value lat)
    (cond 
      [(null? lat) (list)]
      [(pred? (car lat) value) (cons new (cdr lat))]
      [else (cons (car lat) ((substitute pred?) new value (cdr lat)))])))

(define (multi-substitute pred?)
  (lambda (new value lat)
    (cond
      [(null? lat) (list)]
      [(pred? (car lat) value) (cons new ((multi-substitute pred?) new value (cdr lat)))]
      [else (cons (car lat) ((multi-substitute pred?) new value (cdr lat)))])))

(define subst2
  (lambda (new first second lat)
    (cond
      [(null? lat) (list)]
      [(or (eq? first (car lat)) (eq? second (car lat))) (cons new (cdr lat))]
      [ else (cons (car lat) (subst2 new first second (cdr lat)))])))

(define (no-nums lat)
  (cond
    [(null? lat) (list)]
    [(number? (car lat)) (no-nums (cdr lat))]
    [else (cons (car lat) (no-nums (cdr lat)))]))
