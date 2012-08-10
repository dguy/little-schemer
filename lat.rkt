#lang racket

(require "atom.rkt")

(provide insert list-of-atoms? firsts insert-right multi-insert-right insert-left multi-insert-left substitute multi-substitute subst2 no-nums multi-insert)

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

(define (insert pred? sequence)
  (lambda (new value lat)
    (cond
      [(null? lat) (list)]
      [(pred? (car lat) value) (sequence new lat)]
      [else (cons (car lat) ((insert pred? sequence) new value (cdr lat)))])))
  
(define insert-right (insert equal? (lambda(new lat) (cons (car lat)(cons new (cdr lat))))))
(define insert-left  (insert equal? (lambda(new lat) (cons new lat))))


(define (multi-insert pred? sequence)
  (lambda (new value lat)
    (cond
      [(null? lat) (list)]
      [(pred? (car lat) value) (sequence new lat ((multi-insert pred? sequence) new value (cdr lat)))] 
      [else (cons (car lat) ((multi-insert pred? sequence) new value (cdr lat)))])))
     
(define (sequence-right new lat future)
  (cons (car lat) (cons new future)))

(define (sequence-left new lat future)
  (cons new (cons (car lat) future)))

(define multi-insert-right (multi-insert equal? sequence-right))
(define multi-insert-left (multi-insert equal? sequence-left))

(define substitute (insert equal? (lambda(new lat) (cons new (cdr lat)))))
(define multi-substitute (multi-insert equal? (lambda(new lat future) (cons new future))))

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

(define (concat x-things y-things)
  (cond
    [(null? x-things) y-things]
    [else (cons (car x-things) (concat (cdr x-things) y-things))]))
