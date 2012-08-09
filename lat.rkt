#lang racket

(require "atom.rkt")

(define list_of_atoms?
  (lambda (lat)
    (cond
      [(null? lat) #t]
      [(atom? (car lat)) (list_of_atoms? (cdr lat))]
      [else #f])))

(define remove_member
  (lambda (a lat)
    (cond
      [(null? lat) (list)]
      [(eq? a (car lat)) (cdr lat)]
      [else (cons (car lat) (remove_member a (cdr lat)))]))) 

(define multi_remove_member
  (lambda (value lat)
    (cond
      [(null? lat)(list)]
      [(eq? value (car lat)) (multi_remove_member value (cdr lat))]
      [else (cons (car lat) (multi_remove_member value (cdr lat)))]
    )))

(define firsts
  (lambda (ll)
    (cond
      [(null? ll) (list)]
      [else (cons (car (car ll)) (firsts(cdr ll)))])))

(define insert_right
  (lambda (new after_this lat)
    (cond
      [(null? lat) (list)]
      [(eq? after_this (car lat)) (cons after_this (cons new (cdr lat)))]
      [else (cons (car lat) (insert_right new after_this (cdr lat)))])))

(define multi_insert_right
  (lambda (new after lat)
    (cond
      [(null? lat) (list)]
      [(eq? after (car lat)) (cons after (cons new (multi_insert_right new after (cdr lat))))]
      [else (cons (car lat) (multi_insert_right new after (cdr lat)))])))

(define insert_left
  (lambda (new left_of lat)
    (cond
      [(null? lat) (list)]
      [(eq? left_of (car lat)) (cons new lat)]
      [else (cons (car lat) (insert_left new left_of (cdr lat)))])))

(define multi_insert_left
  (lambda (new left_of lat)
    (cond
      [(null? lat) (list)]
      [(eq? left_of (car lat)) (cons new(cons left_of(multi_insert_left new left_of (cdr lat))))]
      [else (cons (car lat) (multi_insert_left new left_of (cdr lat)))])))

(define substitute
  (lambda (new old lat)
    (cond 
      [(null? lat) (list)]
      [(eq? old (car lat)) (cons new (cdr lat))]
      [else (cons (car lat) (substitute new old (cdr lat)))])))

(define multi_substitute
  (lambda (new old lat)
    (cond
      [(null? lat) (list)]
      [(eq? old (car lat)) (cons new (multi_substitute new old (cdr lat)))]
      [else (cons (car lat) (multi_substitute new old (cdr lat)))])))

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
    
(define x (list 1 2 3 4 1 2 3 4))
(remove_member 2 x)
(remove_member 3 x)
(define z (list (list 1 2 3) (list 4 5 6)))
(firsts z)
(insert_right 6 2 x)
(insert_left 6 2 x)
(substitute 6 2 x)
(subst2 6 5 3 x)
(multi_remove_member 1 x)
(multi_insert_right 6 1 x)
(multi_insert_left 6 1 x)
(multi_substitute 6 1 x)
(no-nums (list 1 2 "a" "b" 3 4 "c" "d"))
