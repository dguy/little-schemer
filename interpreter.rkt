#lang racket
(require "atom.rkt" "pairs.rkt" "table.rkt")

(define text-of second)

(define (initial-table name)
  (car (quote ())))

(define (*const e table)
  (cond
    [(number? e) e]
    [(eq? e #t) #t]
    [(eq? e #f) #f]
    [else (build-pair (quote primitive) e)]))

(define (*lambda e table)
  (build-pair (quote non-primitive)
              (cons table (cdr e))))

(define (*quote e table)
  (text-of e))

(define cond-lines-of cdr)

(define (*cond e table)
  (eval-cond (cond-lines-of e) table))

(define (*identifier e table)
  (lookup-in-table e table initial-table))

(define function-of car)
(define arguments-of cdr)

(define (*application e table)
  (apply
    (meaning (function-of e) table)
    (meaning (arguments-of e) table)))

(define table-of first)
(define formals-of second)
(define body-of third)

(define (expression-to-action e)
  (cond
    [(atom? e) (atom-to-action e)]
    [else (list-to-action e)]))
    
(define (list-to-action e)
  (cond
    [(atom? (car e))
     (cond
       [(eq? (car e) (quote quote)) *quote]
       [(eq? (car e) (quote lambda)) *lambda]
       [(eq? (car e) (quote cond)) *cond]
       [else *application])]
    [else *application]))

(define (atom-to-action e)
  (cond
    [(number? e) *const]
    [(eq? e #t) *const]
    [(eq? e #f) *const]
    [(eq? e (quote cons)) *const]
    [(eq? e (quote car)) *const]
    [(eq? e (quote cdr)) *const]
    [(eq? e (quote null?)) *const]
    [(eq? e (quote eq?)) *const]
    [(eq? e (quote atom?)) *const]
    [(eq? e (quote atom?)) *const]
    [(eq? e (quote zero?)) *const]
    [(eq? e (quote add1)) *const]
    [(eq? e (quote sub1)) *const]
    [(eq? e (quote number?)) *const]
    [else *identifier]))

(define condition first)
(define action second)

(define (else? x)
  (and (atom? x) (eq? x (quote else))))

(define (eval-cond lines table)
  (cond
    [(else? (condition (car lines)))
            (meaning (action (car lines)) table)]
    [(meaning (condition (car lines) table))
              (meaning (action (car lines) table))]
    [else (eval-cond (cdr lines) table)]))

(define (eval-list args table)
  (cond 
    [(null? args) (quote ())]
    [else (cons (meaning (car args) table) (eval-list (cdr args) table))]))

(define (primitive? l)
  (eq? (first l) (quote primitive)))

(define (non-primitive? l)
  (eq? (first l) (quote non-primitive)))

(define (:atom? arg)
  (cond
    [(atom? arg) #t]
    [(null? arg #f)]
    [(or (eq? arg (quote primitive)) (eq? (quote non-primitive)))]))

(define (apply-primitive funcs vals)
    (cond
      [(eq? funcs (quote cons)) (cons (first vals) (second vals))]
      [(eq? funcs (quote cdr)) (cdr (first vals))]
      [(eq? funcs (quote car)) (car (first vals))]
      [(eq? funcs (quote null?)) (null? (first vals))]
      [(eq? funcs (quote eq?)) (eq? (first vals) (second vals))]
      [(eq? funcs (quote zero?)) (zero? (first vals))]
      [(eq? funcs (quote add1)) (add1 (first vals))]
      [(eq? funcs (quote sub1)) (sub1 (first vals))]
      [(eq? funcs (quote atom?)) (:atom? (first vals))]
      [(eq? funcs (quote number?)) (number? (first vals))]))

(define (apply-closure closure vals)
  (meaning (body-of closure) 
           (extend-table
             (new-entry
               (formals-of closure)
               vals)
             (table-of closure))))


(define (apply f x)
    (cond
      [(primitive? f) (apply-primitive (second f) x)]
      [(non-primitive? f) (apply-closure (second f) x)]))


(define (meaning e table)
  ((expression-to-action e) e table))

(define (value e)
  (meaning e (quote ())))

