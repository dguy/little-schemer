#lang racket

(require "../../atom.rkt")

(define (last vals)
  (define (iter v result)
    (cond
      ((null? v) result)
      (else (iter (cdr v) v))))
  (iter vals vals))

(define (last-v vals)
  (if (= 1 (length vals))
         vals
         (last-v (cdr vals))))

(define (reverse vals)
  (cond
    ((null? (cdr vals)) vals)
    (else (append (reverse (cdr vals)) (list (car vals)))))) 

(define (deep-reverse vals)
  (cond
    ((null? vals) (list))
    ((atom? (car vals))
            (append (deep-reverse (cdr vals)) (list (car vals))))
    (else (append (deep-reverse (cdr vals)) (list (deep-reverse (car vals)))))))

(define (parity? n) (if (even? n) even? odd?))

(define (same-parity first . rest)
  (define (iter items accum parity)
    (cond
      ((null? items) accum)
      ((parity (car items))  
       (iter (cdr items) (append accum (list (car items))) parity))
      (else (iter (cdr items) accum parity))))
  (iter rest (list first) (parity? first))) 

(define (better-same-parity . vals)
  (define (same-parity v parity?)
    (cond 
      ((null? v) (list))
      ((parity? (car v))
       (cons (car v) (same-parity (cdr v) parity?)))
      (else (same-parity (cdr v) parity?))))
  (same-parity vals (parity? (car vals))))
  
(define (map proc items)
  (if (null? items) (list) 
    (cons (proc (car items)) (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* factor x)) items))

(define (for-each proc items)
  (cond
    ((null? items) #t)
    (else 
      (proc (car items)) 
      (for-each proc (cdr items)))))

(last (list 1 2 3 4 5 6))
(last-v (list 1 2 3 4 5 6))
(reverse (list 1 2 3 4 5 6))
(same-parity  1 2 3 4 5 6 7)
(better-same-parity  1 2 3 4 5 6 7)
(map (lambda (x) (+ 5 x))  (list 1 2 3 4 5 6 7))
(map (lambda (x) (* 5 x))  (list 1 2 3 4 5 6 7))
(scale-list  (list 1 2 3 4 5 6 7) 10)
(for-each (lambda (x)  (displayln x)) (list 57 321 88))
(deep-reverse (list 1 2 (list 3 4) (list 5 6)))
(deep-reverse (list (list (list 1 2) (list 7 8)) (list 3 4) (list 5 6)))
