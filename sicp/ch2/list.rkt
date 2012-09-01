#lang racket

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


(last (list 1 2 3 4 5 6))
(last-v (list 1 2 3 4 5 6))
(reverse (list 1 2 3 4 5 6))

