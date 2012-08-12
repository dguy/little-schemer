#lang racket

(require "atom.rkt")

(define (evens-only* numbers)
  (cond
    [(null? numbers) (list)]
    [(and (atom? (car numbers)) (even? (car numbers))) (cons (car numbers) (evens-only* (cdr numbers)))]
    [(atom? (car numbers)) (evens-only* (cdr numbers))]
    [else (cons (evens-only* (car numbers)) (evens-only* (cdr numbers)))]))
                                                               

(define (evens-only*&collect numbers collector)
  (cond 
    [(null? numbers) (collector (list) 1 0)]
    [(atom? (car numbers))
     (cond
       [(even? (car numbers))
            (evens-only*&collect (cdr numbers) 
                (lambda (evens product sum)
                  (collector (cons (car numbers) evens)
                        (* (car numbers) product) sum))) ]
       [else (evens-only*&collect (cdr numbers)
                (lambda (evens product sum)
                    (collector evens product
                        (+ (car numbers) sum))))])]
    [else (evens-only*&collect (car numbers)
           (lambda (evens product sum)
             (evens-only*&collect (cdr numbers)
                                  (lambda (a-evens a-product a-sum)
                                    (collector (cons evens a-evens)
                                               (* product a-product)
                                               (+ sum a-sum))))))]))



(evens-only* (list 1 2 3 4 5 6 7 8 9 10))
(evens-only* (list (list 1 2) 1 2 (list 3 4) 3 4))
(evens-only*&collect (list (list 1 2 3) 1 2 3 (list 4 5 6) 5 6) (lambda (evens product sum)
                                                             (cons product (cons sum evens))))
