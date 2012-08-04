#lang racket

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define plus
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else (add1 (plus n (sub1 m)))])))

(define minus
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else (sub1 (minus n (sub1 m)))])))


(define add_tuple
  (lambda (tuple)
    (cond
      [(null? tuple) 0]
      [else (plus (car tuple) (add_tuple (cdr tuple)))])))

(define x
  (lambda (n m)
    (cond
      [(zero? m) 0]
      [else (plus n (x n (sub1 m)))])))

(define tuple+
  (lambda (tup1 tup2)
    (cond
      [(null? tup1) tup2]
      [(null? tup2) tup1]
      [else (cons (plus (car tup1) (car tup2)) (tuple+ (cdr tup1) (cdr tup2)))])))


(define gt
  (lambda (a b)
    (cond
        [(zero? a) #f]
        [(zero? b) #t]
        [else (gt (sub1 a) (sub1 b))])))


(define lt
  (lambda (a b)
    (cond
        [(zero? b) #f]
        [(zero? a) #t]
        [else (lt (sub1 a) (sub1 b))])))

(define equal
  (lambda (a b)
      (not (or (gt a b) (lt a b)))))

(define pow
  (lambda (a b)
    (cond
      [(zero? b) 1]
      [else (x a (pow a (sub1 b)))])))

(define divide
  (lambda (a b)
    (cond
      [(lt a b) 0]
      [else (add1 (divide (minus a b) b))])))

(define length
  (lambda (lat)
    (cond
      [(null? lat) 0]
      [else (add1 (length (cdr lat)))])))

(define pick
  (lambda (n lat)
    (cond
      [(zero? (sub1 n)) (car lat)]
      [else (pick (sub1 n) (cdr lat))])))
