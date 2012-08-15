#lang racket

(require "pairs.rkt")

(provide new-entry extend-table lookup-in-table)

(define new-entry build-pair)

(define (lookup-in-entry-help key keys values not-found)
  (cond
    [(null? keys) (not-found key)]
    [(eq? (car keys) key) (car values)]
    [else (lookup-in-entry-help key (cdr keys) (cdr values) not-found)]))

(define (lookup-in-entry key entry not-found)
    (lookup-in-entry-help key
                          (first entry)
                          (second entry)
                          not-found))

(define extend-table cons)

(define (lookup-in-table name table not-found)
  (cond
    [(null? table) (not-found name)]
    [else (lookup-in-entry
            name (car table)
            (lambda (name)
              (lookup-in-table name (cdr table) not-found)))]))
