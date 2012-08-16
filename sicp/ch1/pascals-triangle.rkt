#lang racket

(provide pascals-triangle)

(define (pascals-triangle row col)
  (cond
    ((or (= col 1) (= col row)) 1)
    ((= col 0) 0)
    (else
      (+ (pascals-triangle (- row 1) col)
         (pascals-triangle (- row 1) (- col 1))))))

