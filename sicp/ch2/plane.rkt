#lang racket

(require "../ch1/math.rkt")

(provide make-point x-point y-point make-segment start-segment end-segment midpoint-segment print-point)

(define (make-point x y)
  (cons x y))

(define x-point car)
(define y-point cdr)

(define (make-segment start end)
  (cons start end))

(define start-segment car)
(define end-segment cdr)

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
        (make-point (average (x-point start)
                             (x-point end))
                    (average (y-point start)
                             (y-point end)))))


(define (print-point point)
  (newline)
  (display "(")
  (display (x-point point))
  (display ",")
  (display (y-point point))
  (displayln ")"))
   

