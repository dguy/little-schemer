#lang racket

(require "plane.rkt")

(provide rectangle perimeter width height area)

(define (rectangle bottom-left width height)
 (cons bottom-left (cons width height)))

(define (width rectangle) 
  (car (cdr rectangle)))

(define (height rectangle) 
  (cdr (cdr rectangle)))

(define bottom-left car)

(define (perimeter rectangle)
  (* 2 (+ (width rectangle) (height rectangle))))

(define (area rectangle)
  (* (width rectangle) (height rectangle)))

(define (print-rect rectangle)
  (newline)
  (print-point (bottom-left rectangle))
  (display "width: ")
  (displayln (width rectangle))
  (display "height: ")
  (displayln (height rectangle))
  (display "perimeter: ")
  (displayln (perimeter rectangle))
  (display "area: ")
  (displayln (area rectangle)))

 (print-rect (rectangle (make-point 0 0) 10 5))
 (print-rect (rectangle (make-point 0 0) 5 5))
 (print-rect (rectangle (make-point 0 0) 6 8))

 
