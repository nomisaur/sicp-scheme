#lang sicp


(define (goldenize x)
  (inc (/ 1 x)))

(define (diff a b)
  (cond ((> a b) (- a b))
        ((> b a) (- b a))
        (else 0)))

(define (print a)
  (display a)
  (newline))

(define (average a b)
  (/ (+ a b) 2))

(define (fixed-point function initial tolerance)
  (define (iter prev curr)
    (print curr)
    (if (> tolerance (diff prev curr))
        curr
        (iter curr (function curr))))
  (iter initial (function initial)))


(fixed-point
 (lambda (n) (average n (/ (log 1000) (log n))))
 10
 0.001)