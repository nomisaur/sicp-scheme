#lang sicp

(define (square n) (* n n))

(define (double f)
  (lambda (x)
    (f (f x))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (iter i acc)
    (if (< i 2)
        acc
        (iter (- i 1) (compose f acc))))
  (iter n f))

(define dx 0.001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (smooth-n n)
  (lambda (f)
    ((repeated smooth n) f)))

(((smooth-n 10) square) 2)