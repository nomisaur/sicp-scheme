#lang sicp


(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define (zero f)
  (lambda (x) x))

(define (one f)
  (lambda (x) (f x)))

(define (two f)
  (lambda (x) (f (f x))))


(define (add a b)
  (lambda (f)
    (lambda (x)
      ((b f) ((a f) x)))))

(define (display n)
  ((n inc) 0))

(display (add two one))