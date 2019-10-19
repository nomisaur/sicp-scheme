#lang sicp

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car n)
  (define (iter i x)
    (if (= 0 (remainder x 2))
        (iter (inc i) (/ x 2))
        i))
  (iter 0 n))

(define (cdr n)
  (define (iter i x)
    (if (= 0 (remainder x 3))
        (iter (inc i) (/ x 3))
        i))
  (iter 0 n))

(cons 3 8)

(cdr 52488)