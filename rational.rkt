#lang sicp

(define pair cons)
(define first car)
(define second cdr)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (rational n1 d1)
  (let ((g (gcd n1 d1)))
    (let ((n2 (/ n1 g))
          (d2 (/ d1 g)))
      (if (> d2 0)
          (cons n2 d2)        
          (cons (- n2) (- d2))))))

(rational -8 -12)