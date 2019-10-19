#lang racket

(define (divides? a b)
  (= 0 (remainder a b)))

(define (++ n) (+ n 1))
(define (-- n) (- n 1))

(define (even? n)
  (divides? n 2))

(define (half n) (/ n 2))

(define (square n)
  (* n n))

(define (cube n)
  (* n n n))

(define (^ base expt)
  (cond ((= expt 0) ;base case ;)
         1)
        ((even? expt) ; 2^16 == 4^8 == 16^4 == 256^2 == 65536
         (^ (square base) (half expt)))
        (else
         (* base (^ base (-- expt))))))
