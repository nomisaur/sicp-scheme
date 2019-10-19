#lang sicp

(define (-- n) (- n 1))

(define (pascal row pos)
  (if (or (= pos row) (= pos 1))
      1
      (let ((r-1 (-- row)))
        (+ (pascal r-1 pos)
           (pascal r-1 (-- pos))))))