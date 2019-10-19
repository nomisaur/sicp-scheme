#lang sicp

(define (last lst)
  (define (iter prev next)
    (if (null? next)
        (car prev)
        (iter next (cdr next))))
  (if (null? lst)
      nil
      (iter lst (cdr lst))))

(last (list 23 72 149 34))

(define (reverse lst)
  (define (iter acc arr)
    (if (null? arr)
        acc
        (iter (cons (car arr) acc) (cdr arr))))
  (iter nil lst))

(reverse (list 1 4 9 16 25))
