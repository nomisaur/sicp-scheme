#lang sicp

(define (even? n)
  (= 0 (remainder n 2)))

(define (exor a b)
  (if a (not b) b))

(define (exnor a b)
  (not (exor a b)))

(define (bool== a b) (exnor a b))

(define (reverse lst)
  (define (iter acc arr)
    (if (null? arr)
        acc
        (iter (cons (car arr) acc) (cdr arr))))
  (iter nil lst))

(define (same-parity li-1 . li-n)
  (let ((res (even? li-1)))
    (define (matches? n)
      (bool== res (even? n)))
    (define (iter acc lst)
      (if (null? lst)
          acc
          (let ((head (car lst))
                (tail (cdr lst)))
            (if (matches? head)
                (iter (cons head acc) tail)
                (iter acc tail)))))
    (reverse (iter (list li-1) li-n))))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)