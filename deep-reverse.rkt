#lang sicp

(define (reverse lst)
  (define (iter acc arr)
    (if (null? arr)
        acc
        (iter (cons (car arr) acc) (cdr arr))))
  (iter nil lst))

(define (deep-reverse lst)
  (define (iter acc arr)
    (if (null? arr)
        acc
        (let ((head (car arr))
              (tail (cdr arr)))
          (if (pair? head)
              (iter
               (cons (deep-reverse head) acc)
               tail)
              (iter
               (cons head acc)
               tail)))))
  (iter nil lst))

(define x 
  (list (list 1 2) (list 3 4)))

x

(reverse x)

(deep-reverse x)