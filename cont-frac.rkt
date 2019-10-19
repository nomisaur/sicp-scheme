#lang sicp

(define (-- n) (- n 1))
(define ++ inc)
(define (square n) (* n n))

(define (cont-frac n d k)
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (-- i) (/ (n i) (+ (d i) acc)))))
  (iter k 0))

(define (golden-ratio k)
  (let ((one (lambda (i) 1.0)))
    (cont-frac one one k)))

(define g golden-ratio)

(define (e k)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  (lambda (i)
                    (cond ((or (= i 1) (= i 2))
                           i)
                          ((= 2 (remainder i 3))
                           (* 2 (/ (++ i) 3)))
                          (else
                           1)))
                  k)))

(define (tan-cf x k)
  (let ((-x^2 (- (square x))))
    (cont-frac (lambda (i)
                 (if (= i 1)
                     x
                     -x^2))
               (lambda (i)
                 (-- (* i 2)))
               k)))

(define (t k) (tan-cf 8.0 k))

((lambda (n f)
   (define (iter i)
     (display (f i))
     (newline)
     (if (< i n) (iter (++ i))))
   (iter 1))
 25 t)

