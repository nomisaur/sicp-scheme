#lang sicp

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define tolerance 0.000000001)

(define (-- n) (- n 1))

(define (** n e)
  (define (iter i acc)
    (if (= 1 i)
        acc
        (iter (-- i) (* n acc))))
  (if (= e 0)
      1
      (iter e n)))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00000001)

(define (derive g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube n) (* n n n))
(define (square n) (* n n))

(define (newtons-method g guess)
  (define (newton-transform g)
    (lambda (x)
      (- x (/ (g x) 
              ((derive g) x)))))
  (fixed-point (newton-transform g) 
               guess))

(define (sqrt x)
  (newtons-method 
   (lambda (y) 
     (- (square y) x)) 
   1.0))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (iter i acc)
    (if (< i 2)
        acc
        (iter (- i 1) (compose f acc))))
  (iter n f))



(define (cr x)
  (fixed-point
   ((repeated average-damp 1)
    (lambda (y) (/ x (square y))))
   1.0))

(define (nth-root n r)
  (fixed-point
   ((repeated average-damp (floor (log r 2)))
    (lambda (x)
      (/ n (** x (- r 1)))))
   1.0))

(define (iterative-improve test improve)
  (define (iter x)
    (if (test x)
        x
        (iter (improve x))))
  iter)

(define (fp f first-guess)
  (define (change? guess)
    (= guess (f guess)))
  ((iterative-improve change? f) first-guess))

(define (sr x)
  (fp
   ((repeated average-damp 1)
    (lambda (y) (/ x y)))
   1.0))


(define (sqrt2 n)
  (define (improve guess)
    (average guess (/ n guess)))
  (define (change? guess)
    (= guess (improve guess)))
  ((iterative-improve change? improve) n))

(sr 16.0)