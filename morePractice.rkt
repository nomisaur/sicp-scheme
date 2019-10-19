#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* higher-terms x)))
   0
   coefficient-sequence))

;(horner-eval 2 (list 1 2 3))


(define (count-leaves t)
  (accumulate
   (lambda (feed grow)
     (+ feed grow))
   0
   (map
    (lambda (item)
      (cond ((null? item) 0)
            ((pair? item) (count-leaves item))
            (else 1)))
    t)))

(count-leaves (list 1 2 (list 3 4) 5 (list 6 (list 7) 8)))
(count-leaves (list 1 2 nil))

