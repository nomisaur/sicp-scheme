#lang sicp

(define (expt_ base pow)
  (cond ((= pow 0)
         1)
        ((= pow 1)
         base)
        ((even? pow)
         (expt_ (* base base) (/ pow 2)))
        (else
         (* base (expt_ base (- pow 1))))))

(define (expt base pow)
  (define (iter b p extra)
    (cond ((= p 1)
           (* b extra))
          ((even? p)
           (iter (* b b) (/ p 2) extra))
          (else
           (iter b (- p 1) (* extra base)))))
  (if (= pow 0)
      1
      (iter base pow 1)))