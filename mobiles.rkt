#lang sicp

(define (mobile left right)
  (list left right))

(define (branch length structure)
  (list length structure))

(define (left mobile)
  (car mobile))

(define (right mobile)
  (car (cdr mobile)))

(define (length branch)
  (car branch))

(define (structure branch)
  (car (cdr branch)))

(define (total-weight m)
  (define (get-weight x)
    (if (pair? x)
        (total-weight x)
        x))
  (+ (get-weight (structure (left m)))
     (get-weight (structure (right m)))))

(define (torque len stru)
  (* len
     (if (pair? stru)
         (total-weight stru)
         stru)))

(define (balanced? m)
  (if (pair? m)
      (let ((lb (left m))
            (rb (right m)))
        (let ((ls (structure lb))
              (rs (structure rb)))
          (and (= (torque (length lb) ls)
                  (torque (length rb) rs))
               (balanced? ls)
               (balanced? rs))))
      #t))

(define a (mobile (branch 2 3) (branch 2 3))) 
;(total-weight a)
(define d (mobile (branch 10 a) (branch 12 5)))
(balanced? d)
(balanced? a)