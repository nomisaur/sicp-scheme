#lang sicp

;    : = make
;    . = get

; let :interval = make interval
; let .lb = get lower bound
; let .ub = get upper bound

(define (:interval a b) (cons a b))

(define (.lb i) (car i))
(define (.ub i) (cdr i))

(define (+_i x y)
  (:interval (+ (.lb x) 
                (.lb y))
             (+ (.ub x) 
                (.ub y))))


(define (-_i x y)
  (:interval (- (.lb x)
                (.lb y))
             (- (.ub x)
                (.ub y))))


(define (*_i x y)
  (let ((p1 (* (.lb x) 
               (.lb y)))
        (p2 (* (.lb x) 
               (.ub y)))
        (p3 (* (.ub x) 
               (.lb y)))
        (p4 (* (.ub x) 
               (.ub y))))
    (:interval (min p1 p2 p3 p4)
               (max p1 p2 p3 p4))))

(define (/_i x y)
  (*_i x 
       (:interval
        (/ 1.0 (.ub y)) 
        (/ 1.0 (.lb y)))))
