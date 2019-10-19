#lang sicp

(define void (cond (#f #f)))

(define (display-matrix matrix)
  (map (lambda (row)
         (cond ((not (null? row))
                (display row)
                (newline))))
       matrix)
  void)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))



(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op
                        init
                        (map car seqs))
            (accumulate-n op
                          init
                          (map cdr seqs)))))

(define v123 (list 1 2 3))
(define v456 (list 4 5 6))
(define v789 (list 7 8 9))

(define numpad (list v123 v456 v789))

(define (sum lst)
  (accumulate + 0 lst))

(define (dot-product v1 v2)
  (sum (map * v1 v2)))

(define (m*v m v)
  (map (lambda (row)
         (dot-product row v))
       m))

(define (transpose m)
  (accumulate-n cons nil m))


(define (m*m ops ins)
  (map (lambda (oprow)
         (map (lambda (inrow)
                (dot-product oprow inrow))
              (transpose ins)))
       ops))


;(dot-product v123 v123)

;(dot-product v456 v123)
;(dot-product v789 v123)

;(m*v numpad v123)

(display-matrix (m*m numpad numpad))

;(newline)
;(display-matrix numpad)
;(newline)
;(display-matrix (transpose numpad))
