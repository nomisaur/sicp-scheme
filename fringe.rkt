#lang sicp

(define (fringe tree)
  (define (iter grow feed)
    (if (null? feed)
        grow
        (let ((head (car feed))
              (tail (cdr feed)))
          (if (pair? head)
              (iter (append grow (fringe head))
                    tail)
              (iter (append grow (list head))
                    tail)))))
  (iter nil tree))

(define x
  (list (list 1 2) (list 3 4)))

x
 
(fringe x)

(fringe (list x x))