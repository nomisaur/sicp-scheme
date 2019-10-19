#lang sicp

(define (square n)
  (* n n))

(define (square-list1 items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list1 (cdr items)))))

(square-list1 (list 1 2 3 4))


(define (square-list2 items)
  (map (lambda (i) (square i))
       items))

(square-list2 (list 1 2 3 4))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square 
                     (car things))))))
  (iter items nil))

(square-list (list 1 2 3 4))