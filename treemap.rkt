#lang sicp

(define (square n)
  (* n n))

(define (tree-map fn tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map fn sub-tree)
             (fn sub-tree)))
       tree))

(define (square-tree tree) 
  (tree-map square tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))