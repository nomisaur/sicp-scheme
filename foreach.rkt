#lang sicp

;(define (for-each proc list)
;  (cond ((not (null? list))
;         (proc (car list))
;         (for-each proc (cdr list)))))

(define (for-each proc lead-list . lists)
  (cond ((not (null? list))
         (proc (car list))
         (for-each proc (cdr list)))))

(for-each 
 (lambda (x) (newline) (display x))
 (list 57 321 88))