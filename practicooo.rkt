#lang sicp

(define (last lst)
  (define (iter prev next)
    (if (null? next)
        (car prev)
        (iter next (cdr next))))
  (if (null? lst)
      nil
      (iter lst (cdr lst))))

(define (reverse-map proc lst)
  (define (iter acc arr)
    (if (null? arr)
        acc
        (iter (cons (car arr) acc) (cdr arr))))
  (iter nil lst))

(define (reverse lst)
  (define (iter acc arr)
    (if (null? arr)
        acc
        (iter (cons (car arr) acc) (cdr arr))))
  (iter nil lst))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y)) 
              nil sequence))

(define (append seq1 seq2)
  (accumulate cons (reverse seq1) seq2))

(define (length sequence)
  (accumulate (lambda (feed grow)
                (+ grow 1))

              0 sequence))


(define good (list 1 2 3 4 5 6))

(define meh (list 7 8 9 0))

(map (lambda (element) (* 2 element)) good)

(append good meh)

(length meh)