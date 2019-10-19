#lang sicp

(define (avg a b) (/ (+ a b) 2))

(define (sqrt n)
  (define (improve guess)
    (avg guess (/ n guess)))
  (define (iter guess)
    (if (= guess (/ n guess))
        guess
        (iter (improve guess))))
  (iter 2.0))

(define (square n) (* n n))

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (midpoint-segment segment)
  (let ((p1 (start-segment segment))
        (p2 (end-segment segment)))
    (let ((x1 (x-point p1))
          (y1 (y-point p1))
          (x2 (x-point p2))
          (y2 (y-point p2)))
      (make-point
       (avg x1 x2)
       (avg y1 y2)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;(midpoint-segment
; (make-segment
;  (make-point 1 1)
;  (make-point 5 3)))
;(define (pack4 a b c d)
;  (cons (cons a b) (cons c d)))


(define (make-rect x< x> y^ y_)
  (define (package a b c d)
    (cons (cons a b) (cons c d)))
  (let ((a (make-point x< y^))
        (b (make-point x> y^))
        (c (make-point x> y_))
        (d (make-point x< y_)))
    (let ((a--b (make-segment a b))
          (b--c (make-segment b c))
          (c--d (make-segment c d))
          (d--a (make-segment d a)))
      (let ((points (package a b c d))
            (lines (package a--b b--c c--d d--a)))
        (cons points lines)))))

(define (points rect) (car rect))
(define (lines rect) (cdr rect))

(define (a rect)
  (car (car (points rect))))

(define (b rect)
  (cdr (car (points rect))))

(define (c rect)
  (car (cdr (points rect))))

(define (d rect)
  (cdr (cdr (points rect))))

(define (a--b rect)
  (car (car (lines rect))))

(define (b--c rect)
  (cdr (car (lines rect))))

(define (c--d rect)
  (car (cdr (lines rect))))

(define (d--a rect)
  (cdr (cdr (lines rect))))

(define (len seg)
  (let ((p1 (start-segment seg))
        (p2 (end-segment seg)))
    (let ((x1 (x-point p1))
          (y1 (y-point p1))
          (x2 (x-point p2))
          (y2 (y-point p2)))
      (sqrt (+ (square (- x1 x2))
               (square (- y1 y2)))))))


(define (pack-4 a b c d)
  (cons a (cons b (cons c d))))

(define (get n 4-pack)
  (cond ((= n 1) (car 4-pack))
        ((= n 2) (car (cdr 4-pack)))
        ((= n 3) (car (cdr (cdr 4-pack))))
        ((= n 4) (cdr (cdr (cdr 4-pack))))
        (else (error "nah dawg"))))

(define (make-rect-2 a b c d)
  (pack-4 a b c d))

(define (a_ rect)
  (get 1 rect))

(define (b_ rect)
  (get 2 rect))

(define (c_ rect)
  (get 3 rect))

(define (d_ rect)
  (get 4 rect))



(define (a__b rect)
  (make-segment (a_ rect) (b_ rect)))

(define (b__c rect)
  (make-segment (b_ rect) (c_ rect)))

(define (c__d rect)
  (make-segment (c_ rect) (d_ rect)))

(define (d__a rect)
  (make-segment (d_ rect) (a_ rect)))

(define (perimeter quad)
  (+ (len (a--b quad))
     (len (b--c quad))
     (len (c--d quad))
     (len (d--a quad))))

(define (area rect)
  (* (len (a--b rect))
     (len (b--c rect))))

(define (perimeter2 quad)
  (+ (len (a__b quad))
     (len (b__c quad))
     (len (c__d quad))
     (len (d__a quad))))

(define (area2 rect)
  (* (len (a__b rect))
     (len (b__c rect))))

;(let ((bob (make-rect 0 2 3 0)))
;  (area bob))


(let ((joe (make-rect-2
            (make-point 0 2)
            (make-point 3 2)
            (make-point 3 0)
            (make-point 0 0))))
  (perimeter2 joe))