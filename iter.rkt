#lang sicp

(define (++ n) (inc n))
(define (-- n) (- n 1))



(define (iter infant food end? next feed)
  (define (continue food monkey)
    (if (end? food)
        monkey
        (continue (next food) (feed food monkey))))
  (continue food infant))

(define (foldl op initial sequence)
  (iter initial
        sequence
        null?
        cdr
        (lambda (rest result) (op result (car rest)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

;(foldl * 1 (list 1 2 3))
;(foldl cons nil (list 1 2 3))

(define (num-list start end)
  (iter nil
        end
        (lambda (i) (< i start))
        --
        cons))

(define (flatmap op lst)
  (accumulate append
              nil
              (map op lst)))


(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list j i))
                  (num-list 1 (dec i))))
           (num-list 1 n)))

;(num-list 8 28)

(define (filter predicate sequence) 
  (cond ((null? sequence) nil) 
        ((predicate (car sequence)) 
         (cons (car sequence)  
               (filter predicate (cdr sequence)))) 
        (else (filter predicate (cdr sequence)))))

(define (prime? x) 
  (define (test divisor) 
    (cond ((> (* divisor divisor) x) true) 
          ((= 0 (remainder x divisor)) false) 
          (else (test (+ divisor 1))))) 
  (test 2))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

(define (enumerate-interval low high) 
  (if (> low high) 
      nil 
      (cons low (enumerate-interval (+ low 1) high))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (unique-pairs n))))


(unique-pairs 3)

(prime-sum-pairs 6) 
