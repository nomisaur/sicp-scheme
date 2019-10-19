#lang sicp

(define (iter infant food end? next feed)
  (define (continue food monkey)
    (if (end? food)
        monkey
        (continue (next food) (feed food monkey))))
  (continue food infant))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (num-list start end)
  (iter nil
        end
        (lambda (i) (< i start))
        dec
        cons))

(define (flatmap op lst)
  (accumulate append
              nil
              (map op lst)))

(define (filter include? lst)
  (cond ((null? lst)
         nil)
        ((include? (car lst))
         (cons (car lst)
               (filter include? (cdr lst))))
        (else (filter include? (cdr lst)))))


(define (sum lst)
  (accumulate + 0 lst))

(define (triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list k j i))
                             (num-list 1 (dec j))))
                      (num-list 1 (dec i))))
           (num-list 1 n)))

;(triples 5)

(define (why n s)
  (filter (lambda (triple) (= s (sum triple)))
          (triples n)))


;(why 10 16)

(define (remove item lst)
  (filter (lambda (i) (not (= item i)))
          lst))


;(define (unique-combos lst n)
;  (if (or (= n 0) (null? lst))
;      lst
;      (map (lambda (i)
;             (list (car lst)
;                   i))
;           (unique-combos (cdr lst) (dec n)))))

(define (unique-combos lst n)
  (if (null? lst)
      nil
      (map (lambda (x)
             (map (lambda (y)
                    (append (list x) y))
                  (unique-combos (remove x lst) (dec n))))
           lst)))
       

(unique-combos (list 1 2 3 4) 2)

;for each num i want to append it to each result of -1

