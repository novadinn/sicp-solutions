#lang sicp

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define triple-sum
  (lambda (n)
    (filter (lambda (x) (= (accumulate + 0 x) n))
            (flatmap
             (lambda (i)
               (flatmap
                (lambda (j)
                  (map
                   (lambda (k)
                     (list i j k))
                   (enumerate-interval 0 n)))
                (enumerate-interval 0 n)))
             (enumerate-interval 0 n)))))