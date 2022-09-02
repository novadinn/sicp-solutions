#lang sicp

(define rember
  (lambda (item lst)
    (cond
      ((null? lst) '())
      ((equal? (car lst) item)
       (cdr lst))
      (else (cons (car lst)
                  (rember item (cdr lst)))))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define union-set
  (lambda (set1 set2)
    (cond
      ((or (null? set1) (null? set2)) set2)
      ((element-of-set? (car set1) set2)
       (cons (car set1)
             (union-set (cdr set1) (rember (car set1) set2))))
      (else (cons (car set1)
                  (union-set (cdr set1) set2))))))