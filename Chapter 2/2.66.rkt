#lang sicp

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define key
  (lambda (x) x))
(define lookup
  (lambda (given-key set-of-records)
    (cond
      ((null? set-of-records) false)
      (else
       (let ((current (key (entry set-of-records))))
         (cond
           ((equal? current given-key) #t)
           ((> given-key current)
            (lookup given-key (right-branch set-of-records)))
           (else (lookup given-key (left-branch set-of-records)))))))))