#lang sicp

#|
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define origin-frame
  (lambda (f)
    (car f)))
(define edge1-frame
  (lambda (f)
    (cadr f)))
(define edge2-frame
  (lambda (f)
    (caddr f)))
|#

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define origin-frame
  (lambda (f)
    (car f)))
(define edge1-frame
  (lambda (f)
    (cadr f)))
(define edge2-frame
  (lambda (f)
    (cddr f)))