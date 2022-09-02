#lang sicp
(#%require sicp-pict)

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0) 
                     (make-vect 0.0 0.0) 
                     (make-vect 1.0 1.0)))
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define rotate180
  (lambda (painter)
    (rotate90 (rotate90 painter))))
(define rotate270
  (lambda (painter)
    (rotate90 (rotate90 (rotate90 painter)))))