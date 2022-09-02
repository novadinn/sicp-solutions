#lang sicp
(#%require sicp-pict)

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
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


(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-up
           (transform-painter painter1
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0)))
          (paint-down
           (transform-painter painter2
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point)))
      (lambda (frame)
        (paint-up frame)
        (paint-down frame)))))

(define below-alt
  (lambda (painter1 painter2)
    (rotate90 (beside (rotate270 painter1) (rotate270 painter2)))))