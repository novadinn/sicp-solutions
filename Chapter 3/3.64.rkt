#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define square-limit
  (lambda (S tolerance)
    (cond
      ((or (stream-null? S) (stream-null? (stream-cdr S))) the-empty-stream)
      ((< (abs (- (stream-car S) (stream-car (stream-cdr S)))) tolerance)
       (stream-car (stream-cdr S)))
      (else (cons-stream
             (stream-car S)
             (square-limit (stream-cdr S) tolerance))))))