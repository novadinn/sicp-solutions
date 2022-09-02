#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define random-init
  (lambda ()
    (random 2147483647)))
(define rand-update
  (let ((a (random-init))
        (b (random-init))
        (m (random-init)))
    (lambda (x)
      (remainder (+ (* a x) b) m))))
(define random-numbers
  (lambda (S)
    (cons-stream (random-init)
                 (stream-map
                  (lambda (op opt)
                    (cond ((eq? 'generate (car op))
                           (rand-update opt))
                          ((eq? 'reset (car op))
                           (cadr op))
                          (else (error "Unknown message" op))))
                  S (random-numbers (stream-cdr S))))))