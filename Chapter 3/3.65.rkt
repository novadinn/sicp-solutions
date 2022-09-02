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

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define partial-sums
  (lambda (S)
    (cons-stream (stream-car S)
                 (add-streams (stream-cdr S)
                              (partial-sums S)))))

(define (ln2-iter n)
        (cons-stream (/ 1.0 n)
                     (stream-map - (ln2-iter (+ n 1)))))
(define ln2-stream
        (partial-sums (ln2-iter 1)))