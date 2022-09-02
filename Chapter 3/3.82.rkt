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

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))
(define (random-in-range-stream low1 high1 low2 high2)
  (cons-stream (cons (random-in-range low1 high1) (random-in-range low2 high2))
               (random-in-range-stream low1 high1 low2 high2)))
(define estimateintegral
  (lambda (P x1 x2 y1 y2)
    (let ((area (* (+ (abs x1) (abs x2))
                   (+ (abs y1) (abs y2))
                   4))
          (random-numbers (random-in-range-stream x1 x2 y1 y2)))
      (scale-stream (monte-carlo (stream-map P random-numbers) 0 0) area))))