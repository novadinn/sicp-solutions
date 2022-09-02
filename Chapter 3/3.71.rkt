#lang sicp

(define (cube x) (* x x x))

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

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                 (else
                  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define cube-weighted-pairs
    (weighted-pairs integers integers (lambda (x) (+ (cube (car x)) (cube (cadr x))))))
(define ramanujan-numbers-helper
  (lambda ()
    (define pred
      (lambda (p1 p2)
        (let ((val1 (+ (cube (car p1)) (cube (cadr p1))))
              (val2 (+ (cube (car p2)) (cube (cadr p2)))))
          (= val1 val2))))
    (define iter
      (lambda (pairs prev-pair)
        (cond
          ((pred (stream-car pairs) prev-pair)
           (cons-stream prev-pair
                        (iter (stream-cdr pairs) prev-pair)))
          (else (iter (stream-cdr pairs) (stream-car pairs))))))
    (stream-cdr (iter cube-weighted-pairs (stream-car cube-weighted-pairs)))))
(define pair-stream->cube-sum-stream
  (lambda (s)
    (cons-stream (+ (cube (caar s)) (cube (cadar s)))
                 (pair-stream->cube-sum-stream (stream-cdr s)))))
(define ramanujan-numbers
  (pair-stream->cube-sum-stream (ramanujan-numbers-helper)))

#|
The next five are 4104, 13832, 20683, 32832, 39312
|#