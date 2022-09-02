#lang sicp

(define square
  (lambda (x)
    (* x x)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define estimateintegral
  (lambda (P x1 x2 y1 y2 trials)
    (let ((area (* (+ (abs x1) (abs x2))
                   (+ (abs y1) (abs y2))
                   4)))
      (* area (monte-carlo trials (lambda ()
                                    (P x1 x2 y1 y2)))))))
(define P
  (lambda (x1 x2 y1 y2)
    (let ((random-x (random-in-range x1 x2))
          (random-y (random-in-range y1 y2)))
      (>= (+ (square (- random-x 0)) (square (- random-y 0)))
          1))))
(estimateintegral P -1.0 1.0 -1.0 1 100)