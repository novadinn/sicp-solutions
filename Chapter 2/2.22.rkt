#lang sicp

(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

#|
The above doesn't work because it conses the last item from the front of the list to the answer.
|#

(define (square-list-alt items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

#|
This version conses cars to the squared element, but not a list, so we'll end up with dotted list.
|#