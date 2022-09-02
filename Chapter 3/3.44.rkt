#lang sicp

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

#|
Hugo is wrong. The difference between the transfer procedure and the exchange procedure is that in the
transfer procedure we do not need to access the balance value.
|#