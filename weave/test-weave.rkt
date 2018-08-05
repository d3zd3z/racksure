#lang racket

(require "weave.rkt"
         "naming.rkt")

;;; Perform a somewhat random modification of the data.  Choose some range of the items and reverse
;;; them.
(define (mutate lst)
  (define len (length lst))
  (define-values (a1 b1) (values (random (add1 len)) (random (add1 len))))
  (define-values (a b)
    (if (<= a1 b1)
      (values a1 b1)
      (values b1 a1)))
  (define-values (head rst) (split-at lst a))
  (define-values (mid tail) (split-at rst (- b a)))
  (define new-list (append head (reverse mid) tail))
  (if (equal? lst new-list)
    (mutate lst)
    new-list))

;;; Write the list of items to the given channel.
(define (write-items lst port)
  (for ([item (in-list lst)])
    (displayln item port)))

(define (test-it)
  (define nn (naming "." "sample" "dat" #t))
  (define delta1 (range 100))
  (call-with-first-delta
    nn "first-delta" (hasheq 'type "testing")
    (lambda (out)
      (write-items delta1 out)))
  (define delta2 (mutate delta1))
  (call-with-update-delta
    nn "second-delta" (hasheq 'type "testing2")
    (lambda (out)
      (write-items delta2 out))))

(module+ main
  (test-it))
