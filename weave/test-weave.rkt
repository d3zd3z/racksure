#lang racket

(require "call.rkt"
         "weave.rkt"
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
  (define deltas null)
  (define delta1 (range 100))
  (set! deltas (cons delta1 deltas))
  (call (call-with-first-delta nn "first-delta" (hasheq 'type "testing")) (out)
    (write-items delta1 out))
  (define total 300)
  (printf "Writing ~a deltas~%" total)
  (for ([i (in-range 2 (add1 total))])
    (define delta2 (mutate delta1))
    (define name (format "delta #~a" i))
    (call (call-with-update-delta nn name (hasheq 'type "testing2")) (out)
      (write-items delta2 out))
    (set! deltas (cons delta2 deltas)))

  (set! deltas (reverse deltas))

  (printf "Validating ~a deltas~%" (length deltas))
  (for ([delta (in-list deltas)]
        [num (in-naturals)])
    (validate nn (car deltas) (add1 num)))
  (printf "Passed~%"))

(define (validate nn numbers delta)
  (define (proc line)
    (define num (string->number line))
    (unless num (car numbers)
      (error "Delta mismatch on delta" delta))
    (set! numbers (cdr numbers)))

  (read-delta nn delta proc)
  (unless (null? numbers)
    (error "Delta mismatch on delta" delta)))

(module+ main
  (test-it))
