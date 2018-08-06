#lang racket

(provide call)

;;; A common idiom in racket is something like
;;;
;;; (call-with-output-file pathname
;;;   (lambda (out)
;;;     ...))
;;;
;;; Where a procedure takes, as its last argument a proc of some number of arguments.
;;;
;;; This macro simplifies this usage just a bit, shortening the code both vertically, and reducing a
;;; little bit of rightward drift.
;;;
;;; It also has the advantage of only needing to teach the editor a single keyword 'call' to indent
;;; like a lambda.
;;;
;;; The above would be written as
;;;
;;; (call (call-with-output-file pathname) (out)
;;;   ...)
;;;
;;; Note that the lambda moves outside of the parenthesized call, and its arguments are given in
;;; parenthesis.

(define-syntax call
  (syntax-rules ()
    [(_ (func args ...) (params ...) body ...)
     (func args ... (lambda (params ...) body ...))]))
