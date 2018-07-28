#lang racket

(provide estimator%)

(require "posix.rkt"
	 "humanize.rkt"
	 "meter.rkt")

;;; Walk a node tree, accumulating file and size counts in the tree.
(define (build-estimate tree)
  (define files 0)
  (define octets 0)

  (define (walk-dir node)
    (for ([subdir (in-list (dir-node-dirs node))])
      (walk-dir subdir))
    (for ([file (in-list (dir-node-files node))])
      (walk-file file)))
  (define (walk-file node)
    (define size (need-hash? node))
    (when size
      (set! files (add1 files))
      (set! octets (+ octets size))))

  (walk-dir tree)
  (values files octets))

;;; Determine if a node needs an estimate.  This happens for regular
;;; file nodes that don't have a 'sha1' property.  Returns an size in
;;; bytes if it does, or #f if it does not.
(define (need-hash? node)
  (define atts (node-atts node))
  (and (eq? (hash-ref atts 'kind) 'reg)
       (not (hash-has-key? atts 'sha1))
       (hash-ref atts 'size)))

(define (percentage cur total)
  (~r (* (/ cur total) 100)
      #:min-width 5
      #:precision '(= 1)))

;;; An estimator can build an estimate of hash-update work that needs
;;; to be done, and can also be used as a progress meter of that work.
(define estimator%
  (class object%
    ;;; Give the initial node tree for the estimate.
    (super-new)
    (init tree)

    (define-values (total-files total-bytes) (build-estimate tree))
    (define cur-files 0)
    (define cur-bytes 0)

    (define/public (update)
      (update-meter "~a/~a (~a%) files, ~a/~a (~a%) bytes\n"
		    (~r cur-files #:min-width 7)
		    (~r total-files #:min-width 7)
		    (percentage cur-files total-files)
		    (humanize-bytes cur-bytes)
		    (humanize-bytes total-bytes)
		    (percentage cur-bytes total-bytes)))
    ))
