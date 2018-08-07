#lang racket

(provide estimator%
	 update-hashes)

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

;;; Return the size of the node in bytes, zero if it doesn't have a
;;; size.
(define (node-size node)
  (define atts (node-atts node))
  (hash-ref atts 'size (lambda () 0)))

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

    (define/public (update #:force [frc #f])
      (update-meter "~a/~a (~a%) files, ~a/~a (~a%) bytes\n"
		    (~r cur-files #:min-width 7)
		    (~r total-files #:min-width 7)
		    (percentage cur-files total-files)
		    (humanize-bytes cur-bytes)
		    (humanize-bytes total-bytes)
		    (percentage cur-bytes total-bytes)))

    (define/public (add-file octets)
      (set! cur-files (add1 cur-files))
      (set! cur-bytes (+ cur-bytes octets))
      (update))
    ))

;;; Given a tree, walk it, updating all of the file nodes that need
;;; hashes with ones that have hashes.
(define (update-hashes path tree)
  (define est (new estimator% [tree tree]))

  (define (walk path tree)
    (define new-dirs
      (for/list ([subdir (in-list (dir-node-dirs tree))])
        (define sub-name (bytes-append path #"/" (node-name subdir)))
        (walk sub-name subdir)))

    (define new-files
      (for/list ([subfile (in-list (dir-node-files tree))])
	(if (need-hash? subfile)
	  (let ()
	    (define sub-name (bytes-append path #"/" (node-name subfile)))
	    (define sha1 (sha1-file sub-name))
	    (send est add-file (node-size subfile))
	    (define old-atts (node-atts subfile))
	    (define new-atts (hash-set old-atts 'sha1 sha1))
	    (struct-copy node subfile [atts new-atts]))
	  subfile)))
    (struct-copy dir-node tree
                 [dirs new-dirs]
                 [files new-files]))

  (define new-tree (walk path tree))
  (send est update #:force #t)
  (finalize-meter)
  new-tree)
