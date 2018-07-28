#lang racket

;;; Processing surefiles.

(require "escape.rkt"
	 "humanize.rkt"
	 "posix.rkt"
	 "attmap.rkt"
	 "meter.rkt")

;;; Perform a scan of a directory, with a progress meter.
(define (metered-scan path)
  (define dirs 0)
  (define files 0)
  (define octets 0)
  (define (show forced)
    (update-meter
      "scan: ~a dirs, ~a files, ~a bytes~%" dirs files
      (humanize-bytes octets)
      #:force forced))
  (define (update node)
    (cond [(dir-node? node)
	   (set! dirs (add1 dirs))]
	  [else
	    (set! files (add1 files))
	    (set! octets (+ octets
			    (or (node-size node) 0)))])
    (show #f))
  (define result (scan-tree #"__root__" path #:meter update))
  (show #t)
  (finalize-meter)
  result)

;;; Maybe should be in posix?
(define (node-size node)
  (hash-ref (node-atts node)
	    'size
	    (lambda () #f)))

(define (scan-and-save path [output (current-output-port)])
  (define tree (scan-tree #"__root__" path))
  (save-sure tree output))

;;; Scan a tree, and write the result out.
(define (save-sure tree [output (current-output-port)])
  (write-bytes #"asure-2.0\n-----\n" output)
  (walk-dir tree output))

(define (walk-dir tree output)
  (unless (dir-node? tree)
    (error "Path given was not a directory"))
  (fprintf output "d~a ~a~%"
	   (escape (node-name tree))
	   (attmap->bytes (node-atts tree)))
  (for ([dir (in-list (dir-node-dirs tree))])
    (walk-dir dir output))
  (fprintf output "-\n")
  (for ([file (in-list (dir-node-files tree))])
    (walk-file file output))
  (fprintf output "u\n"))

(define (walk-file file output)
  (fprintf output "f~a ~a~%"
	   (escape (node-name file))
	   (attmap->bytes (node-atts file))))