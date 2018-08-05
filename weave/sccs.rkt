#lang racket

;;; The weave algorithm used comres from the SCCS program.  This can be installed on some Linux
;;; distros by installing the package "cssc".
;;;
;;; If this available, this module can use the sccs tool to generate test deltas, which can be
;;; useful for testing the weave parsing (since the weave parser is needed to add deltas).

(define sccs-exe (find-executable-path "sccs"))

(define (have-sccs?)
  (define out (open-output-bytes))
  (and sccs-exe
       (parameterize ([current-error-port out])
         (system* sccs-exe "-V"))))

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
  (append head (reverse mid) tail))

;;; SCCS is particularly unhappy using files that aren't in the current directory.
;;; It works best to always use a path in the current directory, and change directories if
;;; necessary.
(define (to-sccs-path pth)
  (match (explode-path pth)
    [(list name)
     (string-append "s." (path->string name))]
    [else
      (error "Invalid path for sccs" pth)]))

;;; Write a list of things to a file.  This specifically allows overwrite, since sccs kind of works
;;; that way.
(define (write-list path lst)
  (call-with-output-file path
    (lambda (out)
      (for ([line (in-list lst)])
        (displayln line out)))
    #:exists 'truncate))

;;; Create the initial sccs file.  path must be a simple path, with no directory.
(define (write-initial-sccs path lst)
  (define sccs-path (to-sccs-path path))
  (write-list path lst)
  (run sccs-exe "admin" (string-append "-i" path) "-n" sccs-path)
  (delete-file path))

(define (write-update-sccs path lst)
  (define sccs-path (to-sccs-path path))
  (run sccs-exe "get" "-e" sccs-path)
  (write-list path lst)
  (run sccs-exe "delta" "-yMessage" sccs-path))

;;; Run command, checking result, and capturing stdout/stderr, and only showing it if there is an
;;; error.
(define (run . args)
  (define eout (open-output-string "stderr"))
  (unless (parameterize ([current-error-port eout]
                         [current-output-port eout])
            (apply system* args))
    (error "Unable to run command" args (get-output-string eout))))

(define (generate-tfile path lines deltas)
  (define numbers (range 1 (add1 lines)))
  (write-initial-sccs path numbers)
  (for ([i (in-range 1 deltas)])
    (set! numbers (mutate numbers))
    (write-update-sccs path numbers)))

(module+ main
  (generate-tfile "tfile" 1000 300))
