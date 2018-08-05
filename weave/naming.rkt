#lang racket

(require file/gzip
         file/gunzip)

(provide (struct-out naming)
         main-file-name
         call-with-main-in
         call-with-temp-rename-to
         call-with-temp-file)

;;; A 'naming' represnts the files associated with a given storage file.
(struct naming (directory base extension compress?))

(define (make-gzname nm name want-compress?)
  (if (and want-compress? (naming-compress? nm))
    (string-append name ".gz")
    name))

(define (make-filename nm
                       #:extension [extension (naming-extension nm)]
                       #:compress? [compress? #t])
  (define filename (string-append (naming-base nm)
                                  "."
                                  extension))
  (build-path (naming-directory nm)
              (make-gzname nm filename compress?)))

(define (main-file-name nm) (make-filename nm))
(define (backup-file-name nm) (make-filename nm #:extension "bak"))

(define (compressed-path? pth)
  (regexp-match #rx"\\.gz$" (path->string pth)))

;;; Try to open a "random" file, until it exists.  Returns (values name port) where name is the name
;;; of the temp file, and port is the output port opened to write to it.
(define (open-random-file nm #:compress? [compress? #t])
  (let loop ([limit 30])
    (when (zero? limit)
      (error "Unable to open temporary file"))
    (define num (number->string (random 1000000)))
    (define name (make-filename nm #:extension num #:compress? compress?))
    (define port
      (with-handlers ([exn:fail:filesystem:exists? (lambda (_) #f)])
        (open-output-file name)))
    (if port
      (values name port)
      (loop (sub1 limit)))))

;;; Uses the 'disposabale' package for cleanup.

;;; Open a new temp file, related to the given naming.  If compression is requested, _and_ the
;;; naming convention has compression, it will have a '.gz' suffix and the data compressed.
;;;
;;; There are two use cases covered here:
;;;
;;; - Writing to a temporary file that, on success, will be renamed to another file (possibly after
;;;   renaming that to a backup file.
;;;
;;; - Writing to a temporary file that is then closed, and then read using other tools.  This gives
;;;   an intermediate phase that doesn't match a single disposable.

;;; Open a temporary file, calling 'proc' with the name and  output port as its arguments.  When
;;; proc exits successfully, the file will be renamed to the main file.  If proc exists by another
;;; means (such as raising an exception), the temporary file will be closed and removed instead.
;;; Compression will be determined by whether the naming convention indicates compression should be
;;; used.
(define (call-with-temp-rename-to naming proc)
  (define-values (name port) (open-random-file naming))
  (define main-name (main-file-name naming))
  (define success #f)
  (dynamic-wind
    void
    (thunk
      (define result (call-with-maybe-gz main-name proc name port))
      (close-output-port port)
      (with-handlers ([exn:fail:filesystem? void])
        (rename-file-or-directory main-name (backup-file-name naming) #t))
      (rename-file-or-directory name main-name)
      (set! success #t)
      result)
    (thunk
      (unless success
        (close-output-port port)
        (delete-file name)))))

;;; Call (proc name port), although if 'main-name' ends in '.gz' compress the output.
(define (call-with-maybe-gz main-name proc name port)
  (if (compressed-path? main-name)
    (let ()
      (define done (make-channel))
      (define-values (zin zout) (make-pipe))
      (define-values (_path main-simple-name _) (split-path main-name))
      (thread (lambda ()
                (gzip-through-ports zin port (no-gz main-simple-name) (current-seconds))
                (channel-put done #f)))
      (define result (call-with-continuation-barrier (thunk (proc name zout))))
      (close-output-port zout)
      (channel-get done)
      result)
    (call-with-continuation-barrier (thunk (proc name port)))))

;;; Call (proc input-port) with the possibly uncompressed port input.
(define (call-with-main-in nm proc)
  (call-with-input-file (main-file-name nm)
    (lambda (in)
      (if (naming-compress? nm)
        (let ()
          (define-values (zin zout) (make-pipe))
          (thread (lambda ()
                    (gunzip-through-ports in zout)
                    (close-output-port zout)))
          (proc zin))
        (proc in)))))

(define (no-gz name)
  (regexp-replace #rx"\\.gz$"
                  (path->string name)
                  ""))

;;; Open a temporary file, calling 'proc' with the name and output port as the arguments.  When proc
;;; exits successfully, the file will be removed.  It is common to close the output port, and use
;;; the file as input for something, such as an external process.  Note that the default for
;;; compression here is false, since these intermediate files are usually not compressed.
(define (call-with-temp-file naming proc #:compress? [compress #f])
  (define-values (name port) (open-random-file naming #:compress? compress))
  (dynamic-wind
    void
    (thunk
      (call-with-maybe-gz name proc name port))
    (thunk
      (close-output-port port)
      (delete-file name))))

;;; For testing.
; (define *name* (naming "." "sample" "dat" #t))
