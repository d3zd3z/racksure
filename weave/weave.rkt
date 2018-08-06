#lang racket

(require "call.rkt"
         "naming.rkt"
         "parse.rkt"
         json
         gregor)

(provide (struct-out delta)
         call-with-first-delta
         call-with-update-delta
         read-delta)

;;; tags should be a hasheq from symbols to strings.  Time is a "moment".
(struct delta (number name tags time) #:transparent)

;;; Construct a new delta, filling in the time field.
(define (make-delta number name tags)
  (delta number name tags (now/moment/utc)))

;;; Convert a list of deltas into a json representation.
(define (encode-deltas dlts)
  (define (encode-delta dlt)
    (hasheq 'number (delta-number dlt)
            'name (delta-name dlt)
            'tags (delta-tags dlt)
            'time (moment->iso8601 (delta-time dlt))))
  (define js
    (hasheq 'version 1
            'deltas (map encode-delta dlts)))
  (jsexpr->string js))

(define (decode-deltas text)
  (define top (string->jsexpr text))
  (unless (eqv? (hash-ref top 'version) 1)
    (error "Invalid delta descriptor version"))
  (define (decode-delta js)
    (delta (hash-ref js 'number)
           (hash-ref js 'name)
           (hash-ref js 'tags)
           (iso8601->moment (hash-ref js 'time))))
  (map decode-delta (hash-ref top 'deltas)))

;;; Return the highest delta in a delta list.
(define (highest-delta dlts)
  (foldl max 0 (map delta-number dlts)))

;;; Given a list of existing deltas, return a new list adding a particular new delta.
(define (add-delta dlts name tags)
  (append dlts (list (make-delta (add1 (highest-delta dlts)) name tags))))

;;; Read the header of the weave file, which must be the first line.
(define (read-weave-header input)
  (define line (read-line input))
  (unless (regexp-match #rx"^\1t" line)
    (error "Weave file does not start with proper header"))
  (decode-deltas (substring line 2)))

;;; A line-based writer of the initial delta.  Calls proc with a single argument which is a function
;;; that can be called for each string to be added to the weave.
;;; Write the first delta.  Given a naming convention, calls 'proc' with a port it can write to to
;;; store the first delta.
(define (call-with-first-delta nm name tags proc)
  (call (call-with-temp-rename-to nm) (_ out)
    (define header (add-delta null name tags))
    (fprintf out "\1t~a~%" (encode-deltas header))
    (displayln "\1I 1" out)
    (proc out)
    (displayln "\1E 1" out)))

;;; Update a given delta.  Will call 'proc' with output sent to a temporary file where it can write
;;; the new version.
(define (call-with-update-delta nm name tags proc)
  (define header (call-with-main-in nm read-weave-header))

  (define last-delta (highest-delta header))

  (call (call-with-temp-file nm) (diff-name diff-out)
    (call (call-with-temp-file nm) (tname out)
      (call (call-with-main-in nm) (in)
        ((make-weave-parser in (new weave-write-sink% [out-port out]) last-delta) 0)
        (close-output-port out))
      ;; (system* "/bin/ls" "-l")
      ;; (system* "/bin/cp" tname "hahafofo")

      ;; Run the user operation to a new temporary file.
      (call (call-with-temp-file nm) (new-temp-name new-out)
        (proc new-out)
        (close-output-port new-out)

        ;; Now we can run 'diff on these'.  For now, read to a string, but it'd be nice to be able
        ;; to do this differently.
        (run-diff diff-out tname new-temp-name)
        (close-output-port diff-out)
        ))

    ;; Using the diff, write the updated delta.
    (call (call-with-temp-rename-to nm) (_ out)
      (define new-header (add-delta header name tags))
      (fprintf out "\1t~a~%" (encode-deltas new-header))
      (call (call-with-main-in nm) (main-in)
        (call (call-with-input-file diff-name) (diff-in)
          (write-delta main-in diff-in out last-delta (add1 last-delta)))))
    ;(system* "/bin/ls" "-l" diff-name)
    ))

;;; Run diffs on two filenames, returning the diff output as a single string.
(define (run-diff out-port name-1 name-2)
  (parameterize ([current-output-port out-port])
    ;; Diff has a weird exit status: 0 same, 1 diffs, and 2 if trouble.  We only want to
    ;; abort in the case of '2'.
    (define status
      (system*/exit-code "/usr/bin/diff" name-1 name-2))
    (unless (< status 2)
      (error "Unable to run diff"))))

;;; Given a reader over the previous weave file, over the output of diff, and a writer for the new
;;; weave file, generate a new weave file containing the new delta.
(define (write-delta main-in diff-in out last-delta new-delta)
  (define sink (new weave-write-all-sink% [out-port out]))
  (define parser (make-weave-parser main-in sink last-delta))
  (define adding? #f)
  (define done? #f)
  (for ([line (in-lines diff-in)])
    (match line
      ;; Diff control line.
      [(regexp #px"^(\\d+)(,(\\d+))?([acd]).*$" (list _ a _ b cmd))
       (when adding?
         (send sink end new-delta)
         (set! adding? #f))
       (define left (string->number a))
       (define right (if b (string->number b) left))
       (case cmd
         ;; These have deletions.
         [("d" "c")
          (parser left)
          (send sink delete new-delta)
          (when (eof-object? (parser (add1 right)))
            (set! done? #t))
          (send sink end new-delta)]
         [else
           (when (eof-object? (parser (add1 right)))
             (set! done? #t))])
       ;; Check for additions.
       (case cmd
         [("c" "a")
          (send sink insert new-delta)
          (set! adding? #t)])]
      ;; Removed or separator line, ignore.
      [(regexp #px"^[<-].*") (void)]
      [(regexp #px"^> (.*)$" (list _ text))
       (send sink plain text #t)]
      [else (error "Unexpected diff line" line)]))
  (when adding?
    (send sink end new-delta))

  ;; Anything remaining should be processed.
  (unless done?
    (parser 0)))

(define read-sink%
  (class weave-sink%
    (init proc)
    (super-new)
    (define -proc proc)
    (define/override (plain text keep)
      (when keep
        (-proc text)))))

;;; Read a given delta.  Calls 'proc' with each line from the delta.
(define (read-delta nm delta proc)
  (define sink (new read-sink% [proc proc]))
  (call (call-with-main-in nm) (main-in)
    ((make-weave-parser main-in sink delta) 0)))

(module+ main
  (define *test-naming* (naming "." "sample" "dat" #t))

  ;;; Try a simple test
  (call-with-first-delta
    *test-naming* "First delta" (hasheq 'type "testing")
    (lambda (out)
      (displayln "asure-2.0" out)
      (displayln "-----" out)
      (displayln "d__root__ [gid 0 kind dir perm 493 uid 0 ]" out)
      (displayln "-" out)
      (displayln "u" out))))
