#lang racket

;;; Weave parser.
;;;
;;; This parser is used both to extract deltas as well as for the generation of new deltas.  It
;;; works as a push parser, calling into an object for every line encountered.

(provide weave-sink%
         weave-write-sink%
         make-weave-parser)

;;; This base object implements the methods to handle all of the events, and ignore them.
(define weave-sink%
  (class object%
    (init)
    (super-new)

    (define/public (insert delta) (void))
    (define/public (delete delta) (void))
    (define/public (end delta) (void))
    (define/public (plain text keep) (void))))

;;; A weave sink that just writes the delta to the given output port.
(define weave-write-sink%
  (class weave-sink%
    (init out-port)
    (define -out-port out-port)
    (super-new)

    (define/override (plain text keep)
      (when keep
        (displayln text -out-port)))))

;;; The parser takes in input-port, a weave-sink% and a delta number to extract.  It returns a
;;; function that can parse up to a given line number.  The lines are numbered starting at one, so
;;; calling with line 0 will process all of the rest of the input.
(define (make-weave-parser input sink delta)
  ;; To avoid pushback, keep track of a possible pending line.  This will either be #f to indicate
  ;; no pending line, or have a line that we stopped just before.
  (define pending #f)

  ;; Are we currently keeping lines
  (define keeping #f)

  ;; The number of the line we are currently on
  (define line-num 0)

  ;; The states of the current delta.  This is a list of pairs, with the car as the delta number,
  ;; and the cdr the state, which should be one of the symbols keep, skip or next.  This should
  ;; always be sorted with the largest delta first.
  (define delta-state null)

  ;; Add a new delta to the state.
  (define (delta-push dnum state)
    (set! delta-state
      (sort (cons (cons dnum state) delta-state)
            >
            #:key car)))

  ;; Remove a given state.
  (define (delta-pop dnum)
    (set! delta-state
      (remove dnum delta-state
              (lambda (a b) (= a (car b))))))

  ;; Update the keeping value based on our current state.
  ;; This assumes they are sorted, and will use the first one of an acceptable state.
  (define (update-keep)
    (set! keeping
      (let loop ([lst delta-state])
        (cond [(empty? lst) #f]
              [(eq? (cdar lst) 'keep) #t]
              [(eq? (cdar lst) 'skip) #f]
              [else (loop (cdr lst))])))
    ; (printf "keep update: ~a ~v~%" keeping delta-state)
    )

  (lambda (line-num-to)
    (when pending
      (send sink plain pending keeping)
      (set! pending #f))

    (let loop ()
      (define line (read-line input))
      (cond
        [(eof-object? line) line]

        ;; Either blank lines, or lines not starting with a control are textual lines.  Give them to
        ;; the sink.
        [(or (zero? (string-length line))
             (not (char=? (string-ref line 0) #\u01)))
         (define stop?
           (if keeping
             (begin
               (set! line-num (add1 line-num))
               (if (= line-num line-num-to)
                 (begin
                   (set! pending line)
                   #t)
                 #f))
             #f))
         (if stop?
           line-num
           (begin
             (send sink plain line keeping)
             (loop)))]

        ;; Everything should be a control line.  Skip any that are too short.
        [(< (string-length line) 4)
         (loop)]

        ;; And ignore any that aren't insert/delete/end.
        [(case (string-ref line 1)
           [(#\I #\D #\E) #f]
           [else #t])
         (loop)]

        [else
          (define this-delta (string->number (substring line 3)))
          (case (string-ref line 1)
            [(#\E)
             (send sink end this-delta)
             (delta-pop this-delta)]
            [(#\I)
             (send sink insert this-delta)
             (delta-push this-delta (if (>= delta this-delta)
                                      'keep
                                      'skip))]
            [(#\D)
             (send sink delete this-delta)
             (delta-push this-delta (if (>= delta this-delta)
                                      'skip
                                      'next))]
            [else (error "Unexpected")])
          (update-keep)
          (loop)]))))

(module+ main
  ;;; An object that writes the output to a given output port
  (define weave-write%
    (class weave-sink%
      (init out-port)

      (define -out-port out-port)
      (super-new)

      ;; (define/override (insert delta)
      ;;   (printf "insert ~a~%" delta))
      ;; (define/override (delete delta)
      ;;   (printf "delete ~a~%" delta))
      ;; (define/override (end delta)
      ;;   (printf "end ~a~%" delta))
      (define/override (plain text keep)
        (when keep
          ; (printf "plain: ~v ~a~%" text keep)
          (displayln text -out-port)))))

  (call-with-input-file "s.tfile"
    (lambda (in)
      (call-with-output-file "v001"
        (lambda (out)
          (define sink (new weave-write% [out-port out]))
          ((make-weave-parser in sink 2) 0))))))
