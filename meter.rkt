#lang racket/base

(require racket/contract)

;;; TODO: measure performance change with complex formatting of
;;; capturing arguments and not running 'format' except to update.

(provide
  (contract-out
    [update-meter (->* [string?] [#:force boolean?] #:rest any/c any/c)]
    [meter-update-interval
      (case-> (-> real? void?)
	      (-> real?))]
    [finalize-meter (-> any/c)]))

;;; A progress meter that interacts nicely with the logging system.

(define meter-update-interval (make-parameter 250))

;;; All output is synchronized using the message channel.
(define message-channel (make-channel))

;;; The current meter Arbitrary text, Should end with a newline.  Or
;;; #f if there is no message shown.
(define current-meter #f)

;;; The time we can next update a meter, or #f if none is shown, and
;;; we should update.
(define next-meter-show-ms #f)

;;; Evaluate body in a synchronized context.  This will always be run
;;; by the 'message-thread.  Note that the procedure may return before
;;; the action is actually run.
(define-syntax-rule (in-message-thread body ...)
  (channel-put message-channel
	       (lambda ()
		 body ...)))

;;; Channel messages are procedures that perform the channel action,
;;; in the context of the messaging thread.
(define message-thread
  (thread
    (lambda ()
      (let loop ()
	((channel-get message-channel))
	(loop)))))

;;; Clear the currently printed meter.
(define (clear-meter)
  (when current-meter
    (define lines
      (length (regexp-match-positions #rx"\n" current-meter)))
    (printf "\e[~aA\e[2K" lines)))

;;; Write the meter possibly with a new message (if no message is
;;; given, use the old one).  Assumes a previous message has been
;;; cleared.
(define (show-meter [message #f])
  (define to-show (or message current-meter))
  (when to-show
    (display to-show)
    (flush-output))
  (set! current-meter to-show))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These functions use the above calls synchronously.
;;; Print a log message
(define (show-log-message info)
  (in-message-thread
    (clear-meter)
    (printf "~a: ~a~%" (vector-ref info 0) (vector-ref info 1))
    (show-meter)))

;;; Update a meter printf style.  Force can be set to #t to show the
;;; message regardless of the current time.
(define (update-meter form #:force [frc #f] . v)
  (in-message-thread
    (when (or frc (>= (current-inexact-milliseconds) 
		      (or next-meter-show-ms 0)))
      (clear-meter)
      (show-meter (apply format form v))
      (set! next-meter-show-ms (+ (current-inexact-milliseconds)
				  (meter-update-interval))))))

;;; Finalize the meter. Does not clear, just marks that there is no
;;; message.
(define (finalize-meter)
  (in-message-thread
    (set! current-meter #f)
    (set! next-meter-show-ms #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We want to replace the system logger with our own so that all
;;; messages will interact nicely.

;;; Challenge, how to make this work nicely with the repl.
(define original-logger (current-logger))
(define meter-logger (make-logger))
(current-logger meter-logger)

;;; Start logging, returns the log processing thread.
(define (setup-logging)
  (define recv (make-log-receiver (current-logger) 'warning #f))
  (thread
    (lambda ()
      (let loop ()
	(define evt (sync recv))
	(show-log-message evt)
	(loop)))))

(define logging-thread (setup-logging))

;;; You can run this before re-enter!-ing this module.  It is
;;; purposefully not exported.
(define (stop-logging)
  (kill-thread message-thread)
  (kill-thread logging-thread)
  (current-logger original-logger))
