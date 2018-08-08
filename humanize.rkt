#lang racket/base

(require racket/format
         racket/contract)

(provide
  (contract-out
    ;;; Return a string representing the byte size in a human-friendly
    ;;; form.
    (humanize-bytes [-> real? string?])))

;;; Humanize a byte size.
(define prefixes '("  " "Ki" "Mi" "Gi" "Ti" "Pi" "Ei" "Zi" "Yi"))

(define (humanize-bytes size)
  (let loop ([pre prefixes]
             [size size])
    (if (< size 1024)
      (format "~a~aB" (~r size
                          #:precision (list '= (precision size))
                          #:min-width 6)
              (car pre))
      (loop (cdr pre) (/ size 1024)))))

(define (precision size)
  (cond [(< size 10) 3]
        [(< size 100) 2]
        [else 1]))
