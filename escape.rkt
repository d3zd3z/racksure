#lang racket

(provide
  (contract-out
    ;; Return a new bytes that will be a quoted-printable version of
    ;; the input bytes.
    (escape [-> bytes? bytes?])))

;;; File/path names in Posix systems are treated as a plain sequence
;;; of bytes.  To make this safe for both filenames and link targets,
;;; we use a simple quoted-printable type of encoding, which is
;;; defined by printable? below.

;;; Decide if the character needs to be escaped.
(define (printable? b)
  (define ch (integer->char b))
  (and (char<=? #\! ch #\~)
       (not (char=? ch #\=))
       (not (char=? ch #\[))
       (not (char=? ch #\]))))

(define (escape text)
  (define slen (bytes-length text))
  (define new-len slen)
  (for ([ch (in-bytes text)])
    (unless (printable? ch)
      (set! new-len (+ new-len 2))))
  (define result (make-bytes new-len))
  (let loop ([spos 0]
	     [dpos 0])
    (if (= spos slen)
      result
      (let ([ch (bytes-ref text spos)])
	(cond [(printable? ch)
	       (bytes-set! result dpos ch)
	       (loop (add1 spos) (add1 dpos))]
	      [else
		(bytes-set! result dpos 61)  ; #\=
		(bytes-set! result
			    (+ dpos 1)
			    (bytes-ref hex-digits (arithmetic-shift ch -4)))
		(bytes-set! result
			    (+ dpos 2)
			    (bytes-ref hex-digits (bitwise-and ch 15)))
		(loop (add1 spos) (+ dpos 3))])))))

(define hex-digits #"0123456789abcdef")
