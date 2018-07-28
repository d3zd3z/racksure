#lang racket

(provide
  (contract-out
    ;; Return a new bytes that will be a quoted-printable version of
    ;; the input bytes.
    (escape [-> bytes? bytes?])

    ;; Invert the above escape procedure, producing the original byte string.
    (unescape [-> bytes? bytes?])))

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

;;; Unescape.
(define (unescape text)
  (define text-len (bytes-length text))
  (define len
    (let loop ([pos 0]
	       [len 0])
      (cond [(= pos text-len) len]
	    [(> pos text-len)
	     (error "Invalid quote in escaped text" text)]
	    [else
	      (let ([ch (bytes-ref text pos)])
		(if (= ch 61)
		  (loop (+ pos 3)
			(add1 len))
		  (loop (add1 pos)
			(add1 len))))])))
  (define result (make-bytes len))
  (let loop ([src 0]
	     [dest 0])
    (cond [(= src text-len) result]
	  [else
	    (let ([ch (bytes-ref text src)])
	      (if (= ch 61)
		(let ()
		  (define a (unhex-byte (bytes-ref text (add1 src))))
		  (define b (unhex-byte (bytes-ref text (+ src 2))))
		  (bytes-set! result dest (bitwise-ior (arithmetic-shift a 4) b))
		  (loop (+ src 3)
			(add1 dest)))
		(begin
		  (bytes-set! result dest ch)
		  (loop (add1 src)
			(add1 dest)))))])))

(define (unhex-byte a)
  (cond [(<= 48 a 57) (- a 48)]
	[(<= 65 a 70) (- a 55)]
	[(<= 97 a 102) (- a 87)]
	[else (error "Invalid hex digit" a)]))
