#lang racket

;;; The attribute maps.  Most metadata is stored as a mapping between
;;; simple symbols and a value.  There are different encodings of the
;;; value, depending on the underlying type.
;;;
;;; integer: encoded as the integer in decimal
;;; kind: as a simple string
;;; sha1: The hash is encoded in hex
;;; targ: An escaped string.

(require parser-tools/lex
	 (prefix-in : parser-tools/lex-sre)
	 "escape.rkt"
         "node.rkt"
         "posix.rkt"
	 (only-in file/sha1 bytes->hex-string hex-string->bytes))

(provide attmap->bytes
         decode-line)

(module+ test
  (require rackunit))

;;; Convert an attribute map to a textual representation (as a
;;; byte-string).
(define (attmap->bytes atts)
  (define keys (sort (hash-keys atts) symbol<?))
  (define out (open-output-bytes))
  (display #\[ out)
  (for ([k (in-list keys)])
    (display k out)
    (display #\space out)
    (match (cons k (hash-ref atts k))
      [(cons _ (? integer? num))
       (display num out)]
      [(cons _ (? symbol? item))
       (display item out)]
      [(cons 'sha1 (? bytes? item))
       (display (bytes->hex-string item) out)]
      [(cons 'targ (? bytes? item))
       (display (escape item) out)]
      [item
	(error "Unsupported attmap value" k item)])
    (display #\space out))
  (display #\] out)
  (get-output-bytes out))

(define lex
  (lexer
    [(eof) eof]
    ["["   'begin]
    ["]"   'end]
    [" "   'space]
    [(:+ (:~ #\[ #\] #\space))  lexeme]))

;;; Lex the beginning of the document.
(define init-lex
  (lexer
    [(eof) eof]
    ["adump-2.0\n-----\n" 'header]))

;;; At the start of each line, use this lexer.
(define bol-lex
  (lexer
    [(eof) eof]
    [#\d 'dir]
    [#\- 'sep]
    [#\u 'up]
    [#\f 'file]))

(define rest-lex
  (lexer
    [(eof) eof]
    [#\newline  'newline]
    ["["   'begin]
    ["]"   'end]
    [" "   'space]
    [(:+ (:~ #\[ #\] #\space #\newline))  lexeme]))

;;; Parse a line from the input.  Will return one of the following forms:
;;; (cons 'dir node?)
;;; (cons 'file node?)
;;; 'sep
;;; 'up
;;; or eof-object?
(define (decode-line line)
  (define in (open-input-string line))
  (define (expect token)
    (define tok (rest-lex in))
    (unless (eq? tok token)
      (error "Unexpected token" tok token)))
  (define (decode-atts)
    (let loop ([result (hasheq)])
      (define key (rest-lex in))
      (cond [(eq? key 'end)
             result]
            [(string? key)
             (define key-sym (string->symbol key))
             (expect 'space)
             (define value (rest-lex in))
             (unless (string? value)
               (error "Expecting string" value))
             (expect 'space)
             (define val-value ((hash-ref val-kinds key-sym
                                          (lambda ()
                                            (error "Unknown att key" key-sym)))
                                value))
             (loop (hash-set result key-sym val-value))]
            [else (error "Unexpected token" key)])))
  (match (bol-lex in)
    [(and value (or 'dir 'file))
     (define name (rest-lex in))
     (expect 'space)
     (expect 'begin)
     (define atts (decode-atts))
     (cons value (node (unescape (string->bytes/utf-8 name)) atts))]
    [item
      item]))

(define val-kinds
  (hasheq
    'uid string->number
    'gid string->number
    'ino string->number
    'perm string->number
    'size string->number
    'rdev string->number
    'ctime string->number
    'mtime string->number
    'atime string->number
    'kind string->symbol
    'sha1 hex-string->bytes
    'targ (lambda (x)
            (unescape (string->bytes/utf-8 x)))))

(define (split-input text)
  (define in (open-input-bytes text))
  (define tokens
    (match (port->list lex in)
      [(list-rest 'begin more) more]
      [other (error "Invalid attributes" other)]))
  (let loop ([result (hasheq)]
	     [tokens tokens])
    (match tokens
      [(list-rest (? string? key) 'space (? string? val) 'space more)
       (define key-sym (string->symbol key))
       (define value ((hash-ref val-kinds key-sym
				(lambda ()
				  (error "Unknown att key" key-sym)))
		      val))
       (loop (hash-set result key-sym value)
	     more)]
      [(list 'end)
       result]
      [other
	(error "Invalid attributes" other)])))

(module+ test

  (check-equal? (split-input (bytes-append #"[uid 12345 gid 8421 ino 184672 perm 19274 size 174926 "
					   #"rdev 197482 ctime 197186 mtime 2871974 atime 197386 "
					   #"kind floop sha1 123456789abcdef0 ]"))
		(hasheq 'atime 197386
			'ctime 197186
			'gid 8421
			'ino 184672
			'kind 'floop
			'mtime 2871974
			'perm 19274
			'rdev 197482
			'sha1 #"\0224Vx\232\274\336\360"
			'size 174926
			'uid 12345)))
