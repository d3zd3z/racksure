#lang racket

(require ffi/unsafe
	 ffi/unsafe/alloc
	 ffi/unsafe/define
	 dynext/compile
	 dynext/link
	 racket/runtime-path)

;;; Linux interfaces to low-level file operations.
;;; TODO: Check dates so we don't compile every load.
;(compile-extension #f "c-stat.c" "cstat.o" '())
;(link-extension #f '("c-stat.o") "libcstat.so")

(define-runtime-path c-stat-source "c-stat.c")
(define-runtime-path c-stat-so "libcstat.so")

;;; Compile the C support stubs if necessary.
(when (or (not (file-exists? c-stat-so))
	  (>= (file-or-directory-modify-seconds c-stat-source)
	      (file-or-directory-modify-seconds c-stat-so)))
  (eprintf "Compiling ~a~%" c-stat-source)
  (let ([gcc (find-executable-path "gcc")])
    (unless gcc
      (error "Unable to find compiler 'gcc' in path"))
    (define (safe-run . args)
      (eprintf "Running ~a~%" args)
      (let ([result (apply system* args)])
	(eprintf "result ~a~%" result)
	(unless result
	  (error "Error running compilation: ~a" args))))
    (safe-run gcc "-O2" "-fpic" "-shared" "-o" c-stat-so c-stat-source)))

;  gcc -O2 -fpic -c c-stat.c
;  gcc -fpic -shared -o libcstat.so c-stat.o

;(define-ffi-definer define-libc (ffi-lib "/lib64/libc.so.6"))

;;; These are at least consistant between Linux and BSD-based systems
;;; (including MacOS).
(define S_IFMT  #o170000)
(define S_IFDIR #o040000)
(define S_IFCHR #o020000)
(define S_IFBLK #o060000)
(define S_IFREG #o100000)
(define S_IFIFO #o010000)
(define S_IFLNK #o120000)
(define S_IFSOCK #o140000)

(define-ffi-definer define-cstat (ffi-lib c-stat-so))

(define-cstruct _portstat
		([dev _int64]
		 [ino _int64]
		 [nlink _int64]
		 [mode _int64]
		 [uid _int64]
		 [gid _int64]
		 [rdev _int64]
		 [size _int64]
		 [blksize _int64]
		 [blocks _int64]
		 [atime_sec _uint64]
		 [atime_nsec _uint64]
		 [mtime_sec _uint64]
		 [mtime_nsec _uint64]
		 [ctime_sec _uint64]
		 [ctime_nsec _uint64]))

(define-cstat portable_lstat
	      (_fun #:save-errno 'posix
		    _bytes (o : (_ptr o _portstat))
		    -> (r : _int)
		    -> (values r o)))

(define-cpointer-type _DIR*)

(define-cstruct _portdirent
		([ino _int64]
		 [kind _uint8]
		 [name (_array/vector _uint8 256)]))

(define-cstat portable_closedir
	      (_fun #:save-errno 'posix
		    _DIR*
		    -> _int)
	      #:wrap (deallocator))

(define-cstat portable_readdir
	      (_fun #:save-errno 'posix
		    _DIR* (o : (_ptr o _portdirent))
		    -> (r : _int)
		    -> (values r o)))

(define-cstat portable_opendir
	      (_fun #:save-errno 'posix
		    _bytes
		    -> _DIR*/null)
	      #:wrap (allocator portable_closedir))

(define-cstat readlink
	      (_fun #:save-errno 'posix
		    _bytes _bytes _ssize
		    -> _ssize))

(define (lstat path)
  (define-values (r buf) (portable_lstat path))
  (unless (zero? r)
    (error "Unable to stat file" path (saved-errno)))
  buf)

;;; User-friendly readlink.  Assume more are reasonably short.
(define (read-link path)
  (let loop ([bufsiz 128])
    (define buf (make-bytes bufsiz))
    (define res (readlink path buf bufsiz))
    (cond [(= res bufsiz)
	   (loop (* bufsiz 2))]
	  [(positive? res)
	   (subbytes buf 0 res)]
	  [else (error "Error reading symlink" path (saved-errno))])))

;;; Gosure collects the following information:
;;; DIR: kind=dir  - perm gid uid
;;; REG: kind=file - ctime, gid, ino, mtime, perm, sha1, size, uid
;;; LNK: kind=lnk - uid gid perm targ
;;; CHR: kind=chr  - uid gid perm rdev
;;; BLK: kind=blk  - uid gid perm rdev
;;; FIFO kind=fifo - uid gid perm
;;; SOCK kind=sock - uid gid perm

;;; Convert a stat field accessor to a string generator.
(define (stat-field->getter getter)
  (lambda (path sbuf)
    (getter sbuf)))

(define base-atts `((uid . ,(stat-field->getter portstat-uid))
		    (gid . ,(stat-field->getter portstat-gid))
		    (perm . ,(stat-field->getter
			       (lambda (sbuf)
				 (bitwise-and (portstat-mode sbuf)
					      (bitwise-not S_IFMT)))))))

(define att-values
  (hasheqv
    S_IFDIR `(dir ,@base-atts)
    S_IFREG `(reg
	       (ctime . ,(stat-field->getter portstat-ctime_sec))
	       (mtime . ,(stat-field->getter portstat-mtime_sec))
	       (ino . ,(stat-field->getter portstat-ino))
	       (size . ,(stat-field->getter portstat-size))
	       ,@base-atts)
    S_IFLNK `(link
	       (targ . ,(lambda (path sbuf)
			  (read-link path)))
	       ,@base-atts)
    S_IFCHR `(chr
	       (rdev . ,(stat-field->getter portstat-rdev))
	       ,@base-atts)
    S_IFBLK `(blk
	       (rdev . ,(stat-field->getter portstat-rdev))
	       ,@base-atts)
    S_IFIFO `(fifo ,@base-atts)
    S_IFSOCK `(sock ,@base-atts)))
	      
;;; Convert the posix stat structure given into an att-map.
(define (portstat->attmap path sbuf)
  (define mode (bitwise-and (portstat-mode sbuf) S_IFMT))
  (define info (hash-ref att-values mode
			 (lambda ()
			   (error "Unrecognized file type" path mode))))
  (define atts (for/hasheqv ([kv (in-list (cdr info))])
		 (values (car kv) ((cdr kv) path sbuf))))
  (hash-set atts 'kind (car info)))

(struct dirent (name kind ino) #:transparent)

(struct node (name atts) #:transparent)
(struct dir-node node (dirs files) #:transparent)

(define (node-kind nd)
  (hash-ref (node-atts nd) 'kind))

;; Scan a tree, building up node/dir-node structures associated with
;; it.
;; TODO: Handle errors, warn and skip them.
(define (scan-tree name path)
  (define atts (portstat->attmap path (lstat path)))
  (define dir? (eq? (hash-ref atts 'kind) 'dir))
  (if dir?
    (let ([children (sort (read-directory path) < #:key dirent-ino)]
	  [dirs '()]
	  [files '()])
      (for ([child (in-list children)])
	(define child-name (dirent-name child))
	(define child-path (bytes-append path #"/" child-name))
	(define nd (scan-tree child-name child-path))
	(if (eq? (node-kind nd) 'dir)
	  (set! dirs (cons nd dirs))
	  (set! files (cons nd files))))
      (dir-node name atts
		(sort dirs bytes<? #:key node-name)
		(sort files bytes<? #:key node-name)))
    (node name atts)))

(define (read-directory path)
  (define dirp (portable_opendir path))
  (unless dirp
    (error "Error opening directory" path (saved-errno)))
  (let loop ([result '()])
    (define-values (r ent) (portable_readdir dirp))
    (cond [(zero? r)
	   (define name (get-name (portdirent-name ent)))
	   (if (or (bytes=? name #".")
		   (bytes=? name #".."))
	     (loop result)
	     (loop (cons (dirent name
				 (decode-kind (portdirent-kind ent))
				 (portdirent-ino ent))
			 result)))]
	  [(zero? (saved-errno))
	   (portable_closedir dirp)
	   result]
	  [else (error "Error reading directory" (saved-errno))])))

(define (get-name namearray)
  (define len (let loop ([i 0])
		(when (= i (vector-length namearray))
		  (error "Non-null-terminated name returned in readdir"))
		(define ch (vector-ref namearray i))
		(if (zero? ch)
		  i
		  (loop (add1 i)))))
  (define result (make-bytes len))
  (for ([i (in-range len)])
    (bytes-set! result i (vector-ref namearray i)))
  result)

(define (decode-kind k)
  (case (integer->char k)
    [(#\d) 'dir]
    [(#\.) 'nondir]
    [else 'unknown]))
