#lang racket

;;; Operations on nodes.
(provide (struct-out node)
         (struct-out dir-node)
         node-kind
         compare-trees)

(struct node (name atts) #:transparent)
(struct dir-node node (dirs files) #:transparent)

(define (node-kind nd)
  (hash-ref (node-atts nd) 'kind))

;;; Given a path, append the name of this node as a path.
(define (node-full-path path nd)
  (build-path path (bytes->path (node-name nd))))

;;; Compare two node trees, showing added files/directores etc.
(define (compare-trees left right)
  (compare-subtrees left right "."))

(define (compare-subtrees left right path)
  (att-compare left right path)
  (compare-children (dir-node-dirs left) (dir-node-dirs right) path)
  (compare-files (dir-node-files left) (dir-node-files right) path))

;; Compare the left and right children.
(define (compare-children left right path)
  (let loop ([left left]
             [right right])
    (match* (left right)
      ;; The done case.
      [((list) (list)) #f]

      ;; Extras on the left side.
      [((cons lelt lrest) (list))
       (delete-dir path lelt)
       (loop lrest null)]

      ;; Extras on right.
      [((list) (cons relt rrest))
       (add-dir path relt)
       (loop null rrest)]

      [((cons lelt lrest) (cons relt rrest))
       (cond [(bytes<? (node-name lelt) (node-name relt))
              (delete-dir path lelt)
              (loop lrest (cons relt rrest))]
             [(bytes>? (node-name lelt) (node-name relt))
              (add-dir path relt)
              (loop (cons lelt lrest) rrest)]
             ;; Otherwise, names are equal, compare them.
             [else
               (compare-subtrees lelt
                                 relt
                                 (node-full-path path lelt))
               (loop lrest rrest)])])))

;;; Compare the left and right files.
(define (compare-files left right path)
  (let loop ([left left]
             [right right])
    (match* (left right)
      ;; The done case.
      [((list) (list)) #f]

      ;; Extras on the left side.
      [((cons lelt lrest) (list))
       (delete-file path lelt)
       (loop lrest null)]

      ;; Extras on right.
      [((list) (cons relt rrest))
       (add-file path relt)
       (loop null rrest)]

      [((cons lelt lrest) (cons relt rrest))
       (cond [(bytes<? (node-name lelt) (node-name relt))
              (delete-file path lelt)
              (loop lrest (cons relt rrest))]
             [(bytes>? (node-name lelt) (node-name relt))
              (add-file path relt)
              (loop (cons lelt lrest) rrest)]
             ;; Otherwise names are equal, just compare atts.
             [else
               ;; TODO: The build path could be lazy.
               (att-compare lelt relt path)
               (loop lrest rrest)])])))

(define (att-compare lnode rnode path)
  (define latts (clean-atts (node-atts lnode)))
  (define ratts (clean-atts (node-atts rnode)))
  (unless (equal? latts ratts)
    (let loop ([keys (sort (hash-keys latts) symbol<?)]
               [ratts ratts]
               [wrongs null])
      (if (null? keys)
        (let ()
          (unless (null? wrongs)
            (define names (string-append* (add-between (map symbol->string (reverse wrongs)) ",")))
            (printf "  [~a] ~a~%"
                    (~a names #:min-width 20)
                    (node-full-path path lnode)))
          (for ([att (hash-keys ratts)])
            (warn-att "removed" att)))
        (let ()
          (define key (car keys))
          (define left (hash-ref latts key))
          (define right (hash-ref ratts key #f))
          (unless right
            (warn-att "added" key))
          (loop (cdr keys) (hash-remove ratts key)
                (if (equal? left right)
                  wrongs
                  (cons key wrongs))))))))

;;; Remove the atts that we don't want to compare.
(define (clean-atts atts)
  (hash-remove (hash-remove atts 'ctime) 'ino))

(define warn-att
  (let ([seen (set)])
    (lambda (phase att)
      (unless (set-member? seen (cons phase att))
        (printf "warn: attribute ~a ~a~%" att phase)
        (set! seen (set-add seen (cons phase att)))))))

;; All of the printers.  TODO: Print the names safely.
(define (add-dir path item)
  (printf "+ dir                     ~a~%" (node-full-path path item)))
(define (delete-dir path item)
  (printf "- dir                     ~a~%" (node-full-path path item)))
(define (add-file path item)
  (printf "+ file                    ~a~%" (node-full-path path item)))
(define (delete-file path item)
  (printf "- file                    ~a~%" (node-full-path path item)))
