#lang racket

;;; Operations on nodes.
(provide (struct-out node)
         (struct-out dir-node)
         node-kind)

(struct node (name atts) #:transparent)
(struct dir-node node (dirs files) #:transparent)

(define (node-kind nd)
  (hash-ref (node-atts nd) 'kind))
