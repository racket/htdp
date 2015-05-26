#lang racket/base
(require wxme
         racket/class
         racket/list
         "value-turtles-reader.rkt")
(provide reader
         wxme-turtle?
         wxme-turtle->snip)

(define reader
  (new
   (class* object% (snip-reader<%>)
     (define/public (read-header version stream) (void))
     (define/public (read-snip text-only? version stream)
       (wxme-turtle
        (vec->struc
         (read
          (open-input-bytes
           (send stream read-raw-bytes "value-turtles-snip"))))))
     (super-new))))

(struct wxme-turtle (exp))
(define (wxme-turtle->snip turtle-snip% tv)
  (cond
    [(wxme-turtle? tv)
     (define sexp (wxme-turtle-exp tv))
     (make-object turtle-snip%
       (first sexp)
       (second sexp)
       (third sexp)
       (fourth sexp)
       (fifth sexp))]
    [else tv]))