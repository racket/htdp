#lang racket/base

(require (for-syntax racket/base))

(provide stepper-syntax-property
         with-stepper-syntax-properties
         
         skipto/cdr
         skipto/cddr
         skipto/first
         skipto/second
         skipto/third
         skipto/fourth
         skipto/firstarg)


;; stepper-syntax-property : like syntax property, but adds properties to an association
;; list associated with the syntax property 'stepper-properties
;; unfortunately, syntax transformers cons together properties from separate trees,
;; so instead of an association list, this can be a cons tree of association lists.
(define stepper-syntax-property
  (case-lambda
    [(stx tag)
     (unless (member tag known-stepper-syntax-property-names)
       (raise-type-error 'stepper-syntax-property
                         "known stepper property symbol" 1 stx tag))
     (let ([stepper-props (syntax-property stx 'stepper-properties)])
       (cond [stepper-props
              (define props-alist
                (flatten-association-list-ilist stepper-props))
              (define table-lookup (assq tag stepper-props))
              (if table-lookup
                 (cadr table-lookup)
                 #f)]
             [else #f]))]
    [(stx tag new-val) 
     (unless (member tag known-stepper-syntax-property-names)
       (raise-type-error 'stepper-syntax-property
                         "known stepper property symbol" 1 
                         stx tag new-val))
     (syntax-property stx 'stepper-properties
                      (cons (list tag new-val)
                            (flatten-association-list-ilist
                             (or (syntax-property stx 'stepper-properties)
                                 null))))]))

;; given an improper list of alists, return a single alist
;; note that this doesn't try to combine or eliminate multiple
;; uses of the same key, they just wind up appearing twice in the
;; result, with one binding shadowing the other
(define (flatten-association-list-ilist alist-tree)
  (cond [(pair? alist-tree)
         (cond [(pair? (car alist-tree))
                ;; this one is the real check:
                (cond [(pair? (caar alist-tree))
                       ;; it must be a cons pair of alists, not
                       ;; just an alist:
                       (append (car alist-tree)
                               (flatten-association-list-ilist (cdr alist-tree)))]
                      [else
                       alist-tree])]
               [else
                (raise-argument-error 'flatten-association-list-tree
                                      ;; n.b. improper *should* appear here;
                                      ;; that is, we are expecting an improper list.
                                      "improper list of association trees"
                                      0 alist-tree)])]
        [(null? alist-tree) alist-tree]
        [else
         (raise-argument-error 'flatten-association-list-tree
                               "improper list of association lists"
                               0 alist-tree)]))

(module+ test
  (require rackunit)
  (check-equal?
   (flatten-association-list-ilist '()) '())
  (check-equal?
   (flatten-association-list-ilist
    '(((stepper-and/or-clauses-consumed 2) (stepper-hint comes-from-or)) (stepper-and/or-clauses-consumed 2) (stepper-hint comes-from-or)))
   '((stepper-and/or-clauses-consumed 2) (stepper-hint comes-from-or) (stepper-and/or-clauses-consumed 2) (stepper-hint comes-from-or)))
  (check-equal?
   (flatten-association-list-ilist
    '((stepper-and/or-clauses-consumed 2) (stepper-hint comes-from-or) (stepper-and/or-clauses-consumed 2) (stepper-hint comes-from-or)))
   '((stepper-and/or-clauses-consumed 2) (stepper-hint comes-from-or) (stepper-and/or-clauses-consumed 2) (stepper-hint comes-from-or))))



;; if the given property name isn't in this list, signal an error...
(define known-stepper-syntax-property-names 
  '(stepper-skip-completely
    stepper-hint
    stepper-define-type
    stepper-xml-hint
    stepper-xml-value-hint
    stepper-proc-define-name
    stepper-orig-name
    stepper-prim-name
    stepper-binding-type
    stepper-no-lifting-info
    stepper-and/or-clauses-consumed
    stepper-skipto
    stepper-skipto/discard
    stepper-replace
    stepper-else
    stepper-black-box-expr
    stepper-test-suite-hint
    stepper-highlight
    stepper-fake-exp
    stepper-args-of-call
    stepper-hide-completed
    stepper-hide-reduction
    stepper-use-val-as-final
    stepper-lifted-name
    lazy-op
    ;; used temporarily to help locate syntax expressions
    ;; when adding new skipto annotations:
    finder
    ))


;; with-stepper-syntax-properties : like stepper-syntax-property, 
;; but in a "let"-like form
(define-syntax (with-stepper-syntax-properties stx)
  (syntax-case stx ()
    [(_ ([property val] ...) body)
     (foldl (lambda (property val b)
              #`(stepper-syntax-property #,b #,property #,val))
            #'body
            (syntax->list #`(property ...))
            (syntax->list #`(val ...)))]))


;; commonly used values for stepper-syntax-property:
(define skipto/cdr `(cdr))
(define skipto/cddr `(cdr cdr))
(define skipto/first `(car))
(define skipto/second `(cdr car))
(define skipto/third `(cdr cdr car))
(define skipto/fourth `(cdr cdr cdr car))
(define skipto/firstarg (append skipto/cdr skipto/second))
