#lang racket/base

(require racket/list
         racket/match
         racket/contract
         racket/class
         racket/unit
         "syntax-property.rkt"
         ;; remove when no longer needed:
         "shared-typed.rkt")

#;(provide/contract
   [skipto/auto (syntax?
                 (syntax? syntax? . -> . syntax?) 
                 (syntax? . -> . syntax?)
                 . -> . 
                 syntax?)]
   [in-closure-table (-> any/c boolean?)]
   [attach-info (-> syntax? syntax? syntax?)]
   [transfer-info (-> syntax? syntax? syntax?)])

(provide
 (contract-out
  [syntax->hilite-datum 
   ((syntax?) (#:ignore-highlight? boolean?) . ->* . any)] ; sexp with explicit tags
  [syntax->interned-datum (-> syntax? any)]
  [skipto/auto (-> syntax? boolean? (-> syntax? syntax?)
                   syntax?)]
  [check-path (-> syntax? (listof symbol?) syntax?)]
  )
 
 attach-info
 transfer-info
 *unevaluated* 
 struct-flag
 multiple-highlight
 flatten-take
 zip
 syntax-pair-map
 rebuild-stx ; datum syntax -> syntax
 break-kind? ; predicate
 re-intern-identifier
 finished-xml-box-table
 saved-code-inspector 
 (struct-out annotated-proc) 
 view-controller^
 stepper-frame^
 let-counter)




  

; multiple-highlight is used to indicate multiple highlighted expressions
(define multiple-highlight (gensym "multiple-highlight-"))

; *unevaluated* is the value assigned to temps before they are evaluated. It's not a symbol so
; it won't need quoting in the source.  Bit of a hack, I know.
(define-struct *unevaluated-struct* ())
(define *unevaluated* (make-*unevaluated-struct*))

; struct-flag : uninterned symbol
(define struct-flag (gensym "struct-flag-"))

;  (define expr-read read-getter)
;  (define set-expr-read! read-setter)

(define (flatten-take n a-list)
  (apply append (take a-list n)))

(define-values (closure-table-put! closure-table-lookup in-closure-table)
  (let ([closure-table (make-weak-hash)])
    (values
     (lambda (key value)
       (hash-set! closure-table key value)
       key)                                  ; this return allows a run-time-optimization
     (lambda args ; key or key & failure-thunk
       (apply hash-ref closure-table args))
     (lambda (key)
       (let/ec k
         (hash-ref closure-table key (lambda () (k #f)))
         #t)))))

;(begin (closure-table-put! 'foo 'bar)
;       (and (eq? (in-closure-table 'blatz) #f)
;            (eq? (in-closure-table 'foo) #t)))

;; zip : (listof 'a) (listof 'b) (listof 'c) ...
;;       -> (listof (list 'a 'b 'c ...))
;; zip reshuffles lists of items into a list of item-lists.  Look at the
;; contract, okay?

(define zip
  (lambda args
    (apply map list args)))

(define let-counter
  (stepper-syntax-property #'let-counter 'stepper-binding-type 'stepper-temp))


; syntax-pair-map (using the def'ns of the Racket docs):

(define (syntax-pair-map pair fn)
  (cons (fn (car pair))
        (cond [(syntax? (cdr pair))
               (fn (cdr pair))]
              [(pair? (cdr pair))
               (syntax-pair-map (cdr pair) fn)]
              [(null? (cdr pair))
               null])))



(define saved-code-inspector (variable-reference->module-declaration-inspector
                              (#%variable-reference)))

(define (rebuild-stx new old)
  (datum->syntax old new old old))

(define break-kind?
  (symbols 'normal-break 'normal-break/values 'result-exp-break
           'result-value-break 'double-break 'late-let-break
           'expr-finished-break 'define-struct-break))

; functional update package

;; a trace is one of
;; (cons 'car trace)
;; (cons 'cdr trace)
;; (cons 'syntax-e trace)
;; (cons 'both (list trace trace))
;; null

;; given a symbol, return the corresponding
;; "rebuilding" outward traversal function
(define (rebuild-up-fn fn)
  (case fn 
    [(car)      (λ (stx new) (cons new (cdr stx)))]
    [(cdr)      (λ (stx new) (cons (car stx) new))]
    [(syntax-e) (λ (stx new) (rebuild-stx new stx))]
    [(both-l both-r) (lambda (stx a b) (cons a b))]
    [else (raise-argument-error 'rebuild-up-fn
                                "legal traversal symbol"
                                0 fn)]))

;; given a symbol, return the corresponding
;; "discard" outward traversal function
;; (basically, just return (λ (x y) y))
(define (discard-up-fn fn)
  (case fn
    [(car cdr syntax-e) (λ (stx new) new)]
    [(both-l) (lambda (stx a b) a)]
    [(both-r) (lambda (stx a b) b)]
    [else (raise-argument-error 'discard-up-fn
                                "legal traversal symbol"
                                0 fn)]))

;; like car, but provide a useful error message if given a non-pair
(define (noisy-car arg)
  (cond [(pair? arg) (car arg)]
        [else (raise-argument-error 'noisy-car "pair in syntax traversal" 0 arg)]))

;; like cdr,  but provide a useful error message if given a non-pair
(define (noisy-cdr arg)
  (cond [(pair? arg) (cdr arg)]
        [else (raise-argument-error 'noisy-cdr "pair in syntax traversal" 0 arg)]))

;; like syntax-e,  but provide a useful error message if given a non-syntax-object
(define (noisy-syntax-e arg)
  (cond [(syntax? arg) (syntax-e arg)]
        [else (raise-argument-error 'noisy-syntax-e "syntax object in syntax traversal" 0 arg)]))

;; map a symbol in '(car cdr syntax-e) to the appropriate projector
(define (down-fn-finder fn)
  (case fn
    [(car) noisy-car]
    [(cdr) noisy-cdr]
    [(syntax-e) noisy-syntax-e]
    [else (error 'down-mapping "called on something other than 'car, 'cdr, & 'syntax-e: ~v" fn)]))

;; given a list of traversal symbols[*] and a val and a core-fn and an up-fn-finder,
;; use the traversal symbols to find the target expression, apply the core-fn to
;; it, and the use the up-fn-finder to rebuild the syntax object
;; If the stx is a syntax? object and the fn-list is not empty, infer the
;; existence of a syntax unwrap and re-wrap
;; [*] actually, it can be a tree... it looks like both-l and both-r
;; split annotation into a tree where one path is for the car of the syntax
;; pair and the other is for the cdr. I think this is only used by lazy.
(define (update fn-list stx core-fn up-fn-finder)
  (cond
    [(null? fn-list) (core-fn stx)]
    [else
     (define fn (car fn-list))
     ;; NB this is bogus in the case of both-l and both-r:
     (define rest-fns (cdr fn-list))
     (cond
       [(syntax? stx)
        (define up (up-fn-finder 'syntax-e))
        (up stx (update fn-list (syntax-e stx) core-fn up-fn-finder))]
       ;; simply ignore the syntax-e symbol
       ;; (this clause should not be necessary after the now-obsolete syntax-e label is
       ;; removed everywhere):
       [(equal? fn 'syntax-e)
        (update rest-fns stx core-fn up-fn-finder)]
       [(member fn '(both-l both-r))
        (define up (up-fn-finder fn))
        (up stx
            (update (cadr fn-list) (car stx) core-fn up-fn-finder)
            (update (caddr fn-list) (cdr stx) core-fn up-fn-finder))]
       [else
        (define up (up-fn-finder fn))
        (define down (down-fn-finder (car fn-list)))
        (up stx (update rest-fns (down stx) core-fn up-fn-finder))])]))

;; for debugging, do the "down" part only
(define (check-path stx fn-list)
  (update fn-list stx (λ (x) x) discard-up-fn))

#;(display (equal? (update '(cdr cdr car both-l (car) (cdr))
                           `(a . (b ((1) c . 2) d))
                           (lambda (x) (+ x 1))
                           'rebuild)
                   `(a . (b ((2) c . 3) d))))
  
  
;; skipto/auto : syntax?
;;               (syntax? syntax? . -> . syntax?)
;;               (syntax? . -> . syntax?)
;; "skips over" part of a tree to find a subtree indicated by the
;; stepper-skipto property at the root of the tree, and applies
;; the transformer to it. If no stepper-skipto or stepper-skipto/discard
;; property is present, apply the transformer to the whole tree.
;; The rebuild-mapper is used to rebuild the tree (one rebuilder
;; rebuilds the tree, the other just discards the context completely).
(define (skipto/auto stx force-discard? transformer)
  (cond
    [(stepper-syntax-property stx 'stepper-skipto)
     =>
     (lambda (x) (update x stx 
                         (lambda (stx)
                           (skipto/auto stx force-discard? transformer)) 
                         (if force-discard?
                             discard-up-fn
                             rebuild-up-fn)))]
    [(stepper-syntax-property stx 'stepper-skipto/discard)
     =>
     (lambda (x) (update x stx
                         (lambda (stx)
                           (skipto/auto stx force-discard? transformer)) 
                         discard-up-fn))]
    [else (transformer stx)]))

;; take info from source expressions to reconstructed expressions

(define (attach-info to-exp from-exp)
  (let* ([attached
          (syntax-property
           to-exp 'stepper-properties
           (append (or (syntax-property from-exp 'stepper-properties)
                       null)
                   (or (syntax-property to-exp 'stepper-properties)
                       null)))]
         [attached
          (syntax-property attached 'user-source (syntax-source from-exp))]
         [attached
          (syntax-property attached 'user-position (syntax-position from-exp))]
         [attached
          (syntax-property attached 'user-origin (syntax-property from-exp 'origin))])
    attached))

;; transfer info from reconstructed expressions to other reconstructed
;; expressions

(define (transfer-info to-exp from-exp)
  (let* ([attached
          (syntax-property
           to-exp 'stepper-properties
           (append (or (syntax-property from-exp 'stepper-properties)
                       null)
                   (or (syntax-property to-exp 'stepper-properties)
                       null)))]
         [attached
          (syntax-property attached 'user-source (syntax-property from-exp 'user-source))]
         [attached
          (syntax-property attached 'user-position (syntax-property from-exp 'user-position))]
         [attached
          (syntax-property attached 'user-origin (syntax-property from-exp 'user-origin))])
    attached))

;; re-intern-identifier : (identifier? -> identifier?)
;; re-intern-identifier : some identifiers are uninterned, which breaks
;; test cases.  re-intern-identifier takes an identifier to a string
;; and back again to make in into an interned identifier.
(define (re-intern-identifier identifier)
  #`#,(string->symbol (symbol->string (syntax-e identifier))))

;; syntax->hilite-datum : takes a syntax object with zero or more
;; subexpressions tagged with the 'stepper-highlight', 'stepper-xml-hint', and 'stepper-xml-value-hint' syntax-properties
;; and turns it into a datum, where expressions with the named
;; properties result in (hilite <datum>), (xml-box <datum>), (scheme-box <datum>) and (splice-box <datum>) rather than <datum>. It also
;; re-interns all identifiers.  In cases where a given expression has more than one of these, they appear in the order
;; listed.  That is, an expression with both highlight and xml-box annotations will result it (hilite (xml-box <datum>))
;; 
;; this procedure is useful in checking the output of the stepper.

(define (syntax->hilite-datum stx #:ignore-highlight? [ignore-highlight? #f])
  (let ([datum (syntax-case stx ()
                 [(a . rest) (cons (syntax->hilite-datum #`a) (syntax->hilite-datum #`rest))]
                 [id
                  (identifier? stx)
                  (string->symbol (symbol->string (syntax-e stx)))]
                 [else (if (syntax? stx)
                           (syntax->datum stx)
                           stx)])])
    (let* ([it (case (stepper-syntax-property stx 'stepper-xml-hint)
                 [(from-xml-box) `(xml-box ,datum)]
                 [(from-scheme-box) `(scheme-box ,datum)]
                 [(from-splice-box) `(splice-box ,datum)]
                 [else datum])]
           [it (case (stepper-syntax-property stx 'stepper-xml-value-hint)
                 [(from-xml-box) `(xml-box-value ,it)]
                 [else it])]
           [it (if (and (not ignore-highlight?)
                        (stepper-syntax-property stx 'stepper-highlight))
                   `(hilite ,it)
                   it)])
      it)))

;; finished-xml-box-table : this table tracks values that are the result
;; of evaluating xml boxes.  These values should be rendered as xml boxes,
;; and not as simple lists.

(define finished-xml-box-table (make-weak-hash))

;; syntax->interned-datum : like syntax->datum, except
;; that it re-interns all identifiers.  Useful in checking whether
;; two sexps will have the same printed representation.

(define (syntax->interned-datum stx)
  (syntax-case stx ()
    [(a . rest) (cons (syntax->interned-datum #`a) (syntax->interned-datum #`rest))]
    [id
     (identifier? stx)
     (string->symbol (symbol->string (syntax-e stx)))]
    [else (if (syntax? stx)
              (syntax->datum stx)
              stx)]))


;; the xml-snip-creation@ unit accepts the xml-snip% and scheme-snip% classes and
;; provides functions which map a "spec" to an xml-snip.
;; An xml-spec is (listof xml-spec-elt)
;; An xml-spec-elt is either
;;  - a string,
;;  - (cons/c 'scheme-box scheme-spec), or
;;  - (cons/c 'splice-box scheme-spec)
;;
;; A scheme-spec is (listof scheme-spec-elt)
;; A scheme-spec-elt is either
;;  - a string, or
;;  - (cons ... oh crud.
#;(define xml-snip-creation@
    (unit/sig (create-xml-snip create-scheme-snip create-splice-snip)
      (import (xml-snip% scheme-snip%))
      
      (define (construct-xml-box spec)
        (let* ([new-xml-box (instantiate xml-snip% () 
                              [eliminate-whitespace-in-empty-tags? #t])] ;  need to check what the languages themselves do here
               [xml-editor (send new-xml-box get-editor)])
          (for-each
           (match-lambda
             [`(scheme-box ,@(schemeboxspec ...)) (send new-xml-box insert (construct-scheme-box #f schemeboxspec))]
             [`(splice-box ,@(spliceboxspec ...)) (send new-xml-box insert (construct-scheme-box #f spliceboxspec))]
             [(? string? text) (send xml-editor insert text)])
           spec)
          new-xml-box))
      
      (define (construct-scheme-box splice? spec)
        (let* ([new-scheme-box (instantiate scheme-snip% () [splice? splice?])]
               [scheme-editor (send new-scheme-box get-editor)])
          (for-each 
           (match-lambda
             [`(xml-box ,@(xmlspec ...)) (send scheme-editor insert (construct-xml-box xmlspec))]
             [(? string? text) (send scheme-editor insert text)])
           spec)))))

;; per Robby's suggestion: rather than using a hash table for 
;; lambdas, just use an applicable structure instead.

;; An annotated procedure is represented at runtime by
;; an applicable structure that stores stepper information.
(struct annotated-proc (base info)
  #:property prop:procedure
  (struct-field-index base))


(define-signature view-controller^ (vc-go))
(define-signature stepper-frame^ (stepper-frame%))


(module+ test
  (require rackunit)
  
  (check-equal?
   (syntax->datum
    (skipto/auto (stepper-syntax-property
                  #`(a #,(stepper-syntax-property #`(b c)
                                                  'stepper-skipto
                                                  '(cdr car)))
                  'stepper-skipto
                  '(cdr car))
                 #t
                 (lambda (x) x)))
   'c)

  (define (lifted-name sym) 
    (syntax->datum (get-lifted-var sym)))
  (define cd-stx 
    (datum->syntax #f 'cd))

  (check-equal? (lifted-name (datum->syntax #f 'ab)) 'lifter-ab-0)
  (check-equal? (lifted-name cd-stx) 'lifter-cd-1)
  (check-equal? (lifted-name (datum->syntax #f 'ef)) 'lifter-ef-2)
  (check-equal? (lifted-name cd-stx) 'lifter-cd-1)

  (check-exn exn:fail? (lambda () (stepper-syntax-property #`13 'boozle)))
  (check-exn exn:fail? (lambda () (stepper-syntax-property #`13 'boozle #t)))
  (check-equal? (stepper-syntax-property #`13 'stepper-hint) #f)
  (check-equal? (stepper-syntax-property (stepper-syntax-property #`13 'stepper-hint 'yes)
                                         'stepper-hint) 'yes)
  (check-equal? 
   (stepper-syntax-property (stepper-syntax-property (stepper-syntax-property #`13 
                                                                              'stepper-hint
                                                                              'no)
                                                     'stepper-hint 'yes)
                            'stepper-hint)
   'yes)
  (check-equal? (stepper-syntax-property (stepper-syntax-property (stepper-syntax-property #`13 'stepper-hint 'yes) 'stepper-black-box-expr 'arg) 'stepper-hint) 'yes)
  (check-equal? (syntax->datum (stepper-syntax-property (stepper-syntax-property #`13 'stepper-hint 'yes) 'stepper-black-box-expr 'arg)) 13)
  )
