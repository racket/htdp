#lang typed/racket/base

;; the parts of shared.rkt that can easily
;; be represented using typed racket

(provide Varref-Set
         Binding-Set
         Arglist
         step-result?
         (struct-out Before-After-Result)
         (struct-out Before-Error-Result)
         (struct-out Error-Result)
         (struct-out Runaway-Process)
         (struct-out Posn-Info)
         (struct-out Closure-Record)
         varref-set-union
         binding-set-union
         binding-set-varref-set-intersect
         varref-set-remove-bindings
         arglist-flatten
         get-arg-var
         begin0-temp
         get-lifted-var)

(require/typed "syntax-hider.rkt"
               [#:opaque SStx sstx?]
               [#:opaque SMrk smrk?])

(define-type Varref-Set (Listof Identifier)) ;; should be set?
(define-type Binding-Set (U Varref-Set 'all))
(define-type Arglist
  (U '()
     Identifier
     (Pairof Identifier Arglist)
     (Syntaxof '())
     (Syntaxof (Pairof Identifier Arglist))))

;; represents the result of a step, traveling from the model
;; to the view-controller
(define-type Step-Result
  (U Before-After-Result
     Before-Error-Result
     Error-Result
     'finished-stepping))

(define-type Step-Maybe-Result
  (U Step-Result
     Runaway-Process))

;; dropping types on structure elements to allow
;; compilation to convert these into flat contracts.
;; hoping to re-add types after converting more to TR...
(struct Before-After-Result
  ([pre-exps : (Listof SStx)]
   [post-exps : (Listof SStx)]
   ;; wait... no one looks at the kind? can be removed?
   [kind : Step-Kind]
   [pre-src : (U False Posn-Info)]
   [post-src : (U False Posn-Info)]) #:transparent)
(struct Before-Error-Result
  ([pre-exps : (Listof SStx)]
   [err-msg : String]
   [pre-src : (U False Posn-Info)]) #:transparent)
(struct Error-Result ([err-msg : String]) #:transparent)
(struct Runaway-Process ([sema : Semaphore]) #:transparent)

;; represents a contiguous region of a file. These numbers
;; come from syntax-position and syntax-span
(struct Posn-Info ([posn : (U False Natural)]
                   [span : (U False Natural)]) #:transparent)


(define-type Step-Kind
  (U 'normal
     'user-application))

(define-predicate step-result? Step-Result)

; the closure record is placed in the closure table
(struct Closure-Record ([name : (U False
                                   SStx)]
                        ;; FIXME narrow type later
                        [mark : SMrk]
                        [lifted-index : (U False SStx)])
  #:transparent)

; BINDING-/VARREF-SET FUNCTIONS

; note: a BINDING-SET which is not 'all may be used as a VARREF-SET.
; this is because they both consist of syntax objects, and a binding
; answers true to bound-identifier=? with itself, just like a varref
; in the scope of that binding would.


;; combine a list of binding sets
(: binding-set-union ((Listof Binding-Set) -> Binding-Set))
(define (binding-set-union args)
  (foldl binding-set-pair-union null args))

;; combine a list of varref sets
(: varref-set-union ((Listof Varref-Set) -> Varref-Set))
(define (varref-set-union args)
  (foldl varref-set-pair-union null args))

;; the union of two varref-sets
(: varref-set-pair-union (Varref-Set Varref-Set -> Varref-Set))
(define (varref-set-pair-union a-set b-set)
  (set-pair-union a-set b-set free-identifier=?))

;; the union of two binding sets
(: binding-set-pair-union (Binding-Set Binding-Set -> Binding-Set))
(define (binding-set-pair-union a-set b-set)
  (cond [(eq? a-set 'all) 'all]
        [(eq? b-set 'all) 'all]
        [else (set-pair-union a-set b-set eq?)]))

;; the union of two lists using a specified equality function
(: set-pair-union (All (T)
                       ((Listof T) (Listof T) (T T -> Boolean)
                                   -> (Listof T))))
(define (set-pair-union a-set b-set comparator)
  (cond [(null? b-set) a-set]
        [(null? a-set) b-set]
        [else (append (remove* a-set b-set comparator) a-set)]))

; return the subset of varrefs that appear in the bindings
(: binding-set-varref-set-intersect
   (Binding-Set Varref-Set -> Binding-Set))
(define (binding-set-varref-set-intersect bindings varrefs)
  (cond [(eq? bindings 'all) varrefs]
        [else (filter (lambda ([varref : Identifier])
                        (ormap (lambda ([binding : Identifier])
                                 (bound-identifier=? binding varref))
                               bindings))
                      varrefs)]))

; varref-set-remove-bindings : VARREF-SET (BINDING-SET - 'all) -> VARREF-SET
; remove bindings from varrefs
(: varref-set-remove-bindings (Varref-Set Binding-Set -> Varref-Set))
(define (varref-set-remove-bindings varrefs bindings)
  (cond [(eq? bindings 'all)
         (error 'varref-set-remove-bindings
                "binding-set 'all passed as second argument, first argument was: ~s"
                varrefs)]
        [else (remove* bindings varrefs bound-identifier=?)]))

;; arglist : for our puposes, an ilist is defined like this:
;; arglist : (or/c identifier? null? (cons identifier? arglist?) (syntax (cons identifier? arglist?))
;; ... where an ilist val can be anything _except_ a pair or null

;; arglist-flatten : produces a list containing the elements of the ilist

(: arglist-flatten (Arglist -> (Listof Identifier)))
(define (arglist-flatten arglist)
  (let loop ([ilist : Arglist arglist])
    (cond [(identifier? ilist)
           (cons ilist null)]
          [(or (null? ilist) (syntax-null? ilist))
           null]
          [(pair? ilist)
           (cons (car ilist) (loop (cdr ilist)))]
          [(and (syntax? ilist)
                (pair? (syntax-e ilist)))
           (loop (syntax-e ilist))])))

(define-predicate syntax-null? (Syntaxof Null))


; bogus-binding is used so that we can create legal bindings for temporary variables
(: create-bogus-binding (String -> Identifier))
(define (create-bogus-binding name)
  (let* ([gensymed-name (gensym name)]
         [binding (datum->syntax #'here gensymed-name)])
    binding))


; make-binding-source creates a pool of bindings, indexed by arbitrary keys. These bindings
; not eq? to any other bindings[*], but a client can always get the same binding by
; invoking the resulting procedure with the same key (numbers work well). make-binding-source
; also takes a string which will be part of the printed representation of the binding's
; name; this makes debugging easier.
; [*] actually, this is not true if you don't use a one-to-one function as the binding-maker
; make-gensym-source : (string -> (key -> binding))
(: make-binding-source (All (K) (String (String -> Identifier) (K -> String)
                                        -> (K -> Identifier))))
(define (make-binding-source id-string binding-maker key-displayer)
  (let ([assoc-table ((inst make-weak-hash K Identifier))])
    (lambda ([key : K])
      (let ([maybe-fetch (hash-ref assoc-table key (lambda () #f))])
        (or maybe-fetch
            (begin
              (let* ([new-binding (binding-maker 
                                   (string-append id-string
                                                  (key-displayer key)
                                                  "-"))])
                (hash-set! assoc-table key new-binding)
                new-binding)))))))

; gensyms needed by many modules:

(define begin0-temp (create-bogus-binding "begin0-temp"))

; get-arg-var maintains a list of bindings associated with the non-negative
; integers.  These symbols are used in the elaboration of applications; the nth
; in the application is evaluated and stored in a variable whose name is the nth
; gensym supplied by get-arg-var.

(define get-arg-var
  (make-binding-source "arg" create-bogus-binding number->string))


(: lifted-index Natural)
(define lifted-index 0)

(: next-lifted-symbol (String -> Identifier))
(define (next-lifted-symbol str)
  (let ([index lifted-index]) 
    (set! lifted-index (+ lifted-index 1))
    (datum->syntax #'here (string->symbol (string-append str (number->string index))))))


; get-lifted-var maintains the mapping between let-bindings and the syntax object
; which is used to capture its index at runtime.
; unfortunately, it can't use "make-binding-source" because you need to compare the items 
; with module-variable=?, which means that hash tables won't work.
(: get-lifted-var (Identifier -> Identifier))
(define get-lifted-var
  (let ()
    (: assoc-table (Weak-Assoc Identifier Identifier))
    (define assoc-table (box null))
    (lambda ([stx : Identifier])
      (let ([maybe-fetch (weak-assoc-search assoc-table stx free-identifier=?)])
        (or maybe-fetch
            (begin
              (let* ([new-binding (next-lifted-symbol
                                   (string-append "lifter-"
                                                  (format "~a" (syntax->datum stx))
                                                  "-"))])
                (weak-assoc-add! assoc-table stx new-binding)
                new-binding)))))))

; my weak-assoc lists are lists of two-element lists, where the first one is in a weak box.
; furthermore, the whole thing is in a box, to allow it to be banged when needed.

(define-type (Weak-Assoc T U)
  (Boxof (Listof (List (Weak-Boxof T) U))))

(: weak-assoc-add! (All (T U) ((Weak-Assoc T U) T U -> Void)))
(define (weak-assoc-add! boxed-lst key value)
  (set-box! boxed-lst (cons (list (make-weak-box key) value)
                            (unbox boxed-lst))))

(: weak-assoc-search (All (K V)
                          ((Weak-Assoc K V) K (K K -> Boolean)
                                            -> (U False V))))
(define (weak-assoc-search boxed-lst key eq-fun)
  (define lst (unbox boxed-lst))
  (: found-val (U False V))
  (define found-val #f)
  (define stripped
    (let loop : (Listof (List (Weak-Boxof K) V))
      ([remaining lst])
      (if (null? remaining)
          null
          (let* ([first (car remaining)]
                 [first-key (weak-box-value (car first))])
            (if first-key
                (if (eq-fun key first-key)
                    (begin 
                      (set! found-val (cadr first))
                      remaining)
                    (cons first
                          (loop (cdr remaining))))
                (loop (cdr remaining)))))))
  (set-box! boxed-lst stripped)
  found-val)


(module+ test
  (require typed/rackunit)
  
  
  (check-equal? (map (inst syntax-e Symbol) (arglist-flatten #'(a b c))) '(a b c))
  (check-equal? (map (inst syntax-e Symbol) (arglist-flatten #'(a . (b c)))) '(a b c))
  (check-equal? (map (inst syntax-e Symbol) (arglist-flatten #'(a b . c))) '(a b c))
  (check-equal? (map (inst syntax-e Symbol) (arglist-flatten #'a)) '(a))


  (test-true
   "get-arg-binding"
   (let* ([arg3 (get-arg-var 3)]
          [arg2 (get-arg-var 2)]
          [arg1 (get-arg-var 1)]
          [arg2p (get-arg-var 2)])
     (and (not (eq? arg3 arg2))
          (not (eq? arg3 arg1))
          (not (eq? arg3 arg2p))
          (not (eq? arg2 arg1))
          (eq? arg2 arg2p)
          (not (eq? arg1 arg2p)))))

  
  (struct test () #:transparent)
  (: test-= ((U test False Real) (U test False Real) -> Boolean))
  (define (test-= a b)
    (cond [(and (real? a) (real? b)) (= a b)]
          [(and (test? a) (test? b)) #t]
          [else #f]))
  (test-case
   "weak-assoc"
   
   (: wa (Weak-Assoc (U Real False test) Real))
   (define wa (box null))
   (weak-assoc-add! wa 3 4)
   (weak-assoc-add! wa 9 10)
   (check-eq? (weak-assoc-search wa 3 test-=) 4)
   (check-eq? (weak-assoc-search wa 9 test-=) 10)
   (check-eq? (weak-assoc-search wa 3 test-=) 4)
   (check-eq? (length (unbox wa)) 2)
   (: my-struct (U False test))
   (define my-struct (test))
   (weak-assoc-add! wa my-struct 14)
   (check-eq? (length (unbox wa)) 3)
   (check-eq? (weak-assoc-search wa my-struct eq?) 14)
   (set! my-struct #f)
   (collect-garbage)
   (check-eq? (length (unbox wa)) 3)
   (check-eq? (weak-assoc-search wa 3 test-=) 4)
   (check-eq? (length (unbox wa)) 2)))