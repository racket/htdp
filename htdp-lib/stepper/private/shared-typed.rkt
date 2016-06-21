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
         binding-set-union)

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


