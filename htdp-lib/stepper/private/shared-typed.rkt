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
         (struct-out Closure-Record))

(require/typed "syntax-hider.rkt"
               [#:opaque SStx sstx?])

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
                        [mark : Any]
                        [lifted-index : (U False SStx)])
  #:transparent)