#lang racket/base

;step collector state machine (not yet implemented):
;
; datatype held-type = NO-HELD-STEP | SKIPPED-STEP | HELD(args)
;
; states: global state of held
; global: held : held-type
; edge-names: first, skipped-first, second, skipped-second, double, late-let
;
;transitions (& actions):
;
; held = NO-HELD-STEP :
;  first(x) : held := HELD(x)
;  skipped-first : held := SKIPPED-STEP
;  second(x) : trigger(NO-HELD-STEP, x), held := NO-HELD-STEP.
;      this happens when evaluating unannotated code
;  skipped-second : held := NO-HELD-STEP
;      I believe this can also arise in unannotated code
;  double(x) : double-trigger(x), held := NO-HELD-STEP
;  late-let(x) : late-let-trigger(x), held := NO-HELD-STEP
;
; held = SOME(SKIPPED-STEP) :
;  first(x) : ERROR
;  skipped-first : ERROR
;  second(x) : held := NO-HELD-STEP
;      this happens e.g. for evaluation of top-level var bound to a procedure
;  skipped-second : held := NO-HELD-STEP
;  double(x) : ERROR
;  late-let(x) : ERROR
;
; held = SOME(HELD(args))
;  first(x) : ERROR
;  skipped-first : ERROR
;  second(x) : trigger(HELD(args),x), held = NO-HELD-STEP
;  skipped-second : held = NO-HELD-STEP
;  double(x) : ERROR
;  late-let(x) : ERROR


(require racket/contract
         racket/match
         racket/list
         (prefix-in a: "annotate.rkt")
         (prefix-in r: "reconstruct.rkt")
         "shared.rkt"
         "syntax-property.rkt"
         "marks.rkt"
         "model-settings.rkt"
         "macro-unwind.rkt"
         "lifting.rkt"
         (prefix-in test-engine: test-engine/scheme-tests)
         #;(file "/Users/clements/clements/scheme-scraps/eli-debug.ss")
         ;; for breakpoint display
         ;; (commented out to allow nightly testing)
         #;"display-break-stuff.rkt"
         (for-syntax racket/base))

(provide
 (contract-out
  [go (->*
       (program-expander-contract       ; program-expander
        (-> void?)                      ; dynamic requirer
        (step-result? . -> . void?)     ; receive-result
        (or/c render-settings? false/c)) ; render-settings
       (#:raw-step-receiver
        (-> continuation-mark-set? symbol? void?)
        #:disable-error-handling? boolean?)
       void?)])
 (struct-out posn-info))

(define-logger stepper)

(define program-expander-contract
  ((-> void?) ; init
   ((or/c eof-object? syntax? (cons/c string? any/c)) (-> void?)
                                                      . -> . void?) ; iter
   . -> .
   void?))

(define-struct posn-info (posn span))

; go starts a stepper instance
; see provide stmt for contract
(define (go program-expander dynamic-requirer receive-result render-settings
            #:disable-error-handling? [disable-error-handling? #f]
            #:raw-step-receiver [raw-step-receiver #f])

  ;; finished-exps:
  ;;   (listof (list/c syntax-object? (or/c number? false?)( -> any)))
  ;; because of mutation, these cannot be fixed renderings, but must be
  ;; re-rendered at each step.
  (define finished-exps null)
  (define/contract add-to-finished
    ((-> syntax?) (or/c (listof natural-number/c) false/c) (-> any)
                  . -> . void?)
    (lambda (exp-thunk lifting-indices getter)
      (set! finished-exps
            (append finished-exps
                    (list (list exp-thunk lifting-indices getter))))))

  ;; the "held" variables are used to store the "before" step.
  (define held-exp-list the-no-sexp)

  (define-struct held (exps was-app? source-info))

  (define held-finished-list null)

  (define (reset-held-exp-list)
    (set! held-exp-list the-no-sexp)
    (set! held-finished-list null)
    (set! lhs-recon-thunk null))

  ; used when determining whether to skip step with ellipses on LHS
  ;; ... needs example!
  (define last-rhs-exps null)

  ; thunk that can re-reconstruct the last lhs
  (define lhs-recon-thunk null)

  ; used to resolve lhs ellipses, to make sure highlighting is correct
  ; steps are pushed onto the stack:
  ;   when step=? but not step-and-highlight=?
  ; steps are popped off the stack:
  ;  lhs = ellipses and rhs != last-rhs-exps and skips = 0
  ; skips are defined for each fn in lazy-highlighting.rkt;
  ; # of skips depends on # of hidden !'s in fn def
  (define highlight-stack null)
  (define (highlight-stack-push mark-list)
    (let ([top-called-fn (find-top-called-fn mark-list)])
      (log-stepper-debug
       (format "top called fn = ~a" top-called-fn))
      (set! highlight-stack
            (cons (cons top-called-fn lhs-recon-thunk) highlight-stack))))
  (define (find-top-called-fn mark-list)
    (if (null? mark-list)
        #f
        (let ([top-mark (car mark-list)])
          (if (eq? 'called (mark-label top-mark))
              (object-name (lookup-binding (list top-mark) (get-arg-var 0)))
              (find-top-called-fn (cdr mark-list))))))
  (define (highlight-stack-pop)
    (set! highlight-stack (cdr highlight-stack)))



  ;; highlight-mutated-expressions :
  ;;   ((listof (list/c syntax? syntax?)) (listof (list/c syntax? syntax?))
  ;;   -> (list/c (listof syntax?) (listof syntax?)))
  ;; highlights changes occurring due to mutation.  This function accepts the
  ;; left-hand-side expressions and the right-hand-side expressions, and
  ;; matches them against each other to see which ones have changed due to
  ;; mutation, and highlights these.
  ;; POSSIBLE RESEARCH POINT: if, say, (list 3 4) is mutated to (list 4 5),
  ;;   should the 4 & 5 be highlighted individually or should the list as a
  ;;   whole be highlighted.  Is either one "wrong?"  equivalences between
  ;;   reduction semantics?
  ;;
  ;; 2005-11-14: punting. just highlight the whole darn thing if there are
  ;; any differences.  In fact, just test for eq?-ness.

  #;
  (define (highlight-mutated-expressions lefts rights)
    (if (or (null? lefts) (null? rights))
        (list lefts rights)
        (let ([left-car (car lefts)]
              [right-car (car rights)])
          (if (eq? (syntax-property left-car 'user-source)
                   (syntax-property right-car 'user-source))
              (let ([highlights-added
                     (highlight-mutated-expression left-car right-car)]
                    [rest (highlight-mutated-expressions
                           (cdr lefts) (cdr rights))])
                (cons (cons (car highlights-added) (car rest))
                      (cons (cadr highlights-added) (cadr rest))))))))

  ;; highlight-mutated-expression: syntax? syntax? -> syntax?
  ;; given two expressions, highlight 'em both if they differ at all.

  ;; notes: wanted to use simple "eq?" test... but this will fail when a
  ;; being-stepped definition (e.g.  in a let) turns into a permanent one.
  ;; We pay a terrible price for the lifting thing.  And, for the fact that
  ;; the highlighting follows from the reductions but can't obviously be
  ;; deduced from them.

  #;
  (define (highlight-mutated-expression left right)
    (cond
      ;; if either one is already highlighted, leave them alone.
      [(or (stepper-syntax-property left 'stepper-highlight)
           (stepper-syntax-property right 'stepper-highlight))
       (list left right)]

      ;; first pass: highlight if not eq?.  Should be broken for local-bound
      ;; things as they pass into permanence.
      [(eq? left right)
       (list left right)]

      [else (list (stepper-syntax-property left 'stepper-highlight)
                  (stepper-syntax-property right 'stepper-highlight))]))

  ;; mutated on receipt of a break, used in displaying breakpoint stuff.
  (define steps-received 0)

  (define break
    (lambda (mark-set break-kind returned-value-list)
      (log-stepper-debug
       "---------- BREAK TYPE = ~a ----------" break-kind)

      (set! steps-received (+ steps-received 1))
      ;; have to be careful else this won't be looked up right away:
      ;; (commented out to allow nightly tests to proceed, 2007-09-04
      #;(when (getenv "PLTSTEPPERUNSAFE")
          (let ([steps-received/current steps-received])
            (run-on-drscheme-side
             (lambda ()
               (display-break-stuff
                steps-received/current
                mark-set break-kind returned-value-list)))))

      (define mark-list (and mark-set (extract-mark-list mark-set)))

      (log-stepper-debug "MARKLIST:")
      (when mark-set
        (for ([x (in-list mark-list)])
          (log-stepper-debug (display-mark x))))
      (log-stepper-debug "RETURNED VALUE LIST: ~a" returned-value-list)

      (define (reconstruct-all-completed)
        (filter-map
         (match-lambda
           [(list source-thunk lifting-indices getter)
            (let ([source (source-thunk)])
              (if (r:hide-completed? source)
                  #f
                  (match (r:reconstruct-completed
                          source lifting-indices
                          getter render-settings)
                    [(vector exp #f) (unwind exp render-settings)]
                    [(vector exp #t) exp])))])
         finished-exps))

      (define (compute-posn-info)
        (mark-list->posn-info mark-list))

      (define (compute-step-was-app?)
        (r:step-was-app? mark-list))

      (define (compute-step-kind held-step-was-app?)
        (if (and held-step-was-app?
                 (eq? break-kind 'result-exp-break))
            'user-application
            'normal))

      (define (create-held exps)
        (make-held exps (compute-step-was-app?) (compute-posn-info)))

      ; sends a step to the stepper, except if
      ;  - lhs = rhs
      ;  - lhs = ellipses, highlight-stack = null (ie, this is first step)
      ;  - lhs = ellipses, rhs = last-rhs-exps
      ; when lhs = ellipses, and highlight-stack != null,
      ; pop step from stack and use lhs
      (define/contract
        (send-step lhs-exps lhs-finished-exps
                   rhs-exps rhs-finished-exps
                   step-kind lhs-posn-info rhs-posn-info)
        (-> (listof syntax?)
            (listof syntax?)
            (listof syntax?)
            (listof syntax?)
            any/c any/c any/c
            any)

        (log-stepper-debug "maybe sending step ... \n")
        (log-stepper-debug "LHS = ~a\n" (map syntax->hilite-datum lhs-exps))
        (log-stepper-debug "RHS = ~a\n" (map syntax->hilite-datum rhs-exps))

        (define send?
          (cond
            ; SKIPPING step, lhs = rhs
            ; if highlights differ, push highlight-stack and set last-rhs-exp
            [(step=? lhs-exps rhs-exps)
             (log-stepper-debug "SKIPPING STEP (LHS = RHS)\n")
             (when (not (step-and-highlight=? lhs-exps rhs-exps))
               (log-stepper-debug "Pushing onto highlight-stack:\n  ~a thunk\n"
                                  (syntax->hilite-datum (car lhs-exps)))
               (highlight-stack-push mark-list)
               (set! last-rhs-exps rhs-exps))
             #f]
            [(step=? lhs-exps (list #'(... ...)))
             (cond
               ; SKIPPING step, lhs = ellipses and rhs = last-rhs-exps
               [(step=? rhs-exps last-rhs-exps)
                (log-stepper-debug "SKIPPING STEP (LHS = ellipses and RHS = last RHS)\n")
                #f]
               ; SKIPPING step, lhs = ellipses and highlight-stack = null and
               ; last-rhs = null
               ; if last-rhs != null, send step (lhs = ...)
               [(null? highlight-stack)
                (if (not (null? last-rhs-exps))
                    #t
                    (begin
                      (log-stepper-debug "SKIPPING STEP (LHS = ellipses and highlight-stack = null)\n")
                      #f))]
               ; if top-called-fn = top of highlight stack,
               ;    then send step with lhs = lhs-thunk on highlight stack
               ; else if last-rhs != null, send it, else skip
               [else
                (let ([top-called-fn (caar highlight-stack)]
                      [lhs-thunk (cdar highlight-stack)])
                  (if (and (eq? (find-top-called-fn mark-list) top-called-fn)
                           (eq? break-kind 'result-value-break))
                      (begin
                        (set! lhs-exps (lhs-thunk))
                        (set! lhs-finished-exps rhs-finished-exps)
                        (log-stepper-debug "Popping highlight-stack\n")
                        (highlight-stack-pop)
                        #t)
                      (not (null? last-rhs-exps))))])]
            ; sending step
            [else #t]))

        (when send?
          (receive-result
           (before-after-result
            (append lhs-finished-exps lhs-exps)
            (append rhs-finished-exps rhs-exps)
            step-kind
            lhs-posn-info rhs-posn-info))
          (log-stepper-debug "step sent:\n")
          (log-stepper-debug "LHS = ~a\n" (map syntax->hilite-datum lhs-exps))
          (log-stepper-debug "RHS = ~a\n" (map syntax->hilite-datum rhs-exps))
          (set! last-rhs-exps rhs-exps)))

      ; compares the lhs and rhs of a step (lists of syntaxes)
      ; and returns true if the underlying datums are equal
      (define (step=? lhs rhs)
        (equal? (map syntax->datum lhs)
                (map syntax->datum rhs)))
      (define (step-and-highlight=? lhs rhs)
        (equal? (map syntax->hilite-datum lhs)
                (map syntax->hilite-datum rhs)))

      (if (r:skip-step? break-kind mark-list render-settings)
          (begin
            (log-stepper-debug "skipped step\n")
            (when (or (eq? break-kind 'normal-break)
                      ;; not sure about this...
                      (eq? break-kind 'nomal-break/values))
              (set! held-exp-list the-skipped-step)))

          (begin
            (case break-kind
              ; CASE: normal-break or normal-break/values -------------------
              [(normal-break normal-break/values)
               (when (and (eq? break-kind 'normal-break)
                          returned-value-list)
                 (error 'break
                        "broken invariant: normal-break can't have returned values"))
               (define lhs-reconstructed
                 (r:reconstruct-left-side
                  mark-list returned-value-list render-settings))
               (log-stepper-debug "LHS (pre-unwound):\n  ~a\n"
                                  (syntax->hilite-datum lhs-reconstructed))
               (define lhs-unwound
                 (map (λ (exp) (unwind exp render-settings))
                      (maybe-lift lhs-reconstructed #f)))
               (for ([x (in-list lhs-unwound)])
                 (log-stepper-debug "LHS (unwound): ~a\n"
                                    (syntax->hilite-datum x)))
               (define lhs-finished-exps (reconstruct-all-completed))
               (set! held-finished-list lhs-finished-exps)
               (set! held-exp-list (create-held lhs-unwound))
               (set! lhs-recon-thunk
                     (λ ()
                       (log-stepper-debug "forcing saved MARKLIST")
                       (for ([x (in-list mark-list)])
                         (log-stepper-debug "~a" (display-mark x)))
                       (log-stepper-debug "saved RETURNED VALUE LIST: ~a"
                                          returned-value-list)
                       (map (λ (exp) (unwind exp render-settings))
                            (maybe-lift
                             (r:reconstruct-left-side
                              mark-list
                              returned-value-list
                              render-settings)
                             #f))))]

              ; CASE: result-exp-break or result-value-break ----------------
              [(result-exp-break result-value-break)
               (define (reconstruct)
                 (define rhs-reconstructed
                   (r:reconstruct-right-side
                    mark-list returned-value-list render-settings))
                 (log-stepper-debug "RHS (pre-unwound):\n  ~a"
                                    (syntax->hilite-datum
                                     rhs-reconstructed))
                 (define rhs-unwound
                   (map (λ (exp) (unwind exp render-settings))
                        (maybe-lift rhs-reconstructed #f)))
                 (for ([x (in-list rhs-unwound)])
                   (log-stepper-debug "RHS (unwound): ~a"
                                      (syntax->hilite-datum x)))
                 rhs-unwound)
               (match held-exp-list
                 [(struct skipped-step ())
                  (log-stepper-debug "LHS = skipped, so skipping RHS")
                  ;; don't render if before step was a skipped-step
                  (reset-held-exp-list)]
                 [(struct no-sexp ())
                  (log-stepper-debug "LHS = none")
                  ;; in this case, there was no "before" step, due
                  ;; to unannotated code.  In this case, we make the
                  ;; optimistic guess that none of the finished
                  ;; expressions were mutated.  It would be somewhat
                  ;; painful to do a better job, and the stepper
                  ;; makes no guarantees in this case.
                  (send-step (list #'(... ...)) '()                    ; lhs
                             (reconstruct) (reconstruct-all-completed) ; rhs
                             'normal #f #f)
                  (reset-held-exp-list)]
                 [(struct held (held-exps held-step-was-app? held-posn-info))
                  (send-step held-exps held-finished-list
                             (reconstruct) (reconstruct-all-completed)
                             (compute-step-kind held-step-was-app?)
                             held-posn-info (compute-posn-info))
                  (reset-held-exp-list)])]

              ; CASE: double-break ------------------------------------------
              [(double-break)
               ;; a double-break occurs at the beginning of a let's
               ;; evaluation.
               (when (not (eq? held-exp-list the-no-sexp))
                 (error
                  'break-reconstruction
                  "held-exp-list not empty when a double-break occurred"))
               (define new-finished-list (reconstruct-all-completed))
               (define reconstruct-result
                 (r:reconstruct-double-break mark-list render-settings))
               (log-stepper-debug "LHS (pre-unwound):\n  ~a"
                                  (syntax->hilite-datum (car reconstruct-result)))
               (log-stepper-debug "RHS (pre-unwound):\n  ~a"
                                  (syntax->hilite-datum (cadr reconstruct-result)))
               (define lhs-unwound
                 (map (lambda (exp) (unwind exp render-settings))
                      (maybe-lift (car reconstruct-result) #f)))
               (define rhs-unwound
                 (map (lambda (exp) (unwind exp render-settings))
                      (maybe-lift (cadr reconstruct-result) #t)))
               (for ([x (in-list lhs-unwound)])
                 (log-stepper-debug "LHS (unwound):\n  ~a"
                                    (syntax->hilite-datum x)))
               (for ([x (in-list rhs-unwound)])
                 (log-stepper-debug "right side (unwound):\n  ~a"
                                    (syntax->hilite-datum x)))
               (send-step lhs-unwound new-finished-list
                          rhs-unwound new-finished-list
                          'normal
                          (compute-posn-info) (compute-posn-info))]

              ; CASE: expr-finished-break -----------------------------------
              [(expr-finished-break)
               (unless (not mark-list)
                 (error 'break
                        "expected no mark-list with expr-finished-break"))
               ;; in an expr-finished-break, the returned-vals hold (listof
               ;; (list/c source lifting-index getter)) this will now include
               ;; define-struct breaks, for which the source is the source
               ;; and the getter causes an error.
               (for ([x (in-list returned-value-list)])
                 (log-stepper-debug "add to finished:")
                 (log-stepper-debug "  source: ~a" (syntax->hilite-datum ((car x))))
                 (log-stepper-debug "  index: ~a" (second x))
                 (log-stepper-debug
                  "  getter: ~a"
                  (if (stepper-syntax-property ((car x))
                                               'stepper-black-box-expr)
                      "no getter for term with stepper-black-box-expr property"
                      ((third x)))))
               (for-each (lambda (source/index/getter)
                           (apply add-to-finished source/index/getter))
                         returned-value-list)]

              [else (error 'break "unknown label on break")])))))

  (define maybe-lift
    (if (render-settings-lifting? render-settings)
        lift
        ;; ... oh dear; model.rkt should disable the double-break & late-let
        ;; break when lifting is off.
        (lambda (stx dont-care) (list stx))))

  (define show-lambdas-as-lambdas?
      (render-settings-show-lambdas-as-lambdas? render-settings))

  ;; step through a single expanded expression.
  (define (step-through-expression expanded)
    (define annotated (a:annotate expanded break show-lambdas-as-lambdas?))
    (parameterize (;; I think this parameterization is pointless in the #lang world
                   [test-engine:test-silence #t])
      (eval-syntax annotated)))

  (define (err-display-handler message exn)
    (match held-exp-list
      [(struct no-sexp ())
        (receive-result (error-result message))]
      [(struct held (exps dc posn-info))
       (begin
         (receive-result
          (before-error-result (append held-finished-list exps)
                                    message
                                    posn-info))
         (set! held-exp-list the-no-sexp))]))

  (program-expander
   (lambda () ; init
     (unless disable-error-handling?
       (error-display-handler err-display-handler)))
   (lambda (expanded continue-thunk) ; iter
     (log-stepper-debug "model received expanded syntax object: ~a"
                        (and (syntax? expanded)
                             (syntax->datum expanded)))
     (if (eof-object? expanded)
         (begin
           (dynamic-requirer)
           (receive-result (finished-stepping)))
         (begin (r:reset-lazy-tables)
                (step-through-expression expanded)
                (continue-thunk))))))


; no-sexp is used to indicate no sexpression for display.
; e.g., on an error message, there's no sexp.
(define-struct no-sexp ())
(define the-no-sexp (make-no-sexp))

; skipped-step is used to indicate that the "before" step was skipped.
(define-struct skipped-step ())
(define the-skipped-step (make-skipped-step))

;; produce a posn-info structure or false based on the information in a mark-list
;; mark-list->posn-info : (listof mark) -> (or/c posn-info? false?)
(define (mark-list->posn-info mark-list)
  (let* ([first-mark-source (mark-source (car mark-list))]
         [posn (syntax-position first-mark-source)]
         [span (syntax-span first-mark-source)])
    (if posn
        (make-posn-info posn span)
        #f)))
