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
;  second(x) : trigger(NO-HELD-STEP, x), held := NO-HELD-STEP ; this happens when evaluating unannotated code
;  skipped-second : held := NO-HELD-STEP ; I believe this can also arise in unannotated code
;  double(x) : double-trigger(x), held := NO-HELD-STEP
;  late-let(x) : late-let-trigger(x), held := NO-HELD-STEP
;
; held = SOME(SKIPPED-STEP) :
;  first(x) : ERROR
;  skipped-first : ERROR
;  second(x) : held := NO-HELD-STEP ; this happens e.g. for evaluation of top-level var bound to a procedure
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


(module model mzscheme
  (require (lib "contract.ss")
           (lib "etc.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred")
           "my-macros.ss"
           (prefix a: "annotate.ss")
           (prefix r: "reconstruct.ss")
           "shared.ss"
           "marks.ss"
           "highlight-placeholder.ss"
           "model-settings.ss")
 

  (define program-expander-contract
    (-> (-> void?) ; init
        (-> (union eof-object? syntax? (cons/p string? any?)) (-> void?) void?) ; iter
        void?))
    

  (provide/contract [go (-> program-expander-contract       ; program-expander
                            (-> step-result? void?)         ; receive-result
                            (union render-settings? false?) ; render-settings
                            void?)])
  
  (define (send-to-eventspace eventspace thunk)
    (parameterize ([current-eventspace eventspace])
      (queue-callback thunk)))

  ; go starts a stepper instance
  ; see provide stmt for contract 
  (define (go program-expander receive-result render-settings)
    (local
        
        ((define finished-exprs null)
         
         (define held-expr-list no-sexp)
         (define held-redex-list no-sexp)
         
         (define basic-eval (current-eval))
         
         ; redivide takes a list of sexps and divides them into the 'before', 'during', and 'after' lists,
         ; where the before and after sets are maximal-length lists where none of the s-expressions contain
         ; a highlight-placeholder
         ; (->* ((listof sexp)) (list/p sexp sexp sexp))
         (define (redivide exprs)
           (letrec ([contains-highlight-placeholder
                     (lambda (expr)
                       (if (pair? expr)
                           (or (contains-highlight-placeholder (car expr))
                               (contains-highlight-placeholder (cdr expr)))
                           (eq? expr highlight-placeholder)))])
             (let* ([list-length (length exprs)]
                    [split-point-a (- list-length (length (or (memf contains-highlight-placeholder exprs) null)))]
                    [split-point-b (length (or (memf contains-highlight-placeholder (reverse exprs)) null))])
               (if (<= split-point-b split-point-a)
                   (error 'redivide-exprs "s-expressions did not contain the highlight-placeholder: ~v" exprs)
                   (values (sublist 0 split-point-a exprs) ; before
                           (sublist split-point-a split-point-b exprs) ; during
                           (sublist split-point-b list-length exprs)))))) ; after

         
;         (redivide `(3 4 (+ (define ,highlight-placeholder) 13) 5 6))
;         (values `(3 4) `((+ (define ,highlight-placeholder) 13)) `(5 6))
;         
;         (redivide `(,highlight-placeholder 5 6))
;         (values `() `(,highlight-placeholder) `(5 6))
;         
;         (redivide `(4 5 ,highlight-placeholder ,highlight-placeholder))
;         (values `(4 5) `(,highlight-placeholder ,highlight-placeholder) `())
;         
;         (printf "will be errors:~n")
;         (equal? (redivide `(1 2 3 4))
;                 error-value)
;         
;         (redivide `(1 2 ,highlight-placeholder 3 ,highlight-placeholder 4 5))
;         (values `(1 2) `(,highlight-placeholder 3 ,highlight-placeholder) `(4 5))
         
         
         (define break 
           (opt-lambda (mark-set break-kind [returned-value-list null])
             (let* ([mark-list (extract-mark-list mark-set)])
               (let ([double-redivide
                      (lambda (finished-exprs new-exprs-before new-exprs-after)
                        (let*-values ([(before current after) (redivide new-exprs-before)]
                                      [(before-2 current-2 after-2) (redivide new-exprs-after)]
                                      [(_) (unless (and (equal? before before-2)
                                                        (equal? after after-2))
                                             (error 'break "reconstructed before or after defs are not equal."))])
                          (values (append finished-exprs before) current current-2 after)))]
                     [reconstruct-helper
                      (lambda ()
                        (let* ([reconstruct-pair
                                (r:reconstruct-current mark-list break-kind returned-value-list render-settings)]
                               [reconstructed (car reconstruct-pair)]
                               [redex-list (cadr reconstruct-pair)])
                          (2vals reconstructed redex-list)))])
                 (if (r:skip-step? break-kind mark-list render-settings)
                     (when (eq? break-kind 'normal-break)
                       (set! held-expr-list skipped-step))
                     (case break-kind
                       [(normal-break)
                        (let*-2vals ([(reconstructed redex-list) (reconstruct-helper)])
                          (set! held-expr-list reconstructed)
                          (set! held-redex-list redex-list))]
                       
                       [(result-exp-break result-value-break)
                        (if (eq? held-expr-list skipped-step)
                            (begin 
                              (set! held-expr-list no-sexp)
                              (set! held-redex-list no-sexp))
                            (let*-2vals ([(reconstructed reduct-list) (reconstruct-helper)])
                              ; this invariant (contexts should be the same)
                              ; fails in the presence of unannotated code.  For instance,
                              ; currently (map my-proc (cons 3 empty)) goes to
                              ; (... <body-of-my-proc> ...), where the context of the first one is
                              ; empty and the context of the second one is (... ...).
                              ; so, I'll just disable this invariant test.
                              ;
                              ; in fact, this also fails for let/let*/etc., where a single expression
                              ; reduces to multiple top-level-expressions.
                              ;
                              ;(when (not (equal? reconstructed held-expr-list))
                              ;  (error 'reconstruct-helper
                              ;         "pre- and post- redex/uct wrappers do not agree:~nbefore: ~a~nafter~a"
                              ;         held-expr-list reconstructed))
                              (let ([result
                                     (if (not (eq? held-expr-list no-sexp))
                                         (let*-values 
                                             ([(new-finished current-pre current-post after) 
                                               (double-redivide finished-exprs held-expr-list reconstructed)])
                                           (make-before-after-result new-finished current-pre held-redex-list current-post reduct-list after))
                                         (let*-values
                                             ([(before current after) (redivide reconstructed)])
                                           (make-before-after-result (append finished-exprs before) `(,highlight-placeholder) `(...)
                                                                     current reduct-list after)))])
                                (set! held-expr-list no-sexp)
                                (set! held-redex-list no-sexp)
                                (receive-result result))))]
                       [(double-break)
                        ; a double-break occurs at the beginning of a let's evaluation.
                        (let* ([reconstruct-quadruple
                                (r:reconstruct-current mark-list break-kind returned-value-list render-settings)])
                          (when (not (eq? held-expr-list no-sexp))
                            (error 'break-reconstruction
                                   "held-expr-list not empty when a double-break occurred"))
                          (let*-values 
                              ([(new-finished current-pre current-post after) 
                                (double-redivide finished-exprs 
                                                 (list-ref reconstruct-quadruple 0) 
                                                 (list-ref reconstruct-quadruple 2))])
                            (receive-result (make-before-after-result new-finished
                                                                      current-pre
                                                                      (list-ref reconstruct-quadruple 1)
                                                                      current-post
                                                                      (list-ref reconstruct-quadruple 3)
                                                                      after))))]
                       [(late-let-break)
                        (let ([new-finished (car (r:reconstruct-current mark-list break-kind returned-value-list render-settings))])
                          (set! finished-exprs (append finished-exprs new-finished)))]
                       
                       [(expr-finished-break)
                        (let ([reconstructed (r:reconstruct-completed mark-list returned-value-list render-settings)])
                          (set! finished-exprs (append finished-exprs (list reconstructed))))]
                       
                       [else (error 'break "unknown label on break")]))))))
         
         (define (step-through-expression expanded expand-next-expression)
           (let* ([annotated (a:annotate expanded break 'foot-wrap)])
             (parameterize ([current-eval basic-eval])
               (eval annotated))
             (expand-next-expression)))
         
         (define (err-display-handler message exn)
           (if (not (eq? held-expr-list no-sexp))
                  (let*-values
                      ([(before current after) (redivide held-expr-list)])
                    (receive-result (make-before-error-result (append finished-exprs before) 
                                                              current held-redex-list message after)))
                  (receive-result (make-error-result finished-exprs message)))))
      
      (program-expander
       (lambda () 
         ; swap these to allow errors to escape (e.g., when debugging)
         ;(error-display-handler err-display-handler)
         (void)
         )
       (lambda (expanded continue-thunk) ; iter
         (if (eof-object? expanded)
             (begin
               (receive-result (make-finished-result finished-exprs))
               (receive-result (make-finished-stepping)))
             (step-through-expression expanded continue-thunk)))))))

