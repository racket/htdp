(module model mzscheme
  (require (lib "specs.ss" "framework")
           (lib "etc.ss")
           (lib "mred.ss" "mred")  
           (prefix frame: (lib "framework.ss" "framework"))
           "my-macros.ss"
           (prefix a: "annotate.ss")
           (prefix r: "reconstruct.ss")
           "shared.ss"
           "highlight-placeholder.ss")
 
  
  
  (provide go) ; go : (program-expander-type[program-expander] (step-result semaphore -> void)[receive-result] -> void) 

  (define program-expander-contract
    (-> (-> void?) ; init
        (-> string? any? void?) ; error
        (-> (union eof-object? syntax? (cons/p string? any?)) (-> void?) void?) ; iter
        void?))
    
  (define go-contract (-> program-expander-contract ; program-expander
                          (-> step-result? semaphore? void?) ; receive-result
                          void?))
  
  (define (send-to-eventspace eventspace thunk)
    (parameterize ([current-eventspace eventspace])
      (queue-callback thunk)))

  ; go starts a stepper instance
  ; see provide stmt for contract
    (define go 
      (contract
       go-contract
       (lambda (program-expander receive-result)
         (local
             
             ((define finished-exprs null)
              
              (define current-expr #f)
              
              (define packaged-envs a:initial-env-package)
              
              (define drscheme-eventspace (current-eventspace))
              
              (define (send-to-drscheme-eventspace thunk)
                (send-to-eventspace drscheme-eventspace thunk))
              
              (define user-computation-semaphore (make-semaphore))
              
              (define held-expr-list no-sexp)
              (define held-redex-list no-sexp)
              
              (define basic-eval (current-eval))
              
              ; if there's a sexp which _doesn't_ contain a highlight in between two that do, we're in trouble.
              
              (define (redivide exprs)
                (letrec ([contains-highlight-placeholder
                          (lambda (expr)
                            (if (pair? expr)
                                (or (contains-highlight-placeholder (car expr))
                                    (contains-highlight-placeholder (cdr expr)))
                                (eq? expr highlight-placeholder)))])
                  (let loop ([exprs-left exprs] [mode 'before])
                    (cond [(null? exprs-left) 
                           (if (eq? mode 'before)
                               (error 'redivide "no sexp contained the highlight-placeholder.")
                               (values null null null))]
                          [(contains-highlight-placeholder (car exprs-left))
                           (if (eq? mode 'after)
                               (error 'redivide "highlighted sexp when already in after mode")
                               (let-values ([(before during after) (loop (cdr exprs-left) 'during)])
                                 (values before (cons (car exprs-left) during) after)))]
                          [else
                           (case mode
                             ((before) 
                              (let-values ([(before during after) (loop (cdr exprs-left) 'before)])
                                (values (cons (car exprs-left) before) during after)))
                             ((during after) 
                              (let-values ([(before during after) (loop (cdr exprs-left) 'after)])
                                (values before during (cons (car exprs-left) after)))))]))))
              
              ;(redivide `(3 4 (+ (define ,highlight-placeholder) 13) 5 6))
              ;(values `(3 4) `((+ (define ,highlight-placeholder) 13)) `(5 6))
              ;
              ;(redivide `(,highlight-placeholder 5 6))
              ;(values `() `(,highlight-placeholder) `(5 6))
              ;
              ;(redivide `(4 5 ,highlight-placeholder ,highlight-placeholder))
              ;(values `(4 5) `(,highlight-placeholder ,highlight-placeholder) `())
              ;
              ;(printf "will be errors:~n")
              ;(equal? (redivide `(1 2 3 4))
              ;        error-value)
              ;
              ;(equal? (redivide `(1 2 ,highlight-placeholder 3 ,highlight-placeholder 4 5))
              ;        error-value)
              
              
              (define (break mark-set key break-kind returned-value-list)
                (let* ([mark-list (continuation-mark-set->list mark-set key)])
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
                                   (r:reconstruct-current current-expr mark-list break-kind returned-value-list)]
                                  [reconstructed (car reconstruct-pair)]
                                  [redex-list (cadr reconstruct-pair)])
                             (2vals reconstructed redex-list)))])
                    (unless (r:skip-step? break-kind mark-list)
                      (case break-kind
                        [(normal-break)
                         (let*-2vals ([(reconstructed redex-list) (reconstruct-helper)])
                                     (set! held-expr-list reconstructed)
                                     (set! held-redex-list redex-list))]
                        
                        [(result-exp-break result-value-break)
                         (unless (and (eq? break-kind 'result-exp-break) (eq? held-expr-list no-sexp))
                           (let*-2vals ([(reconstructed reduct-list) (reconstruct-helper)])
                                       ;              ; this invariant (contexts should be the same)
                                       ;              ; fails in the presence of unannotated code.  For instance,
                                       ;              ; currently (map my-proc (cons 3 empty)) goes to
                                       ;              ; (... <body-of-my-proc> ...), where the context of the first one is
                                       ;              ; empty and the context of the second one is (... ...).
                                       ;              ; so, I'll just disable this invariant test.
                                       ;              (when (not (equal? reconstructed held-expr-list))
                                       ;                (error 'reconstruct-helper
                                       ;                                  "pre- and post- redex/uct wrappers do not agree:~nbefore: ~a~nafter~a"
                                       ;                                  held-expr-list reconstructed))
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
                                         (send-to-drscheme-eventspace
                                          (lambda ()
                                            (receive-result result user-computation-semaphore)))
                                         (semaphore-wait user-computation-semaphore))))]
                        [(double-break)
                         ; a double-break occurs at the beginning of a let's evaluation.
                         (let* ([reconstruct-quadruple
                                 (r:reconstruct-current current-expr mark-list break-kind returned-value-list)])
                           (when (not (eq? held-expr-list no-sexp))
                             (error 'break-reconstruction
                                    "held-expr-list not empty when a double-break occurred"))
                           (let*-values 
                               ([(new-finished current-pre current-post after) 
                                 (double-redivide finished-exprs 
                                                  (list-ref reconstruct-quadruple 0) 
                                                  (list-ref reconstruct-quadruple 2))])
                             (send-to-drscheme-eventspace
                              (lambda () 
                                (receive-result (make-before-after-result new-finished
                                                                          current-pre
                                                                          (list-ref reconstruct-quadruple 1)
                                                                          current-post
                                                                          (list-ref reconstruct-quadruple 3)
                                                                          after)
                                                user-computation-semaphore)))
                             (semaphore-wait user-computation-semaphore)))]
                        [(late-let-break)
                         (let ([new-finished (car (r:reconstruct-current current-expr mark-list break-kind returned-value-list))])
                           (set! finished-exprs (append finished-exprs new-finished)))]
                        [else (error 'break "unknown label on break")])))))
              
              (define (step-through-expression expanded expand-next-expression)
                (let*-values ([(annotated envs) (a:annotate expanded packaged-envs break 
                                                            'foot-wrap)])
                  (set! packaged-envs envs)
                  (set! current-expr expanded)
                  (let ([expression-result
                         (parameterize ([current-eval basic-eval])
                           (eval annotated))])
                    (add-finished-expr expression-result)
                    (expand-next-expression))))
              
              (define (add-finished-expr expression-result)
                (let ([reconstructed (r:reconstruct-completed current-expr expression-result)])
                  (set! finished-exprs (append finished-exprs (list reconstructed)))))
              
              (define (err-display-handler message exn)
                (send-to-drscheme-eventspace
                 (lambda ()
                   (if (not (eq? held-expr-list no-sexp))
                       (let*-values
                           ([(before current after) (redivide held-expr-list)])
                         (receive-result (make-before-error-result (append finished-exprs before) 
                                                                   current held-redex-list message after)
                                         user-computation-semaphore))
                       (receive-result (make-error-result finished-exprs message) user-computation-semaphore))))))
           
              (program-expander
               void ; init
               err-display-handler ; error
               (lambda (expanded continue-thunk) ; iter
                 (if (eof-object? expanded)
                     (send-to-drscheme-eventspace 
                      (lambda ()
                        (receive-result (make-finished-result finished-exprs) user-computation-semaphore)))
                     (step-through-expression expanded continue-thunk))))))
       `go
       'caller)))

