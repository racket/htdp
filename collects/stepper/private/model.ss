(module model mzscheme
  (require (lib "etc.ss")
           (lib "mred.ss" "mred")  
           (prefix frame: (lib "framework.ss" "framework"))
           "my-macros.ss"
           (prefix a: "annotate.ss")
           (prefix r: "reconstruct.ss")
           "shared.ss"
           "highlight-placeholder.ss")
 
  
  (provide go)
  
  (define (send-to-eventspace eventspace thunk)
    (parameterize ([current-eventspace eventspace])
      (queue-callback thunk)))

  (define (go program-expander receive-result)
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
  
         ; if there's a sexp which _doesn't_ contain a highlight in between two that do, we're in trouble.
  
         (define (redivide exprs)
           (letrec ([contains-highlight-placeholder
                     (lambda (expr)
                       (if (pair? expr)
                           (or (contains-highlight-placeholder (car expr))
                               (contains-highlight-placeholder (cdr expr)))
                           (eq? expr highlight-placeholder)))])
             (let loop ([exprs exprs] [mode 'before])
               (cond [(null? exprs) 
                      (if (eq? mode 'before)
                          (error 'redivide "no sexp contained the highlight-placeholder.")
                          (values null null null))]
                     [(contains-highlight-placeholder (car exprs))
                      (if (eq? mode 'after)
                          (error 'redivide "highlighted sexp when already in after mode")
                          (let-values ([(before during after) (loop (cdr exprs) 'during)])
                            (values before (cons (car exprs) during) after)))]
                     [else
                      (case mode
                        ((before) 
                         (let-values ([(before during after) (loop (cdr exprs) 'before)])
                           (values (cons (car exprs) before) during after)))
                        ((during after) 
                         (let-values ([(before during after) (loop (cdr exprs) 'after)])
                           (values before during (cons (car exprs) after)))))]))))

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
           (fprintf (current-error-port) "entering (break) with break-kind: ~a~n" break-kind)
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
               (case break-kind
                 [(normal-break)
                  (when (not (r:skip-redex-step? mark-list))
                    (let*-2vals ([(reconstructed redex-list) (reconstruct-helper)])
                      (set! held-expr-list reconstructed)
                      (set! held-redex-list redex-list)))
                  (fprintf (current-error-port) "finished with normal-break~n")]
                 
                 [(result-break)
                  (fprintf (current-error-port) "returned-value-list: ~a~nskip-redex-step: ~a~n"
                           returned-value-list
                           (r:skip-redex-step? mark-list))
                  (when (if (not (null? returned-value-list))
                            (not (r:skip-redex-step? mark-list))
                            (and (not (eq? held-expr-list no-sexp))
                                 (not (r:skip-result-step? mark-list))))
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
                        (fprintf (current-error-port) "finished reconstructing result-break~n")
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
                      (fprintf (current-error-port) "finished reconstructing double-break~n")
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
                  (let ([new-finished (r:reconstruct-current current-expr mark-list break-kind returned-value-list)])
                    (set! finished-exprs (append finished-exprs new-finished)))]
                 [else (error 'break "unknown label on break")]))))
         
         (define (step-through-expression expanded expand-next-expression)
           ; is there an eof test?
           ; if so, here's the old thing to to do:
           ; (receive-result (make-finished-result finished-exprs))
           (let*-values ([(annotated envs) (a:annotate expanded packaged-envs break 
                                                       'foot-wrap)])
             (set! packaged-envs envs)
             (set! current-expr expanded)
             (fprintf (current-error-port) "about to perform eval~n")
             (let/ec k
               (let ([expression-result
                      (parameterize ([current-exception-handler (make-exception-handler k)])
                        (eval annotated))])
                 (add-finished-expr expression-result)
                 (send-to-drscheme-eventspace expand-next-expression)))))
         
         (define (add-finished-expr expression-result)
           (let ([reconstructed (r:reconstruct-completed current-expr expression-result)])
             (set! finished-exprs (append finished-exprs (list reconstructed)))))
         
         (define (handle-exception exn)
           (if (not (eq? held-expr-list no-sexp))
               (let*-values
                   ([(before current after) (redivide held-expr-list)])
                 (receive-result (make-before-error-result (append finished-exprs before) 
                                                           current held-redex-list (exn-message exn) after)
                                 user-computation-semaphore))
               (receive-result (make-error-result finished-exprs (exn-message exn)) user-computation-semaphore)))
  
         (define (make-exception-handler k)
           (lambda (exn)
             (send-to-drscheme-eventspace
              (lambda ()
                (handle-exception exn)))
             (k))))
      
      (program-expander
        (lambda (error? expanded send-to-user-eventspace continue-thunk)
          (if error?
              (handle-exception (cadr expanded))
              (begin
                (send-to-user-eventspace
                 (lambda ()
                   (fprintf (current-error-port) "entering user eventspace~n")
                   (queue-callback
                    (lambda ()
                      (fprintf (current-error-port) "callback triggered~n")
                      (step-through-expression expanded continue-thunk))))))))))))    

