(module model mzscheme
  (require (lib "etc.ss")
           (lib "mred.ss" "mred")
           (prefix i: "model-input.ss")
           (prefix a: "annotate.ss")
           (prefix r: "reconstruct.ss")
           "shared.ss")
 
  
  (provide
   ; model interface
   go)
  
;  (import [i : stepper:model-input^]
;          mred^
;          [z : zodiac:system^]
;          [d : drscheme:export^]
;          [p : mzlib:print-convert^]
;          [e : zodiac:interface^]
;          [a : stepper:annotate^]
;          [r : stepper:reconstruct^]
;          stepper:shared^)
  
  ; my-assq is like assq but it only returns the second element of the list. This
  ; is the way it should be, I think.
  (define (my-assq assoc-list value)
    (cond [(null? assoc-list) #f]
          [(eq? (caar assoc-list) value) (cadar assoc-list)]
          [else (my-assq (cdr assoc-list) value)]))
  
  (define continue-user-computation #f)
  
  (define (go)
    (local
  
        ((define finished-exprs null)
  
         (define current-expr #f)

         (define packaged-envs a:initial-env-package)
  
         (define first-time #t)
         
         (interface:expand-program
          (lambda (expanded)
            (when first-time?
              (set! first-time #f)
              (model-settings:gather-eventspace-info))
            ; is there an eof test?
            ; if so, here's the old thing to to do:
            ; (i:receive-result (make-finished-result finished-exprs))
            (let*-values ([(annotated envs) (a:annotate expanded packaged-envs break 
                                                                    'foot-wrap)]
                     (set! packaged-envs envs)
                     (set! current-expr expanded)
                     (check-for-repeated-names expanded exception-handler)
                     (let ([expression-result
                            (parameterize ([current-exception-handler exception-handler])
                              (user-primitive-eval annotated))])
                       (send-to-drscheme-eventspace
                        (lambda ()
                          (add-finished-expr expression-result)
                          (read-next-expr)))))
  
  (define (read-next-expr)
    (send-to-user-eventspace
     (lambda ()
       (let/ec k
         (let ([exception-handler (make-exception-handler k)])
           (d:interface:set-zodiac-phase 'reader)
           (let* ([new-expr (with-handlers
                                ((exn:read? exception-handler))
                              (reader))])
             (if (z:eof? new-expr)
                 (begin
                   (send-to-drscheme-eventspace
                    (lambda ()
                      ))
                   'finished)
                 (let* ([new-parsed (if (z:eof? new-expr)
                                        #f
                                        (begin
                                          (d:interface:set-zodiac-phase 'expander)
                                          (with-handlers
                                              ((exn:syntax? exception-handler))
                                            (z:scheme-expand new-expr 'previous user-vocabulary))))])
                   ))))))))
  
  (define (check-for-repeated-names expr exn-handler)
    (with-handlers
        ([exn:user? exn-handler]
         [exn:syntax? exn-handler])
      (when (z:define-values-form? expr)
        (for-each (lambda (name) 
                    (when (check-global-defined name)
                      (e:static-error "define" 'kwd:define expr
                                      "name is already bound: ~s" name)))
                  (map z:varref-var (z:define-values-form-vars expr))))))
  
  (define (add-finished-expr expression-result)
    (let ([reconstructed (r:reconstruct-completed current-expr expression-result)])
      (set! finished-exprs (append finished-exprs (list reconstructed)))))
  
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
    (let ([mark-list (continuation-mark-set->list mark-set key)])
      (let ([double-redivide
             (lambda (finished-exprs new-exprs-before new-exprs-after)
               (let*-values ([(before current after) (redivide new-exprs-before)]
                             [(before-2 current-2 after-2) (redivide new-exprs-after)]
                             [(_) (unless (and (equal? before before-2)
                                               (equal? after after-2))
                                    (error 'break "reconstructed before or after defs are not equal."))])
                 (values (append finished-exprs before) current current-2 after)))]
            [reconstruct-helper
             (lambda (finish-thunk)
               (let* ([reconstruct-pair
                       (r:reconstruct-current current-expr mark-list break-kind returned-value-list)]
                      [reconstructed (car reconstruct-pair)]
                      [redex-list (cadr reconstruct-pair)])
                 (finish-thunk reconstructed redex-list)))])
        (case break-kind
          [(normal-break)
           (when (not (r:skip-redex-step? mark-list))
             (reconstruct-helper 
              (lambda (reconstructed redex-list)
                (set! held-expr-list reconstructed)
                (set! held-redex-list redex-list))))]
          [(result-break)
           (when (if (not (null? returned-value-list))
                     (not (r:skip-redex-step? mark-list))
                     (and (not (eq? held-expr-list no-sexp))
                          (not (r:skip-result-step? mark-list))))
             (reconstruct-helper 
              (lambda (reconstructed reduct-list)
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
                  (let/ec k
                    (set! continue-user-computation k)
                    (i:receive-result result))))))]
          [(double-break)
           ; a double-break occurs at the beginning of a let's evaluation.
           (send-to-drscheme-eventspace
            (lambda ()
              (let* ([reconstruct-quadruple
                      (r:reconstruct-current current-expr mark-list break-kind returned-value-list)])
                (when (not (eq? held-expr-list no-sexp))
                  (error 'break-reconstruction
                         "held-expr-list not empty when a double-break occurred"))
                (let*-values 
                    ([(new-finished current-pre current-post after) (double-redivide finished-exprs 
                                                                                     (list-ref reconstruct-quadruple 0) 
                                                                                     (list-ref reconstruct-quadruple 2))])
                  (i:receive-result (make-before-after-result new-finished
                                                              current-pre
                                                              (list-ref reconstruct-quadruple 1)
                                                              current-post
                                                              (list-ref reconstruct-quadruple 3)
                                                              after))))))
           (suspend-user-computation)]
          [(late-let-break)
           (send-to-drscheme-eventspace
            (lambda ()
              (let ([new-finished (r:reconstruct-current current-expr mark-list break-kind returned-value-list)])
                (set! finished-exprs (append finished-exprs new-finished))
                (continue-user-computation))))
           (suspend-user-computation)]
          [else (error 'break "unknown label on break")]))))
  
  (define (handle-exception exn)
    (if (not (eq? held-expr-list no-sexp))
        (let*-values
            ([(before current after) (redivide held-expr-list)])
          (i:receive-result (make-before-error-result (append finished-exprs before) 
                                                      current held-redex-list (exn-message exn) after)))
        (begin
          (i:receive-result (make-error-result finished-exprs (exn-message exn))))))
  
  (define (make-exception-handler k)
    (lambda (exn)
      (send-to-drscheme-eventspace
       (lambda ()
         (handle-exception exn)))
      (k)))

    (binding-index-reset)
    
    
    
    ; result of invoking stepper-instance : (->)
    continue-user-computation))
