(unit/sig stepper:reconstruct^
  (import [z : zodiac:system^]
          mzlib:function^
          [e : zodiac:interface^]
          [utils : stepper:cogen-utils^]
          [b : plt:basis^]
          stepper:marks^
          [s : stepper:model^]
	  stepper:shared^)

  (define the-undefined-value (letrec ([x x]) x))
  
  (define nothing-so-far (gensym "nothing-so-far-"))
  
  (define memoized-read->raw
    (let ([table (make-hash-table-weak)])
      (lambda (read)
        (or (hash-table-get table read (lambda () #f))
            (let ([raw (z:sexp->raw read)])
              (hash-table-put! table read raw)
              raw)))))
  
  (define (make-apply-pred-to-raw pred)
    (lambda (expr)
      (pred (memoized-read->raw (expr-read expr)))))
             
  (define (make-check-raw-first-symbol symbol)
    (make-apply-pred-to-raw
     (lambda (raw)
       (and (pair? raw)
            (eq? (car raw) symbol)))))

  (define comes-from-define?
    (make-check-raw-first-symbol 'define))

  (define comes-from-define-procedure?
    (make-apply-pred-to-raw
     (lambda (raw) (and (pair? raw)
                        (eq? (car raw) 'define)
                        (pair? (cadr raw))))))
  
  (define comes-from-lambda-defined-procedure?
    (make-apply-pred-to-raw
     (lambda (raw) (and (pair? raw)
                        (eq? (car raw) 'define)
                        (pair? (caddr raw))
                        (eq? (caaddr raw) 'lambda)))))
  
  (define comes-from-define-struct?
    (make-check-raw-first-symbol 'define-struct))
  
  (define comes-from-cond?
    (make-check-raw-first-symbol 'cond))
  
  (define comes-from-lambda?
    (make-check-raw-first-symbol 'lambda))
  
  (define comes-from-case-lambda?
    (make-check-raw-first-symbol 'case-lambda))

  (define comes-from-and?
    (make-check-raw-first-symbol 'and))
  
  (define comes-from-or?
    (make-check-raw-first-symbol 'or))

  (define (rectify-value val)
    (let ([closure-record (closure-table-lookup val (lambda () #f))])
      (cond
        [closure-record
         (or (closure-record-name closure-record)
             (let ([mark (closure-record-mark closure-record)])
               (o-form-case-lambda->lambda 
                (rectify-source-expr (mark-source mark) (list mark) null))))]
        [else
         (s:print-convert val)])))
  
  (define (o-form-case-lambda->lambda o-form)
    (cond [(eq? (car o-form) 'lambda)
           o-form]
          [else ; o-form = case-lambda
           (let ([args (caadr o-form)]
                 [body-exps (cdr (cadr o-form))])
             `(lambda ,args ,@body-exps))]))
  
  (define (o-form-lambda->define o-form name)
    (let ([args (cadr o-form)]
          [body-exps (cddr o-form)])
      `(define (,name ,@args) ,@body-exps)))
  
  (define (final-mark-list? mark-list)
    (and (not (null? mark-list)) (eq? (mark-label (car mark-list)) 'final)))
 
  (define continuation? 
    (let ([r (regexp "#<continuation>")])
      (lambda (k)
        (let ([p (open-output-string)])
          (display k p)
          (not (not (regexp-match r (get-output-string p))))))))
  
  (define (skip-result-step? mark-list)
    (in-inserted-else-clause mark-list))
  
  (define (skip-redex-step? mark-list)
    (and (pair? mark-list)
         (let ([expr (mark-source (car mark-list))])
           (or (and (z:varref? expr)
                    (or (z:lambda-varref? expr)
                        (let ([var (z:varref-var expr)])
                          (with-handlers 
                              ([exn:variable? (lambda args #f)])
                            (or (and (s:check-pre-defined-var var)
                                     (or (procedure? (s:global-lookup var))
                                         (eq? var 'empty)))
                                (let ([val (if (z:top-level-varref? expr)
                                               (s:global-lookup var)
                                               (lookup-binding mark-list (z:bound-varref-binding expr)))])
                                  (and (procedure? val)
                                       (not (continuation? val))
                                       (eq? var
                                            (closure-record-name 
                                             (closure-table-lookup val (lambda () #f)))))))))))
               (and (z:app? expr)
                    (let ([fun-val (mark-binding-value
                                    (lookup-binding mark-list (get-arg-binding 0)))])
                      (and (procedure? fun-val)
                           (procedure-arity-includes? 
                            fun-val
                            (length (z:app-args expr)))
                           (or (and (s:constructor-style-printing?)
                                    (if (s:abbreviate-cons-as-list?)
                                        (eq? fun-val list) ; that needs exporting too.
                                        (and (s:user-cons? fun-val)
                                             (second-arg-is-list? mark-list))))
                               (s:user-vector? fun-val)
                               (and (eq? fun-val void)
                                    (eq? (z:app-args expr) null))
                               (struct-constructor-procedure? fun-val)
                               ; this next clause may be obviated by the previous one.
                               (let ([closure-record (closure-table-lookup fun-val (lambda () #f))])
                                 (and closure-record
                                      (closure-record-constructor? closure-record)))))))
               (in-inserted-else-clause mark-list)))))
  
  (define (second-arg-is-list? mark-list)
    (let ([arg-val (mark-binding-value (lookup-binding mark-list (get-arg-binding 2)))])
      (list? arg-val)))  
  
  (define (in-inserted-else-clause mark-list)
    (and (not (null? mark-list))
         (let ([expr (mark-source (car mark-list))])
           (or (and (z:zodiac? expr)
                    (not (z:if-form? expr))
                    (comes-from-cond? expr))
               (in-inserted-else-clause (cdr mark-list))))))
  
   ; static-binding-indexer (z:parsed -> integer)
  
  (define static-binding-indexer
    (let* ([name-number-table (make-hash-table)]
           [binding-number-table (make-hash-table-weak)])
      (lambda (binding)
        (cond [(hash-table-get binding-number-table binding (lambda () #f)) =>
               (lambda (x) x)]
              [else (let* ([orig-name (z:binding-orig-name binding)]
                           [old-index (hash-table-get name-number-table orig-name (lambda () -1))]
                           [new-index (+ old-index 1)])
                      (hash-table-put! name-number-table orig-name new-index)
                      (hash-table-put! binding-number-table binding new-index)
                      new-index)]))))
  
  ; construct-lifted-name (z:parsed num -> string)
  
  (define (construct-lifted-name binding dynamic-index)
    (let* ([static-index (static-binding-indexer binding)])
      (string->symbol
       (string-append (symbol->string (z:binding-orig-name binding)) "0" 
                      (number->string static-index) "0" 
                      (number->string dynamic-index)))))
  
  ; binding-lifted-name ((listof mark) z:binding -> num)
  
  (define (binding-lifted-name mark-list binding)
      (construct-lifted-name binding (mark-binding-value (lookup-binding mark-list (get-lifted-gensym binding)))))
  
  
  ; rectify-source-expr (z:parsed (ListOf Mark) (ListOf z:binding) -> sexp)
  
  (define (rectify-source-expr expr mark-list lexically-bound-bindings)
    (let ([recur (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-bindings))]
          [let-recur (lambda (expr bindings) (rectify-source-expr expr mark-list (append bindings lexically-bound-bindings)))])
      (cond [(z:varref? expr)
             (cond [(z:bound-varref? expr)
                    (let ([binding (z:bound-varref-binding expr)])
                      (if (memq binding lexically-bound-bindings)
                          (z:binding-orig-name binding)
                          (if (z:lambda-binding? binding)
                              (rectify-value (mark-binding-value (lookup-binding mark-list binding)))
                              (binding-lifted-name mark-list binding))))]
                   [(z:top-level-varref? expr)
                    (z:varref-var expr)])]
            
            [(z:app? expr)
             (map recur (cons (z:app-fun expr) (z:app-args expr)))]
            
            [(z:struct-form? expr)
             (if (comes-from-define-struct? expr)
                 (e:internal-error expr "this expression should have been skipped during reconstruction")
                 (let ([super-expr (z:struct-form-super expr)]
                       [raw-type (utils:read->raw (z:struct-form-type expr))]
                       [raw-fields (map utils:read->raw (z:struct-form-fields expr))])
                   (if super-expr
                       `(struct (,raw-type ,(recur super-expr))
                                ,raw-fields)
                       `(struct ,raw-type ,raw-fields))))]
            
            [(z:if-form? expr)
             (cond
               [(comes-from-cond? expr)
                `(cond ,@(rectify-cond-clauses (z:zodiac-start expr) expr mark-list lexically-bound-bindings))]
               [(comes-from-and? expr)
                `(and ,@(rectify-and-clauses (z:zodiac-start expr) expr mark-list lexically-bound-bindings))]
               [(comes-from-or? expr)
                `(or ,@(rectify-or-clauses (z:zodiac-start expr) expr mark-list lexically-bound-bindings))]
               [else
                `(if ,(recur (z:if-form-test expr))
                     ,(recur (z:if-form-then expr))
                     ,(recur (z:if-form-else expr)))])]
            
            [(z:quote-form? expr)
             (let ([raw (utils:read->raw (z:quote-form-expr expr))])
               (rectify-value raw)
;               (cond [(or (string? raw)
;                          (number? raw)
;                          (boolean? raw)
;                          (s:image? raw))
;                      raw]
;                     [else
;                      `(quote ,raw)])
               )]

            [(z:let-values-form? expr)
             (let* ([bindings (z:let-values-form-vars expr)]
                    [binding-list (apply append bindings)]
                    [binding-names (map (lambda (b-list) (map z:binding-orig-name b-list)) bindings)]
                    [right-sides (map recur (z:let-values-form-vals expr))]
                    [must-be-values? (ormap (lambda (n-list) (not (= (length n-list) 1))) binding-names)]
                    [_ (printf "about to first-time rectify body~n")]
                    [rectified-body (let-recur (z:let-values-form-body expr) binding-list)]
                    [_2 (printf "finished first-time rectify body~n")])
               (if must-be-values?
                   `(let-values ,(map list binding-names right-sides) ,rectified-body)
                   `(let ,(map list (map car binding-names) right-sides) ,rectified-body)))]
            
            [(z:letrec-values-form? expr)
             (let* ([bindings (z:letrec-values-form-vars expr)]
                    [binding-list (apply append bindings)]
                    [binding-names (map (lambda (b-list) (map z:binding-orig-name b-list)) bindings)]
                    [right-sides (map (lambda (expr) (let-recur expr binding-list))
                                      (z:letrec-values-form-vals expr))]
                    [must-be-values? (ormap (lambda (n-list) (not (= (length n-list) 1))) binding-names)]
                    [rectified-body (let-recur (z:letrec-values-form-body expr) binding-list)])
               (if must-be-values?
                   `(letrec-values ,(map list binding-names right-sides) ,rectified-body)
                   `(letrec ,(map list (map car binding-names) right-sides) ,rectified-body)))]
                    
            [(z:case-lambda-form? expr)
             (let* ([arglists (z:case-lambda-form-args expr)]
                    [bodies (z:case-lambda-form-bodies expr)]
                    [o-form-arglists
                     (map (lambda (arglist) 
                            (utils:improper-map z:binding-orig-name
                                              (utils:arglist->ilist arglist)))
                          arglists)]
                    [binding-form-arglists (map z:arglist-vars arglists)]
                    [o-form-bodies 
                     (map (lambda (body binding-form-arglist) (let-recur body binding-form-arglist))
                          bodies
                          binding-form-arglists)])
               (cond [(or (comes-from-lambda? expr) (comes-from-define? expr))
                      `(lambda ,(car o-form-arglists) ,(car o-form-bodies))]
                     [(comes-from-case-lambda? expr)
                      `(case-lambda ,@(map list o-form-arglists o-form-bodies))]
                     [else
                      (e:internal-error expr "unknown source for case-lambda")]))]
            
            ; we won't call rectify-source-expr on define-values expressions
            
            [else
             (print-struct #t)
             (e:internal-error
              expr
              (format "stepper:rectify-source: unknown object to rectify, ~a~n" expr))])))
 
  ; these macro unwinders (and, or) are specific to beginner level
  
  (define (rectify-and-clauses and-source expr mark-list lexically-bound-bindings)
    (let ([rectify-source (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-bindings))])
      (if (and (z:if-form? expr) (equal? and-source (z:zodiac-start expr)))
          (cons (rectify-source (z:if-form-test expr))
                (rectify-and-clauses and-source (z:if-form-then expr) mark-list lexically-bound-bindings))
          null)))
  
  (define (rectify-or-clauses or-source expr mark-list lexically-bound-bindings)
    (let ([rectify-source (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-bindings))])
      (if (and (z:if-form? expr) (equal? or-source (z:zodiac-start expr)))
          (cons (rectify-source (z:if-form-test expr))
                (rectify-or-clauses or-source (z:if-form-else expr) mark-list lexically-bound-bindings))
          null)))
  
  (define (rectify-cond-clauses cond-source expr mark-list lexically-bound-bindings)
    (let ([rectify-source (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-bindings))])
      (if (equal? cond-source (z:zodiac-start expr))
          (if (z:if-form? expr)
              (cons (list (rectify-source (z:if-form-test expr))
                          (rectify-source (z:if-form-then expr)))
                    (rectify-cond-clauses cond-source (z:if-form-else expr) mark-list lexically-bound-bindings))
              null)
          `((else ,(rectify-source expr))))))

  ; reconstruct-completed : reconstructs a completed expression or definition.  This now
  ; relies upon the s:global-lookup procedure to find values in the user-namespace.
  ; I'm not yet sure whether or not 'vars' must be supplied or whether they can be derived
  ; from the expression itself.
  
  (define (reconstruct-completed expr value)    
      (cond [(z:define-values-form? expr)
             (if (comes-from-define-struct? expr)
                 (utils:read->raw (expr-read expr))
                 (let* ([vars (map z:varref-var (z:define-values-form-vars expr))]
                        [values (map s:global-lookup vars)]
                        [rectified-vars (map rectify-value values)])
                   (cond [(comes-from-define-procedure? expr)
                          (let* ([mark (closure-record-mark  (closure-table-lookup (car values)))]
                                 [rectified (rectify-source-expr (mark-source mark) (list mark) null)])
                            (o-form-lambda->define (o-form-case-lambda->lambda rectified)
                                                   (car vars)))]
                         [(comes-from-lambda-defined-procedure? expr)
                          (let* ([mark (closure-record-mark (closure-table-lookup (car values)))]
                                 [rectified (rectify-source-expr (mark-source mark) (list mark) null)])
                            `(define ,(car vars) ,(o-form-case-lambda->lambda rectified)))]
                         [(comes-from-define? expr)
                          `(define ,(car vars) ,(car rectified-vars))]
                         [else
                          `(define-values ,vars
                             ,(if (= (length values) 1)
                                  (car rectified-vars)
                                  `(values ,@rectified-vars)))])))]
            [(z:begin-form? expr) ; hack for xml stuff
             (utils:read->raw (expr-read expr))]
            [else
             (rectify-value value)]))
  
  
  ; reconstruct-lifted ((listof symbol) sexp -> sexp)
  ; reconstruct-lifted really should take into account the original source expression. Local may
  ; screw me up. We'll cross that bridge when we get to it.
  
  (define (reconstruct-lifted names sexp)
    (case (length names)
      ((0) `(define-values () ,sexp))
      ((1) `(define ,(car names) ,sexp))
      (else `(define-values ,names ,sexp))))
  
  (define (so-far-only so-far) (values so-far null null))
    
  ; reconstruct-current : takes a parsed expression, a list of marks, the kind of break, and
  ; any values that may have been returned at the break point. It produces a list containing the
  ; reconstructed sexp, and the (contained) sexp which is the redex.  If the redex is a heap value
  ; (and can thus be distinguished from syntactically identical occurrences of that value using
  ; eq?), it is embedded directly in the sexp. Otherwise, its place in the sexp is taken by the 
  ; highlight-placeholder, which is replaced by the highlighted redex in the construction of the 
  ; text%
  
  ; z:parsed (list-of mark) symbol (list-of value) -> 
  ; (list sexp sexp)

  (define (reconstruct-current expr mark-list break-kind returned-value-list)
    
    (local
        ((define (rectify-source-top-marks expr)
           (rectify-source-expr expr mark-list null))
         
         (define (rectify-top-level expr so-far)
           (if (z:define-values-form? expr)
               (let ([vars (z:define-values-form-vars expr)]
                     [val (z:define-values-form-val expr)])
                 (cond [(comes-from-define-struct? expr)
                        (let* ([struct-expr val]
                               [super-expr (z:struct-form-super struct-expr)]
                               [raw-type (utils:read->raw (z:struct-form-type struct-expr))]
                               [raw-fields (map utils:read->raw (z:struct-form-fields struct-expr))])
                          `(define-struct
                            ,(if super-expr
                                 (list raw-type so-far)
                                 raw-type)
                            ,raw-fields))]
                       [(or (comes-from-define-procedure? expr)
                            (and (comes-from-define? expr)
                                 (pair? so-far)
                                 (eq? (car so-far) 'lambda)))
                        (let* ([proc-name (z:varref-var
                                           (car (z:define-values-form-vars expr)))]
                               [o-form-proc so-far])
                          (o-form-lambda->define o-form-proc proc-name))]
                                              
                       [(comes-from-define? expr)
                        `(define 
                           ,(z:varref-var (car vars))
                           ,so-far)]
                       
                       [else
                        `(define-values 
                           ,(map utils:read->raw vars)
                           ,(rectify-source-top-marks val))]))
               so-far))
         
         ; rectify-inner ((listof mark) sexp -> (listof sexp) (listof sexp) sexp)
         
         (define (rectify-inner mark-list so-far)
           (let* ([rectify-source-current-marks 
                   (lambda (expr)
                     (rectify-source-expr expr mark-list null))]
                  [rectify-let 
                   (lambda (letrec? binding-sets vals body)
                     (let+ ([val binding-list (apply append binding-sets)]
                            [val binding-names (map (lambda (set) (map z:binding-orig-name set)) binding-sets)]
                            [val dummy-var-list (if letrec?
                                                    binding-list
                                                    (build-list (length binding-list) get-arg-binding))]
                            [_ (printf "about to look up rhs-vals~n")]
                            [val rhs-vals (map (lambda (arg-binding) 
                                                 (mark-binding-value (lookup-binding mark-list arg-binding)))
                                               dummy-var-list)]
                            [_ (printf "finished looking up rhs-vals~n")]
                            [val rhs-lifted-name-sets
                                 (map (lambda (binding-set)
                                        (map (lambda (binding)
                                               (binding-lifted-name mark-list binding))
                                             binding-set))
                                      binding-sets)]
                            [_ (printf "finished looking up dynamic indices~n")]
                            [val (values before-defs after-defs)
                                 (let loop ([rhs-vals rhs-vals]
                                            [rhs-sources vals]
                                            [rhs-lifted-name-sets rhs-lifted-name-sets])
                                   (cond [(null? binding-sets) (values null null)]
                                         [(eq? (car rhs-vals) (if letrec?
                                                                  the-undefined-value
                                                                  *unevaluated*))
                                          (values null
                                                  (cons (reconstruct-lifted (car rhs-lifted-name-sets) so-far)
                                                        (map (lambda (rhs-lifted-name-set rhs-source)
                                                               (reconstruct-lifted rhs-lifted-name-set 
                                                                                   (rectify-source-expr expr
                                                                                                        mark-list
                                                                                                        (if letrec?
                                                                                                            binding-list
                                                                                                            null))))
                                                             (cdr rhs-lifted-name-sets)
                                                             (cdr rhs-sources))))]
                                         [else
                                          (let*-values ([(first-set) (car rhs-lifted-name-sets)]
                                                        [(set-vals remaining) (list-partition rhs-vals (length first-set))]
                                                        [(reconstructed) (map reconstruct-lifted
                                                                              rhs-lifted-name-sets
                                                                              (map rectify-value set-vals))]
                                                        [(before after) (loop remaining
                                                                              (cdr rhs-sources)
                                                                              (cdr rhs-lifted-name-sets))])
                                            (values (append reconstructed before)
                                                    after))]))]
                            [_ (printf "about to rectify body~n")]
                            [val rectified-body (rectify-source-expr body mark-list binding-list)]
                            [_ (printf "finished rectifying body~n")])
                       (values before-defs after-defs rectified-body)))]
                  [top-mark (car mark-list)]
                  [expr (mark-source top-mark)])
             (cond 
               ; variable references
               [(z:varref? expr)
               (so-far-only
                (if (eq? so-far nothing-so-far)
                    (rectify-source-current-marks expr)
                    (e:internal-error expr 
                                      "variable reference given as context")))]
               
               ; applications
               
               [(z:app? expr)
                (so-far-only
                 (let* ([sub-exprs (cons (z:app-fun expr) (z:app-args expr))]
                        [arg-temps (build-list (length sub-exprs) get-arg-binding)]
                        [arg-vals (map (lambda (arg-temp) 
                                         (mark-binding-value (lookup-binding mark-list arg-temp)))
                                       arg-temps)])
                   (case (mark-label (car mark-list))
                     ((not-yet-called)
                      (letrec
                          ([split-lists
                            (lambda (exprs vals)
                              (if (or (null? vals)
                                      (eq? (car vals) *unevaluated*))
                                  (values null exprs)
                                  (let-values ([(small-vals small-exprs)
                                                (split-lists (cdr exprs) (cdr vals))])
                                    (values (cons (car vals) small-vals) small-exprs))))])
                        (let-values ([(evaluated unevaluated) (split-lists sub-exprs arg-vals)])
                          (let* ([rectified-evaluated (map rectify-value evaluated)])
                            (if (null? unevaluated)
                                rectified-evaluated
                                (append rectified-evaluated
                                        (cons so-far
                                              (map rectify-source-current-marks (cdr unevaluated)))))))))
                     ((called)
                      (if (eq? so-far nothing-so-far)
                          `(...) ; in unannotated code
                          `(... ,so-far ...)))
                     (else
                      (e:static-error "bad label in application mark: ~s" expr)))))]
               
               ; define-struct 
               
               [(z:struct-form? expr)
                (so-far-only
                 (if (comes-from-define-struct? expr)
                     so-far
                     (let ([super-expr (z:struct-form-super expr)]
                           [raw-type (utils:read->raw (z:struct-form-type expr))]
                           [raw-fields (map utils:read->raw (z:struct-form-fields expr))])
                       (if super-expr
                           `(struct (,raw-type ,so-far)
                                    ,raw-fields)
                           `(struct ,raw-type ,raw-fields)))))]
               
               ; if
               
               [(z:if-form? expr)
                (so-far-only
                 (let ([test-exp (if (eq? so-far nothing-so-far)
                                     (rectify-value (mark-binding-value (lookup-binding mark-list if-temp)))
                                     so-far)])
                   (cond [(comes-from-cond? expr)
                          (let* ([clause (list test-exp (rectify-source-current-marks (z:if-form-then expr)))]
                                 [cond-source (z:zodiac-start expr)]
                                 [rest-clauses (rectify-cond-clauses cond-source (z:if-form-else expr) mark-list null)])
                            `(cond ,clause ,@rest-clauses))]
                         [(comes-from-and? expr)
                          `(and ,test-exp ,@(rectify-and-clauses (z:zodiac-start expr)
                                                                 (z:if-form-then expr)
                                                                 mark-list
                                                                 null))]
                         [(comes-from-or? expr)
                          `(or ,test-exp ,@(rectify-or-clauses (z:zodiac-start expr)
                                                               (z:if-form-else expr)
                                                               mark-list
                                                               null))]
                         [else
                          `(if ,test-exp 
                               ,(rectify-source-current-marks (z:if-form-then expr))
                               ,(rectify-source-current-marks (z:if-form-else expr)))])))]
               
               ; quote : there is no mark or break on a quote.
               
               ; begin, begin0 : may not occur directly (or indirectly?) except in advanced
               
               ; let-values
               
               [(z:let-values-form? expr)
                (rectify-let #f
                             (z:let-values-form-vars expr)
                             (z:let-values-form-vals expr)
                             (z:let-values-form-body expr))]
               
               [(z:letrec-values-form? expr)
                (rectify-let #t
                             (z:letrec-values-form-vars expr)
                             (z:letrec-values-form-vals expr)
                             (z:letrec-values-form-body expr))]
               
               ; define-values : define's don't get marks, so they can't occur here
               
               ; lambda : there is no mark or break on a quote
               
               [else
                (print-struct #t)
                (e:internal-error
                 expr
                 (format "stepper:reconstruct: unknown object to reconstruct, ~a~n" expr))])))
         
         
         (define redex #f)
         
         (define (current-def-rectifier defs so-far mark-list first)
           (if (null? mark-list)
               (append defs
                       (list (rectify-top-level expr so-far)))
               (let-values ([(before after reconstructed) (rectify-inner mark-list so-far)])
                 (current-def-rectifier
                  (append before defs after)
                  (if first
                      (begin
                        (set! redex reconstructed)
                        highlight-placeholder)
                      reconstructed)
                  (cdr mark-list)
                  #f))))
         
         (define (rectify-let-values-step)
           (let*-values ([(redex) (rectify-source-expr (mark-source (car mark-list)) mark-list null)]
                         [(before-step) (current-def-rectifier null highlight-placeholder (cdr mark-list) #f)]
                         [(r-before r-after reduct) (rectify-inner mark-list #f)]
                         [(new-defs (append r-before r-after))]
                         [(after-step) (current-def-rectifier (build-list (length new-defs) 
                                                                          (lambda (x) highlight-placeholder))
                                                              highlight-placeholder
                                                              (cdr mark-list) 
                                                              #f)])
             (list before-step (list redex)
                   after-step (append new-defs (list reduct)))))
           
           
         ;         (define (confusable-value? val)
         ;           (not (or (number? val)
         ;                    (boolean? val)
         ;                    (string? val)
         ;                    (symbol? val))))
         
         (define answer
           (case break-kind
             ((result-break)
              (let* ([innermost (if (null? returned-value-list)
                                    (rectify-source-expr (mark-source (car mark-list)) mark-list null)
                                    (rectify-value (car returned-value-list)))]
                     [current-def (current-def-rectifier null highlight-placeholder (cdr mark-list) #f)])
                (list current-def (list innermost))))
             ((normal-break)
              (let ([current-def (current-def-rectifier null nothing-so-far mark-list #t)])
                  (list current-def (list redex))))
             ((double-break)
              (rectify-let-values-step))
             ((late-let-break)
              (let ([reconstructed (rectify-inner mark-list #f)])
                (let loop ([first (car reconstructed)] [rest (cdr reconstructed)]) ; take all but last
                  (if (null? rest) null (cons first (loop (car rest) (cdr rest)))))))
             (else
              (e:internal-error 'reconstruct-current-def "unknown break kind: " break-kind))))

         )
      
      answer)))
