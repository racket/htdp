(unit/sig stepper:reconstruct^
  (import [z : zodiac:system^]
          mzlib:function^
	  [e : stepper:error^]
          [p : mzlib:print-convert^]
          [b : userspace:basis^]
          [s : stepper:settings^]
	  stepper:shared^)

  (define nothing-so-far (gensym "nothing-so-far-"))
  
  (define (mark-source mark)
    (car (mark)))
  
  (define (mark-bindings mark)
    (cddr (mark)))
  
  (define (mark-label mark)
    (cadr (mark)))
  
  (define (mark-binding-value mark-binding)
    ((car mark-binding)))
  
  (define (mark-binding-varref mark-binding)
    (cadr mark-binding))
  
  (define (find-var-binding mark-list var)
    (if (null? mark-list)
        ; must be a primitive
        (error 'find-var-binding "variable not found in environment: ~a" var)
	; (error var "no binding found for variable.")
	(let* ([bindings (mark-bindings (car mark-list))]
	       [matches (filter (lambda (mark-var)
				  (eq? var (z:varref-var (mark-binding-varref mark-var))))
                                bindings)])
	  (cond [(null? matches)
		 (find-var-binding (cdr mark-list) var)]
		[(> (length matches) 1)
		 (error 'find-var-binding "more than one variable binding found for var: ~a" var)]
		[else ; (length matches) = 1
		 (car matches)]))))

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
         (parameterize
             ([p:constructor-style-printing (s:get-constructor-style-printing)]
              [p:abbreviate-cons-as-list (s:get-abbreviate-cons-as-list)]
              [p:empty-list-name (s:get-empty-list-name)]
              [p:show-sharing (s:get-show-sharing)]
              [p:current-print-convert-hook 
               (lambda (v basic-convert sub-convert)
                 (if (s:image? v)
                     v
                     (basic-convert v)))]
              [current-namespace (s:get-namespace)])
           (p:print-convert val))])))
  
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
  
  (define (stop-here? mark-list)
    (not (and (pair? mark-list)
              (let ([expr (mark-source (car mark-list))])
                (or (and (z:varref? expr)
                         (or (z:bound-varref? expr)
                             (let ([var (z:varref-var expr)])
                               (or (memq var (s:get-global-defined-vars))
                                   (call-with-current-continuation
                                    (lambda (k)
                                      (with-handlers ([exn:variable?
                                                       (lambda (exn) (k #f))])
                                        (let ([val (mark-binding-value
                                                    (find-var-binding mark-list (z:varref-var expr)))])
                                          (and (procedure? val)
                                               (not (continuation? val))
                                               (eq? var
                                                    (closure-record-name 
                                                     (closure-table-lookup val))))))))))))
                    (and (z:app? expr)
                         (let ([fun-val (mark-binding-value
                                         (find-var-binding mark-list 
                                                           (z:varref-var (get-arg-symbol 0))))])
                           (and (procedure-arity-includes? 
                                 fun-val
                                 (length (z:app-args expr)))
                                (or (and (s:get-constructor-style-printing)
                                         (if (s:get-abbreviate-cons-as-list)
                                             (eq? fun-val list)
                                             (eq? fun-val (s:get-cons))))
                                    (eq? fun-val (s:get-vector))
                                    (and (eq? fun-val void)
                                         (eq? (z:app-args expr) null))
                                    (struct-constructor-procedure? fun-val)
                                    ; this next clause may be obviated by the previous one.
                                    (let ([closure-record (closure-table-lookup fun-val (lambda () #f))])
                                      (and closure-record
                                           (closure-record-constructor? closure-record)))))))
                    (in-inserted-else-clause mark-list))))))
  
  (define (in-inserted-else-clause mark-list)
    (cond [(null? mark-list) #f]
          [(let ([expr (mark-source (car mark-list))])
             (and (z:zodiac? expr)
                  (not (z:if-form? expr))
                  (comes-from-cond? expr)))
           #t]
          [else (in-inserted-else-clause (cdr mark-list))]))
    
  (define (rectify-source-expr expr mark-list lexically-bound-vars)
    (let ([recur (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-vars))])
      (cond [(z:varref? expr)
             (cond [(memq (z:varref-var expr) lexically-bound-vars)
                    (z:binding-orig-name (z:bound-varref-binding expr))]
                   [(z:top-level-varref? expr)
                    (string->uninterned-symbol (symbol->string (z:varref-var expr)))]
                   [else
                    (rectify-value (mark-binding-value (find-var-binding mark-list 
                                                                         (z:varref-var expr))))])]
            
            [(z:app? expr)
             (map recur (cons (z:app-fun expr) (z:app-args expr)))]
            
            [(z:struct-form? expr)
             (if (comes-from-define-struct? expr)
                 (e:internal-error expr "this expression should have been skipped during reconstruction")
                 (let ([super-expr (z:struct-form-super expr)]
                       [raw-type (read->raw (z:struct-form-type expr))]
                       [raw-fields (map read->raw (z:struct-form-fields expr))])
                   (if super-expr
                       `(struct (,raw-type ,(recur super-expr))
                                ,raw-fields)
                       `(struct ,raw-type ,raw-fields))))]
            
            [(z:if-form? expr)
             (cond
               [(comes-from-cond? expr)
                `(cond ,@(rectify-cond-clauses (z:zodiac-start expr) expr mark-list lexically-bound-vars))]
               [(comes-from-and? expr)
                `(and ,@(rectify-and-clauses (z:zodiac-start expr) expr mark-list lexically-bound-vars))]
               [(comes-from-or? expr)
                `(or ,@(rectify-or-clauses (z:zodiac-start expr) expr mark-list lexically-bound-vars))]
               [else
                `(if ,(recur (z:if-form-test expr))
                     ,(recur (z:if-form-then expr))
                     ,(recur (z:if-form-else expr)))])]
            
            [(z:quote-form? expr)
             (let ([raw (read->raw (z:quote-form-expr expr))])
               (cond [(or (string? raw)
                          (number? raw)
                          (boolean? raw))
                      raw]
                     [else
                      `(quote ,raw)]))]

            [(z:case-lambda-form? expr)
             (let* ([arglists (z:case-lambda-form-args expr)]
                    [bodies (z:case-lambda-form-bodies expr)]
                    [o-form-arglists
                     (map (lambda (arglist) 
                            (improper-map z:binding-orig-name
                                          (arglist->ilist arglist)))
                          arglists)]
                    [var-form-arglists
                     (map (lambda (arglist)
                            (map z:binding-var (z:arglist-vars arglist)))
                          arglists)]
                    [o-form-bodies 
                     (map (lambda (body var-form-arglist)
                            (rectify-source-expr body 
                                                 mark-list
                                                 (append var-form-arglist
                                                         lexically-bound-vars)))
                          bodies
                          var-form-arglists)])
               (cond [(or (comes-from-lambda? expr) (comes-from-define? expr))
                      `(lambda ,(car o-form-arglists) ,(car o-form-bodies))]
                     [(comes-from-case-lambda? expr)
                      `(case-lambda ,@(map list o-form-arglists o-form-bodies))]
                     [else
                      (e:dynamic-error expr "unknown source for case-lambda")]))]
            
            ; we won't call rectify-source-expr on define-values expressions
            
            [else
             (print-struct #t)
             (e:dynamic-error
              expr
              (format "stepper:rectify-source: unknown object to rectify, ~a~n" expr))])))
 
  ; these macro unwinders (and, or) are specific to beginner level
  
  (define (rectify-and-clauses and-source expr mark-list lexically-bound-vars)
    (let ([rectify-source (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-vars))])
      (if (and (z:if-form? expr) (equal? and-source (z:zodiac-start expr)))
          (cons (rectify-source (z:if-form-test expr))
                (rectify-and-clauses and-source (z:if-form-then expr) mark-list lexically-bound-vars))
          null)))
  
  (define (rectify-or-clauses or-source expr mark-list lexically-bound-vars)
    (let ([rectify-source (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-vars))])
      (if (and (z:if-form? expr) (equal? or-source (z:zodiac-start expr)))
          (cons (rectify-source (z:if-form-test expr))
                (rectify-or-clauses or-source (z:if-form-else expr) mark-list lexically-bound-vars))
          null)))
  
  (define (rectify-cond-clauses cond-source expr mark-list lexically-bound-vars)
    (let ([rectify-source (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-vars))])
      (if (equal? cond-source (z:zodiac-start expr))
          (if (z:if-form? expr)
              (cons (list (rectify-source (z:if-form-test expr))
                          (rectify-source (z:if-form-then expr)))
                    (rectify-cond-clauses cond-source (z:if-form-else expr) mark-list lexically-bound-vars))
              null)
          `((else ,(rectify-source expr))))))
  
  ; reconstruct : takes a parsed list of expressions, a list of continuation-marks,
  ; a list of all the names defined in the users program, and which top-level expression is
  ; currently being evaluated, and it produces a list containing the reconstructed sexp, and the
  ; sexp which is the redex.  Note that the redex is guaranteed to be eq? to some element in the 
  ; reconstructed program
  
  ;((list-of z:parsed) (list-of mark) (list-of symbol) num -> 
  ; (list sexp sexp))
  
  (define (reconstruct expr-list mark-list all-defs-list current-def-num break-kind)
    
    (local
        ((define (rectify-source-top-marks expr)
           (rectify-source-expr expr mark-list null))
         

        
         (define (rectify-top-level expr current-expr? so-far)
           (if (z:define-values-form? expr)
               (let ([vars (z:define-values-form-vars expr)]
                     [val (z:define-values-form-val expr)])
                 (cond [(comes-from-define-struct? expr)
                        (let* ([struct-expr val]
                               [super-expr (z:struct-form-super struct-expr)]
                               [raw-type (read->raw (z:struct-form-type struct-expr))]
                               [raw-fields (map read->raw (z:struct-form-fields struct-expr))])
                          `(define-struct
                            ,(if super-expr
                                 (list raw-type (if current-expr?
                                                    so-far
                                                    (rectify-source-top-marks super-expr)))
                                 raw-type)
                            ,raw-fields))]
                       [(or (comes-from-define-procedure? expr)
                            (and (comes-from-define? expr)
                                 current-expr?
                                 (pair? so-far)
                                 (eq? (car so-far) 'lambda)))
                        (let* ([proc-name (z:varref-var
                                           (car (z:define-values-form-vars expr)))]
                               [o-form-proc (if current-expr?
                                                so-far
                                                (rectify-source-top-marks 
                                                 (z:define-values-form-val expr)))])
                          (o-form-lambda->define o-form-proc proc-name))]
                                              
                       [(comes-from-define? expr)
                        `(define 
                           ,(z:varref-var (car vars))
                           ,(if current-expr?
                                so-far
                                (rectify-source-top-marks val)))]
                       
                       [else
                        `(define-values 
                           ,(map read->raw vars)
                           ,(rectify-source-top-marks val))]))
               (if current-expr?
                   so-far
                   (rectify-source-top-marks expr))))
         
         (define (rectify-old-var var)
           (let ([val (mark-binding-value (find-var-binding mark-list var))])
             (rectify-value val)))
                  
         (define (rectify-old-expression expr vars)
           (let ([values (map (lambda (var) (mark-binding-value
                                             (find-var-binding mark-list var)))
                              vars)])
             (cond [(z:define-values-form? expr)
                    (if (comes-from-define-struct? expr)
                        (read->raw (expr-read expr))
                        (let ([rectified-vars (map rectify-value values)])
                          (cond [(comes-from-define-procedure? expr)
                                 (let* ([mark (closure-record-mark  (closure-table-lookup (car values)))]
                                        [rectified (rectify-source-expr (mark-source mark) (list mark) null)])
                                   (o-form-lambda->define (o-form-case-lambda->lambda rectified)
                                                          (car vars)))]
                                [(comes-from-define? expr)
                                 `(define ,(car vars) ,(car rectified-vars))]
                                [else
                                 `(define-values ,vars
                                    ,(if (= (length values) 1)
                                         (car rectified-vars)
                                         `(values ,@rectified-vars)))])))]
                   [else
                    (rectify-old-var (top-level-exp-gensym-source expr))])))

         (define (reconstruct-inner mark-list so-far)
           (let ([rectify-source-current-marks 
                  (lambda (expr)
                    (rectify-source-expr expr mark-list null))])
             (if (null? mark-list)
                 so-far
                 (let* ([top-mark (car mark-list)]
                        [expr (mark-source top-mark)])
                   (cond 
                     ; variable references
                     [(z:varref? expr)
                      (if (eq? so-far nothing-so-far)
                          (rectify-source-current-marks expr)
                          (e:dynamic-error expr 
                                           "variable reference given as context"))]
                     
                     ; applications
                     
                     [(z:app? expr)
                      (let* ([sub-exprs (cons (z:app-fun expr) (z:app-args expr))]
                             [arg-temps (build-list (length sub-exprs) get-arg-symbol)]
                             [arg-temp-syms (map z:varref-var arg-temps)]
                             [arg-vals (map (lambda (arg-sym) 
                                              (mark-binding-value (find-var-binding mark-list arg-sym)))
                                            arg-temp-syms)])
                        (case (mark-label (car mark-list))
                          ((not-yet-called)
                           ;                         (printf "length of mark-list: ~s~n" (length mark-list))
                           ;                         (printf "mark has binding for third arg: ~s~n" 
                           ;                                 (find-var-binding (list (car mark-list)) (z:varref:var 
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
                               (let* ([eval-exprs (list-take (length evaluated) sub-exprs)]
                                      [rectified-evaluated (map rectify-value evaluated)])
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
                           (e:static-error expr "bad label in application mark"))))]
                     
                     ; define-struct 
                     
                     [(z:struct-form? expr)
                      (if (comes-from-define-struct? expr)
                          so-far
                          (let ([super-expr (z:struct-form-super expr)]
                                [raw-type (read->raw (z:struct-form-type expr))]
                                [raw-fields (map read->raw (z:struct-form-fields expr))])
                            (if super-expr
                                `(struct (,raw-type ,so-far)
                                         ,raw-fields)
                                `(struct ,raw-type ,raw-fields))))]
                     
                     ; if
                     
                     [(z:if-form? expr)
                      (let ([test-exp (if (eq? so-far nothing-so-far)
                                          (rectify-source-current-marks 
                                           (create-bogus-bound-varref if-temp))
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
                                    ,(rectify-source-current-marks (z:if-form-else expr)))]))]
                     
                     ; quote : there is no mark or break on a quote.
                     
                     ; define-values : define's don't get marks, so they can't occur here
                     
                     ; lambda : there is no mark or break on a quote
                     
                     [else
                      (print-struct #t)
                      (e:dynamic-error
                       expr
                       (format "stepper:reconstruct: unknown object to reconstruct, ~a~n" expr))])))))
         
         
         (define old-defs
           (let ([old-exps (list-take current-def-num expr-list)]
                 [old-exp-vars (list-take current-def-num all-defs-list)])
             (map rectify-old-expression old-exps old-exp-vars)))
         
         (define redex #f)
         
         (define current-def-rectifier
           (lambda (so-far mark-list first)
             (if (null? mark-list)
                 (rectify-top-level (list-ref expr-list current-def-num) #t so-far)
                 (current-def-rectifier 
                  (let ([reconstructed (reconstruct-inner mark-list so-far)])
                    (when first
                      (set! redex reconstructed))
                    reconstructed)
                  (cdr mark-list)
                  #f))))
         
         (define last-defs-thunk
           (lambda () 
             (if (>= current-def-num (length expr-list))
                 ()
                 (map (lambda (expr) (rectify-top-level expr #f null)) 
                      (list-tail expr-list (+ current-def-num 1))))))
         )
      
      (if (final-mark-list? mark-list)
          (list old-defs null)
          (let ([last-defs (last-defs-thunk)])
            (if (eq? break-kind 'pre-break)
                (let* ([first-exp (rectify-source-expr (mark-source (car mark-list)) mark-list null)]
                       [current-def (current-def-rectifier first-exp (cdr mark-list) #f)])
                  (list (append old-defs (list current-def) last-defs) first-exp))
                (let ([current-def (current-def-rectifier  nothing-so-far mark-list #t)])
                  (list (append old-defs (list current-def) last-defs) redex))))))))
