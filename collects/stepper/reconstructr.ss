(unit/sig stepper:reconstruct^
  (import [z : zodiac:system^]
          mzlib:function^
	  [e : stepper:error^]
          [p : mzlib:print-convert^]
          [b : userspace:basis^]
	  stepper:shared^)
    
  (define beginner-level-defined-vars
    (list first second third fourth fifth sixth seventh eighth rest cons? empty empty?
          boolean=? identity compose foldl foldr last-pair remv remq remove remv* remq* remove*
          assf memf filter build-string build-vector build-list quicksort loop-until ignore-errors))
  
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
        (list #f #f)
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

  ; JEEZ, I'm dumb! Why not just use read->raw to make my life 100% easier!?
  
  (define (read-expr-first-symbol read-expr)
    (if (z:list? read-expr)
        (let ([first (car (z:read-object read-expr))])
          (if (z:symbol? first)
              (z:symbol-orig-name first)
              #f))
        #f))

  (define (make-comes-from-blah blah)
    (lambda (expr)
      (let* ([read-expr (expr-read expr)]
             [first-symbol (read-expr-first-symbol read-expr)])
        (and first-symbol
             (eq? first-symbol blah)))))
  
  (define comes-from-define?
    (make-comes-from-blah 'define))

  (define (comes-from-define-procedure? expr)
    (if (comes-from-define? expr)
        (let* ([read-expr (expr-read expr)]
               [defined-var (cadr (z:read-object read-expr))])
          (if (z:scalar? defined-var)
              #f
              #t))
        #f))
  
  (define comes-from-define-struct?
    (make-comes-from-blah 'define-struct))
  
  (define comes-from-cond?
    (make-comes-from-blah 'cond))
  
  (define comes-from-lambda?
    (make-comes-from-blah 'lambda))
  
  (define comes-from-case-lambda?
    (make-comes-from-blah 'case-lambda))
  
  (define (rectify-value val)
    (cond [(and (procedure? val) (primitive? val))
           (primitive-name val)]
          [(procedure? val)
           (let ([recorded-name (closure-record-name
                                 (closure-table-lookup val))])
             (if recorded-name
                 recorded-name
                 'unknown-procedure))]
          [else (parameterize
                    ([p:constructor-style-printing #t]
                     [p:abbreviate-cons-as-list #f])
                  (p:print-convert val))]))
  
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
 
  (define (stop-here? mark-list all-defs)
    (not (and (pair? mark-list)
              (let ([expr (mark-source (car mark-list))])
                (and (z:varref? expr)
                     (or (z:bound-varref? expr)
                         (let ([var (z:varref-var expr)])
                           (or (built-in-name var)
                               (memq var beginner-level-defined-vars)
                               (call-with-current-continuation
                                (lambda (k)
                                  (with-handlers ([exn:variable?
                                                   (lambda (exn) (k #f))])
                                    (procedure? (mark-binding-value
                                                 (find-var-binding mark-list (z:varref-var expr)))))))))))))))
  
  (define (rectify-source-expr expr mark-list lexically-bound-vars)
    (let ([recur (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-vars))])
      (cond [(z:varref? expr)
             (if (memq (z:varref-var expr) lexically-bound-vars)
                 (z:binding-orig-name (z:bound-varref-binding expr))
                 (let* ([var-record (find-var-binding mark-list (z:varref-var expr))]
                        [var-val-thunk (car var-record)]
                        [var-top-level? (z:top-level-varref? expr)])
                   (if var-top-level?
                       (z:varref-var expr)
                       (rectify-value (var-val-thunk)))))]
            
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
             (if (comes-from-cond? expr)
                 `(cond ,@(rectify-cond-clauses (z:zodiac-start expr) expr mark-list lexically-bound-vars))
                 `(if ,(recur (z:if-form-test expr))
                      ,(recur (z:if-form-then expr))
                      ,(recur (z:if-form-else expr))))]
            
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
 
  (define (rectify-cond-clauses cond-source expr mark-list lexically-bound-vars)
    (let ([rectify-source (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-vars))])
      (if (and (z:if-form? expr) (equal? cond-source (z:zodiac-start expr)))
          (cons (list (rectify-source (z:if-form-test expr))
                      (rectify-source (z:if-form-then expr)))
                (rectify-cond-clauses cond-source (z:if-form-else expr) mark-list lexically-bound-vars))
          `((else ,(rectify-source expr))))))
  
  ; reconstruct : takes a parsed list of expressions, a list of continuation-marks,
  ; a list of all the names defined in the users program, and which top-level expression is
  ; currently being evaluated, and it produces a list containing the reconstructed sexp, and the
  ; sexp which is the redex.  Note that the redex is guaranteed to be eq? to some element in the 
  ; reconstructed program
  
  ;((list-of z:parsed) (list-of mark) (list-of symbol) num -> 
  ; (list sexp sexp))
  
  (define (reconstruct expr-list mark-list all-defs-list current-def-num)
    
    (local
        ((define (rectify-source-current-marks expr)
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
                                                    (rectify-source-current-marks super-expr)))
                                 raw-type)
                            ,raw-fields))]
                       [(or (comes-from-define-procedure? expr)
                            (and (comes-from-define? expr)
                                 current-expr?
                                 (eq? (car so-far) 'lambda)))
                        (let* ([proc-name (z:varref-var
                                           (car (z:define-values-form-vars expr)))]
                               [o-form-proc (if current-expr?
                                                so-far
                                                (rectify-source-current-marks 
                                                 (z:define-values-form-val expr)))])
                          (o-form-lambda->define o-form-proc proc-name))]
                                              
                       [(comes-from-define? expr)
                        `(define 
                           ,(z:varref-var (car vars))
                           ,(if current-expr?
                                so-far
                                (rectify-source-current-marks val)))]
                       
                       [else
                        `(define-values 
                           ,(map read->raw vars)
                           ,(rectify-source-current-marks val))]))
               (if current-expr?
                   so-far
                   (rectify-source-current-marks expr))))
         
         (define (rectify-old-var var)
           (let ([val ((car (find-var-binding mark-list var)))])
             (if (procedure? val)
                 (let* ([closure-record (closure-table-lookup val)]
                        [info ((closure-record-mark closure-record))]
                        [expr (car info)])
                   (rectify-source-expr expr (list (lambda () info)) null))
                 (rectify-value val))))
                  
         (define (rectify-old-expression expr vars)
           (cond [(z:define-values-form? expr)
                  (if (comes-from-define-struct? expr)
                      (read->raw (expr-read expr))
                      (let ([rectified-vars (map rectify-old-var vars)])
                        (cond [(comes-from-define-procedure? expr)
                               (o-form-lambda->define (o-form-case-lambda->lambda (car rectified-vars))
                                                      (car vars))]
                              [(comes-from-define? expr)
                               `(define ,(car vars) ,(car rectified-vars))]
                              [else
                               `(define-values ,vars
                                  ,(if (= (length rectified-vars) 1)
                                       (car rectified-vars)
                                       `(values ,@rectified-vars)))])))]
                 [else
                  (rectify-old-var (top-level-exp-gensym-source expr))]))

         (define (reconstruct-inner mark-list so-far)
           (if (null? mark-list)
               so-far
               (let* ([top-mark (car mark-list)]
                      [expr (mark-source top-mark)])
                 (cond 
                   ; variable references
                   [(z:varref? expr)
                    (if (not (eq? nothing-so-far so-far))
                        (e:dynamic-error expr 
                                         "variable reference given as context")
                        (rectify-source-current-marks expr))]
                   
                   ; applications
                   
                   [(z:app? expr)
                    (let* ([sub-exprs (cons (z:app-fun expr) (z:app-args expr))]
                           [arg-temps (build-list (length sub-exprs) get-arg-symbol)]
                           [arg-temp-syms (map z:varref-var arg-temps)]
                           [arg-vals (map (lambda (arg-sym) 
                                            ((car (find-var-binding mark-list arg-sym))))
                                          arg-temp-syms)]
                           ; this next function is a terrible hack. It will have to go,
                           ; at some point. The "inferred-name" thing doesn't work at
                           ; all for non-primitives.
                           [beginner-rectify-app-value
                            (lambda (expr val)
                              (if (and (procedure? val) (z:varref? expr))
                                  (if (z:top-level-varref? expr)
                                      (z:varref-var expr)
                                      (z:binding-orig-name (z:bound-varref-binding expr)))
                                  (rectify-value val)))])
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
                             (let* ([eval-exprs (list-take (length evaluated) sub-exprs)]
                                    [rectified-evaluated (map beginner-rectify-app-value 
                                                             eval-exprs
                                                             evaluated)])
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
                      (if (comes-from-cond? expr)
                          (let* ([clause (list test-exp (rectify-source-current-marks (z:if-form-then expr)))]
                                 [cond-source (z:zodiac-start expr)]
                                 [rest-clauses (rectify-cond-clauses cond-source (z:if-form-else expr) mark-list null)])
                            `(cond ,clause ,@rest-clauses))
                          `(if ,test-exp 
                               ,(rectify-source-current-marks (z:if-form-then expr))
                               ,(rectify-source-current-marks (z:if-form-else expr)))))]
                   
                   ; quote : there is no mark or break on a quote.
                   
                   ; define-values : define's don't get marks, so they can't occur here
                   
                   ; lambda : there is no mark or break on a quote

                   [else
                    (print-struct #t)
                    (e:dynamic-error
                     expr
                     (format "stepper:reconstruct: unknown object to reconstruct, ~a~n" expr))]))))
         
         
         (define old-defs
           (let ([old-exps (list-take current-def-num expr-list)]
                 [old-exp-vars (list-take current-def-num all-defs-list)])
             (map rectify-old-expression old-exps old-exp-vars)))
         
         (define redex #f)
         
         (define current-def-thunk
           (lambda ()
             (let loop ([so-far nothing-so-far] [mark-list mark-list] [first #t])
               (if (null? mark-list)
                   (rectify-top-level (list-ref expr-list current-def-num) #t so-far)
                   (loop 
                    (let ([reconstructed (reconstruct-inner mark-list so-far)])
                      (when first
                        (set! redex reconstructed))
                      reconstructed)
                    (cdr mark-list)
                    #f)))))
         
         (define last-defs-thunk
           (lambda () 
             (if (>= current-def-num (length expr-list))
                 ()
                 (map (lambda (expr) (rectify-top-level expr #f null)) 
                      (list-tail expr-list (+ current-def-num 1))))))
         )
      
      (if (final-mark-list? mark-list)
          (list old-defs null)
          (list
           (append old-defs (list (current-def-thunk)) (last-defs-thunk))
           redex)))))
