(unit/sig stepper:reconstruct^
  (import [z : zodiac:system^]
          mzlib:function^
	  [e : stepper:error^]
	  stepper:shared^)
  
  (define nothing-so-far (gensym "nothing-so-far-"))
  
  (define-values (closure-table-put! closure-table-lookup)
    (let ([closure-table (make-hash-table-weak)])
      (values
       (lambda (key value)
	 (hash-table-put! closure-table key value))
       (lambda (key)
         (hash-table-get closure-table key)))))
  
  (define (find-var-binding mark-list var)
    (if (null? mark-list)
        ; must be a primitive
        (list #f (make-varref var #t))
	; (error var "no binding found for variable.")
	(let* ([mark-vars (cdr ((car mark-list)))]
	       [matches (filter (lambda (mark-var)
				  (eq? var (varref-var (cadr mark-var))))
                                mark-vars)])
	  (cond [(null? matches)
		 (find-var-binding (cdr mark-list) var)]
		[(> (length matches) 1)
		 (error matches "more than one variable binding found")]
		[else ; (length matches) = 1
		 (car matches)]))))

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
          [(and (procedure? val) (inferred-name val))
           (inferred-name val)]
          [else val]))
  
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
  
  
  (define (rectify-source-expr expr mark-list lexically-bound-vars)
    (let ([recur (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-vars))])
      (cond [(z:varref? expr)
             (if (memq (z:varref-var expr) lexically-bound-vars)
                 (z:binding-orig-name (z:bound-varref-binding expr))
                 (let* ([var-record (find-var-binding mark-list (z:varref-var expr))]
                        [var-val-thunk (car var-record)]
                        [var-top-level? (varref-top-level? (cadr var-record))])
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
                 `(cond ,@(rectify-cond-clauses (z:zodiac-start expr) expr mark-list))
                 `(if ,(recur (z:if-form-test expr))
                      ,(recur (z:if-form-then expr))
                      ,(recur (z:if-form-else expr))))]
            
            [(z:quote-form? expr)
             `(quote ,(read->raw (z:quote-form-expr expr)))]

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
 
  (define (rectify-cond-clauses cond-source expr mark-list)
    (if (and (z:if-form? expr) (equal? cond-source (z:zodiac-start expr)))
        (cons (list (rectify-source-expr (z:if-form-test expr) mark-list)
                    (rectify-source-expr (z:if-form-then expr) mark-list))
              (rectify-cond-clauses cond-source (z:if-form-else expr)))
        `((else ,(rectify-source-expr expr mark-list)))))
  
  
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
                                 (eq? (car so-far 'lambda))))
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
         
         (define (rectify-old-definition var)
           (let ([val ((car (find-var-binding mark-list var)))])
             (if (procedure? val)
                 (let* ([info ((closure-table-lookup val))]
                        [expr (car info)]
                        [o-form-proc (rectify-source-expr expr (list (lambda () info)) null)])
                   (o-form-lambda->define (o-form-case-lambda->lambda o-form-proc) var)) 
                 `(define ,var ,val))))
                 
 

         (define (reconstruct-inner mark-list so-far)
           (if (null? mark-list)
               so-far
               (let* ([top-mark (car mark-list)]
                      [expr (car (top-mark))])
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
                           [arg-sym-list (build-list (length sub-exprs) get-arg-symbol)]
                           [arg-vals (map (lambda (arg-sym) 
                                            ((car (find-var-binding mark-list arg-sym))))
                                          arg-sym-list)])
                      (letrec
                          ([split-lists
                            (lambda (exprs vals)
                              (if (or (null? vals)
                                      (eq? (car vals) *unevaluated*))
                                  (values null exprs)
                                  (let-values ([(small-vals small-exprs)
                                                (split-lists (cdr exprs) (cdr vals))])
                                    (values (cons (car vals) small-vals) small-exprs))))])
                        (printf "app-sub-exprs: ~a~napp-arg-vals: ~a~n"
                                sub-exprs
                                arg-vals)
                        (let-values ([(evaluated unevaluated) (split-lists sub-exprs arg-vals)])
                          (cond [(= (length unevaluated) 0)
                                 (if (eq? so-far nothing-so-far)
                                     `(...) ; in unannotated code
                                     `(... ,so-far ...))] ; context contains unevaluated code
                                [(and (= (length evaluated) 0)
                                      (eq? so-far nothing-so-far))
                                 (map rectify-source-current-marks unevaluated)] ; first application step
                                [else
                                 (printf "app-so-far: ~a~n" so-far)
                                 (printf "app-unevaluated: ~a~n" unevaluated)
                                 (append (map rectify-value evaluated)
                                         (cons so-far
                                               (map rectify-source-current-marks (cdr unevaluated))))]))))]
                   
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
                    ;; ADD NOTHING_SO_FAR test
                    (if (comes-from-cond? expr)
                        (let* ([clause (list so-far (rectify-source-current-marks (z:if-form-then expr)))]
                               [cond-source (z:zodiac-start expr)]
                               [rest-clauses (rectify-cond-clauses cond-source (z:if-form-else expr) mark-list)])
                          `(cond ,clause ,@rest-clauses))
                        `(if ,so-far ,(rectify-source-current-marks (z:if-form-else expr))))]
                   
                   ; quote
                   
                   [(z:quote-form? expr)
                    (if (not (eq? so-far nothing-so-far))
                        (e:dynamic-error expr 
                                         "quoted expression given as context")
                        (let ([a (rectify-source-current-marks expr)])
                          (printf "rectified quote: ~a~n" a)
                          a))]
                   
                   
                   ; define-values : define's don't get marks, so they can't occur here
                   
                   ; lambda
                   
                   [(z:case-lambda-form? expr)
                    (if (not (eq? so-far nothing-so-far))
                        (e:dynamic-error expr "lambda expression given as context")
                        (rectify-source-current-marks expr))]
                   
                   [else
                    (print-struct #t)
                    (e:dynamic-error
                     expr
                     (format "stepper:reconstruct: unknown object to reconstruct, ~a~n" expr))]))))
         
         (define old-defs
           (let ([old-defs (flatten-take current-def-num all-defs-list)])
             (map rectify-old-definition old-defs)))
         
         (define current-def 
           (let loop ([so-far nothing-so-far] [mark-list mark-list])
             (if (null? mark-list)
                 (rectify-top-level (list-ref expr-list current-def-num) #t so-far)
                 (loop (reconstruct-inner mark-list so-far) (cdr mark-list)))))
                  
         (define last-defs
           (map (lambda (expr) (rectify-top-level expr #f null)) 
                (list-tail expr-list (+ current-def-num 1))))
         )
      
      (append old-defs (list current-def) last-defs))))
 