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
                
  (define (comes-from-define? expr)
    (let* ([read-expr (expr-read expr)]
           [first-symbol (read-expr-first-symbol read-expr)])
      (and first-symbol
           (eq? first-symbol 'define))))
  
      
  (define (comes-from-define-procedure? expr)
    (if (comes-from-define? expr)
        (let* ([read-expr (expr-read expr)]
               [defined-var (cadr (z:read-object read-expr))])
          (if (z:scalar? defined-var)
              #f
              #t))
        #f))
  
  (define (comes-from-define-struct? expr)
    (let* ([read-expr (expr-read expr)]
           [first-symbol (read-expr-first-symbol read-expr)])
      (and first-symbol
           (eq? first-symbol 'define-struct))))
  
  (define (comes-from-cond? expr)
    (let* ([read-expr (expr-read expr)]
           [first-symbol (read-expr-first-symbol read-expr)])
      (and first-symbol
           (eq? first-symbol 'cond))))
  
  (define (rectify-value val)
    (cond [(and (procedure? val) (primitive? val))
           (primitive-name val)]
          [(and (procedure? val) (inferred-name val))
           (inferred-name val)]
          [else val]))
  
  (define (rectify-source-expr expr mark-list)
    (cond [(z:varref? expr)
           (let* ([var-record (find-var-binding mark-list (z:varref-var expr))]
                  [var-val (car var-record)]
                  [var-top-level? (varref-top-level? (cadr var-record))])
             (if var-top-level?
                 (z:varref-var expr)
                 (rectify-value var-val)))]
          
          [(z:app? expr)
           (map (lambda (expr) (rectify-source-expr expr mark-list))
                (cons (z:app-fun expr) (z:app-args expr)))]
          
          [(z:struct-form? expr)
           (if (comes-from-define-struct? expr)
               (e:internal-error expr "this expression should have been skipped during reconstruction")
               (let ([super-expr (z:struct-form-super expr)]
                     [raw-type (read->raw (z:struct-form-type expr))]
                     [raw-fields (map read->raw (z:struct-form-fields expr))])
                 (if super-expr
                     `(struct (,raw-type ,(rectify-source-expr super-expr))
                              ,raw-fields)
                     `(struct ,raw-type ,raw-fields))))]
          
          [(z:if-form? expr)
           (if (comes-from-cond? expr)
               `(cond ,@(rectify-cond-clauses (z:zodiac-start expr) expr mark-list))
               `(if ,(rectify-source-expr (z:if-form-test expr) mark-list)
                    ,(rectify-source-expr (z:if-form-then expr) mark-list)
                    ,(rectify-source-expr (z:if-form-else expr) mark-list)))]
          
          [(z:quote-form? expr)
           `(quote ,(read->raw (z:quote-form-expr expr)))]
          
          ; we won't call rectify-source-expr on define-values expressions
       
          [else
           (print-struct #t)
           (e:dynamic-error
            expr
            (format "stepper:rectify-source: unknown object to rectify, ~a~n" expr))]))
 
  (define (rectify-cond-clauses cond-source expr mark-list)
    (if (and (z:if-form? expr) (equal? cond-source (z:zodiac-start expr)))
        (cons (list (rectify-source-expr (z:if-form-test expr) mark-list)
                    (rectify-source-expr (z:if-form-then expr) mark-list))
              (rectify-cond-clauses cond-source (z:if-form-else expr)))
        `((else ,(rectify-source-expr expr mark-list)))))
  
  
  (define (reconstruct expr-list mark-list all-defs-list current-def-num)
    
    (local
        ((define (rectify-source-current-marks expr)
           (rectify-source-expr expr mark-list))
         

        
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
                       [(comes-from-define-procedure? expr)
                        (let ([name (read->raw (car vars))]
                              [args (map read->raw (car (z:case-lambda-form-args val)))]
                              [body (rectify-source-current-marks (car (z:case-lambda-form-bodies)))])
                          `(define ,(cons name args) ,body))]
                       
                       ; this won't work for (define a (begin 3 (lambda (x) x)))
                       
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
           (let ([val (car (find-var-binding mark-list var))])
             (if (procedure? val)
                 (let* ([info (closure-table-lookup val)]
                        [expr (car info)]
                        [args-list-list (z:case-lambda-form-args expr)]
                        [bodies-list (z:case-lambda-form-args expr)])
                   (if (> (length bodies-list) 1)
                       (e:dynamic-error expr "too many bodies!")
                       `(define ,(cons var (car args-list-list))
                          ,(rectify-source-expr (car bodies-list) (list info)))))
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
                                            (car (find-var-binding mark-list arg-sym)))
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
                        nothing-so-far)]
                   
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


         
         
                   
                        
                        
                        
                        