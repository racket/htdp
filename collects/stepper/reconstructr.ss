(unit/sig stepper:reconstruct^
  (import [z : zodiac:system^]
	  [e : stepper:error^]
	  stepper:unparse^)
  
  (define-values (closure-table-put! closure-table-lookup)
    (let ([closure-table (make-hash-table-weak)])
      (values
       (lambda (key value)
	 (hash-table-put! closure-table key value))
       (lambda (key)
	 (hash-table-get closure-table key)))))
  
  (define (find-var-binding mark-list var)
    (if (null? mark-list)
	(error var "no binding found for variable.")
	(let* ([mark-vars (cddr ((car mark-list)))]
	       [matches (filter (lambda (mark-var)
				  (eq? var (varref-var mark-var))))])
	  (cond ((null? matches)
		 (find-var-binding (cdr mark-list) var))
		((> (length matches) 1)
		 (error matches "more than one variable binding found"))
		(else ; (length matches) = 1
		 (car matches))))))

  (define (read-expr-first-symbol read-expr)
    (if (z:list? read-expr)
        (let ([first (car (z:object read-expr))])
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
               [defined-var (cadr (z:object read-expr))])
          (if (z:scalar defined-var)
              #f
              #t))
        #f))
  
  (define (comes-from-cond? expr)
    (let* ([read-expr (expr-read expr)]
           [first-symbol (read-expr-first-symbol read-expr)])
      (and first-symbol
           (eq? first-symbol 'cond))))
  
  
 
  (define (reconstruct-inner top-defs current-def mark-list so-far)
    
    (local
        ((define (rectify-value val)
           (cond [(and (procedure? val) (primitive? val))
                  (primitive-name val)]
                 [(and (procedure? val) (inferred-name val))
                  (inferred-name val)]
                 [else val]))
         
         (define (rectify-source-expr expr)
           (cond [(z:varref expr)
                  (let* ([var-record (find-var-binding mark-list (z:varref-var expr))]
                         [var-val (car var-record)]
                         [var-top-level? (varref-top-level? (cadr var-record))])
                    (if var-top-level?
                        (z:binding-orig-name
                         (z:bound-varref-binding expr))
                        (rectify-value var-val)))]
                 
                 [(z:app? expr)
                  (map rectify-source-expr (cons (z:app-fun expr) (z:app-args expr)))]
                 
                 [(z:struct-form? expr)
                  (if (comes-from-define-struct expr)
                      (e:internal-error expr "this expression should have been skipped during reconstruction")
                      (let ([super-expr (z:struct-form-super expr)]
                            [raw-type (read->raw (z:struct-form-type expr))]
                            [raw-fields (map read->raw (z:struct-form-fields expr))])
                        (if super-expr
                            `(struct (,raw-type ,(rectify-source-expr super-expr))
                              ,raw-fields)
                            `(struct ,raw-type ,raw-fields))))]
                 
                 [(z:if-form? expr)
                  (if (comes-from-cond expr)
                      `(cond ,@(rectify-cond-clauses (z:start expr) expr))
                      `(if ,(rectify-source-expr (z:if-form-test expr))
                           ,(rectify-source-expr (z:if-form-then expr))
                           ,(rectify-source-expr (z:if-form-else expr))))]
                 
                 [(z:quote-form? expr)
                  `(quote ,(read->raw (z:quote-form-expr expr)))]

                 
                 [(z:define-values? expr)
                  (let ([vars (z:define-values-form-vars expr)]
                        [val (z:define-values-form-val expr)])
                    (cond [(comes-from-define-struct? expr)
                           (let* ([struct-expr val]
                                  [super-expr (z:struct-form-super struct-expr)]
                                  [raw-type (read->raw (z:struct-form-type struct-expr))]
                                  [raw-fields (map read-raw (z:struct-form-fields struct-expr))])
                             `(define-struct
                               ,(if super-expr
                                    (list raw-type (rectify-source-expr super-expr))
                                    raw-type)
                               ,raw-fields))]
                          [(comes-from-define-procedure? expr)
                           (let ([name (read->raw (car vars))]
                                 [args (map read->raw (car (z:case-lambda-form-args val)))]
                                 [body (rectify-source-expr (car (z:case-lambda-form-bodies)))]
                                 
                        [(comes-from define expr)
                         `(define 
                            ,(read->raw (car vars))
                            ,(rectify-source-expr val))]
                        [else
                         `(define-values 
                            ,(map read->raw vars)
                            ,(rectify-source-expr val)))]
                 
                 
                         
                      
                 
                 
                 
         (define (rectify-cond-clauses cond-source expr)
           (if (and (z:if-form? expr) (equal? cond-source (z:start expr)))
               (cons (list (rectify-source-expr (z:if-form-test expr))
                           (rectify-source-expr (z:if-form-then expr)))
                     (rectify-cond-clauses cond-source (z:if-form-else expr)))
               `((else ,(rectify-source-expr expr)))))
                

                  
                  
                  (define (reconstruct-inner mark-list so-far)
           (if (null? mark-list)
               so-far
               (let* ([top-mark (car mark-list)]
                      [expr (car top-mark)])
                 (cond 
                   ; variable references
                   [(z:varref? expr)
                    (if (not (null? so-far))
                        (e:dynamic-error expr 
                                         "variable reference given as context")
                        (else (rectify-source-expr expr)))]
                   
                   ; applications
                   
                   [(z:app? expr)
                    (let* ([sub-exprs (cons (z:app-fun expr) (z:app-args expr))]
                           [arg-sym-list (build-list (length sub-exprs) get-arg-symbol)]
                           [arg-vals (map (lambda (arg-sym) 
                                            (car (find-var-binding mark-list arg-sym)))
                                          arg-sym-list)])
                      (if  (andmap (lambda (val) (not (eq? val *undefined*))) arg-vals)
                           `(... ,so-far ...)
                           (letrec
                               ([split-lists
                                 (lambda (exprs vals)
                                   (if (eq? (car vals) *undefined*)
                                       (values null exprs)
                                       (let-values ([(small-vals small-exprs)
                                                     (split-lists (cdr vals) (cdr exprs))])
                                         (values (cons (car vals) small-vals) small-exprs))))])
                             (let-values ([(evaluated unevaluated) (split-lists sub-exprs arg-vals)])
                               (append (map rectify-value evaluated)
                                       (cons so-far
                                             (map rectify-source-expr (cdr unevaluated))))))))]
                   
                   ; define-struct 
                   
                   [(z:struct-form? expr)
                    (if (comes-from-define-struct expr)
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
                        (let ([clause (list so-far (rectify-source-expr (z:if-form-then expr)))]
                              [cond-source (z:start expr)]
                              [rest-clauses (rectify-cond-clauses cond-source (z:if-form-else expr))])
                          `(cond ,clause ,@rest-clauses))
                        `(if ,so-far ,(rectify-source-expr (z:if-form-else expr))))]
                   
                   ; quote
                   
                   [(z:quote-form? expr)
                    (if (not (null? so-far))
                        (e:dynamic-error expr 
                                         "quoted expression given as context")
                        (else (rectify-source-expr expr)))]
                    
                    
                   ; define-values : define's don't get marks, so they can't occur here
                   
                   ; lambda
                   
                   [(z:case-lambda-form? expr)
                    (if (not (null? so-far))
                        (e:dynamic-error expr
                                         "lambda expression given as context")
                        
                   
                        
                   
                        
                        
                        
                        