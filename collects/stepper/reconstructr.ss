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
                  *
                 
                 

                  
                  
                  (define (reconstruct-inner mark-list so-far)
           (if (null? mark-list)
               so-far
               (let* ([top-mark (car mark-list)]
                      [expr (car top-mark)])
                 (cond 
                   ; variable references
                   [(z:varref? expr)
                    (cond ((not (null? so-far))
                           (e:dynamic-error expr 
                                            "variable reference given as context"))
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
                    (
                   