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

  
  (define (reconstruct mark-list)
    
    (letrec
        ([reconstruct/inner
          (lambda (mark-list so-far)
            (if (null? mark-list)
                so-far
                (let* ([top-mark (car mark-list)]
                       [expr (car top-mark)])
                  (cond 
                    ; variable references
                    ((z:varref? expr)
                     (cond ((not (null? so-far))
                            (e:dynamic-error expr 
                                             "variable reference given as context"))
                           (else
                            (let* ([var-record (find-var-binding mark-list (z:varref-var expr))]
                                   [var-val (car var-record)]
                                   [var-top-level? (varref-top-level? (cadr var-record))])
                              (if var-top-level?
                                  (z:binding-orig-name
                                   (z:bound-varref-binding expr))
                                  var-val)))))
                    
                    ; applications
                    
                    ((z:app? expr)
                     (let* ([sub-exprs (cons (z:app-fun expr) (z:app-args expr))]
                            [arg-sym-list (build-list (length sub-exprs) get-arg-symbol)]
                            [arg-vals (map (lambda (arg-sym) 
                                             (car (find-var-binding mark-list arg-sym)
                     
                     
                     
                     
                     
  
    