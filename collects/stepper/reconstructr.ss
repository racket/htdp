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
	  
  
  (define (reconstruct key mark-list)
    
    (define (reconstruct/inner mark-list so-far)
      (if (null? mark-list)
	  so-far
	  (let* ([top-mark (car mark-list)]
		 [mark-source (car top-mark)]
		 [expr (find-source-expr key mark-source)])
	    (cond (
		   ; variable references
		   
		   (z:scalar expr)
		   (cond ((not (null? so-far))
			  (e:dynamic-error expr 
					   "scalar expression given as context"))
			 ((not (z:symbol expr))
			  (e:dynamic-error expr
					   "non-symbol given as evaluated expression"))
			 (else
			  (let* ([var-record (find-var-binding mark-list (z:object expr))]
				 [var-val (car var-record)]
				 [var-top-level? (varref-top-level? (cadr var-record))])
			    (if var-top-level?
				(z:object expr)
				var-val)))))
		  
		  
					
  
    