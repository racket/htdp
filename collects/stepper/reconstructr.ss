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
  
  (define source-table (make-hash-table-weak))
  
  (define (register key value)
    (hash-table-put! source-table key value))
  
  (define (find-source-expr key offset)
    (let search-exprs ([exprs (hash-table-get key)])
      (let ([expr 
	     (car (filter 
		   (lambda (expr) 
		     (< offset (z:location-offset (z:zodiac-finish expr))))
		   exprs))])
	(if (= offset (z:location-offset (z:zodiac-start expr)))
	    expr
	    (cond
	      ((z:scalar? expr) (e:static-error "starting offset inside scalar:" offset))
	      ((z:sequence? expr) 
	       (let ([object (z:read-object expr)])
		 (cond
		   ((z:list? expr) (search-exprs object))
		   ((z:vector? expr) 
		    (search-exprs (vector->list object))) ; can source exprs be here?
		   ((z:improper-list? expr)
		    (search-exprs (search-exprs object))) ; can source exprs be here?
		   (else (e:static-error "unknown expression type in sequence" expr)))))
	      (else (e:static-error "unknown read type" expr)))))))
  
  (define (reconstruct mark-list)
    (
  
  
    