(unit/sig stepper:shared^
  (import [z : zodiac:system^]
	  stepper:error)
  
  ; copied from aries
  
  (define read->raw
    (lambda (read)
      (if (z:zodiac? read)
	  (z:sexp->raw read)
	  read)))
 
  (define arglist->ilist
    (lambda (arglist)
      (cond
	((z:list-arglist? arglist)
	 (z:arglist-vars arglist))
	((z:ilist-arglist? arglist)
	 (let loop ((vars (z:arglist-vars arglist)))
	   (if (null? (cddr vars))
	       (cons (car vars) (cadr vars))
	       (cons (car vars) (loop (cdr vars))))))
	((z:sym-arglist? arglist)
	 (car (z:arglist-vars arglist)))
	(else
	 (e:internal-error arglist
			   "Given to arglist->ilist")))))
  
  ; gensyms needed by both the annotater and the reconstructor:
  
  ; *unevaluated* is the value assigned to temps before they are evaluated.
  (define *unevaluated* (gensym "unevaluated-"))
  
  ; get-arg-symbol maintains a list of gensyms associated with the non-negative
  ; integers.  These symbols are used in the elaboration of applications; the nth
  ; in the application is evaluated and stored in a variable whose name is the nth
  ; gensym supplied by get-arg-symbol.
  
  ; I'm just going to implement this with a simple assq list. if this isn't good
  ; enough, it can always be improved later.
  
  (define get-arg-symbol
    (let ([assoc-table (make-hash-table)])
      (lambda (arg-num)
	(let ([arg-symbol (hash-table-get assoc-table arg-num (lambda () #f))])
	  (if arg-symbol
	      arg-symbol
	      (begin
		(let ([new-sym (gensym (string-append "arg" (number->string arg-num) "-"))])
		  (hash-table-put! assoc-table arg-num new-sym)
		  new-sym)))))))
  
  ; test cases: (returns #t on success)
  (let ([arg3 (get-arg-symbol 3)]
        [arg2 (get-arg-symbol 2)]
        [arg1 (get-arg-symbol 1)]
        [arg2p (get-arg-symbol 2)])
    (and (not (eq? arg3 arg2))
         (not (eq? arg3 arg1))
         (not (eq? arg3 arg2p))
         (not (eq? arg2 arg1))
         (eq? arg2 arg2p)
         (not (eq? arg1 arg2p))))
  
  ; the varref structure contains a name and a boolean which indicates whether the reference
  ; is a top-level (substitutable) one (this includes unit-bound and class-bound, but _not_
  ; local-bound
  
  (define-struct varref (var top-level?))
    
  ; source correlation stuff:
  
  (define source-table (make-hash-table-weak))

  (define (register-source key value)
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
  
  
  
) 