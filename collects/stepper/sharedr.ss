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
  
  
) 