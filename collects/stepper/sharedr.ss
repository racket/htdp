(unit/sig stepper:shared^
  (import [z : zodiac:system^]
	  [e : stepper:error^])
  
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
  
  (define (read-exprs text)
    (let ([reader (z:read (open-input-string text) 
                          (z:make-location 1 1 0 "stepper-string"))])
      (let read-loop ([new-expr (reader)])
        (if (z:eof? new-expr)
            ()
            (cons new-expr (read-loop (reader)))))))

  ; the closure record is placed in the closure table

  (define (make-closure-record a b)
    (list a b))
  
  (define closure-record-name car)
  (define closure-record-mark cadr)
  
  ; bogus-varref is used so that we can create legal zodiac varrefs for temporary variables
  
  (define (create-bogus-bound-varref name)
    (z:make-bound-varref #f #f #f #f name #f))
  
  (define (create-bogus-top-level-varref name)
    (z:make-top-level-varref #f #f #f #f name))

  ; gensyms needed by both the annotater and the reconstructor:
  
  ; *unevaluated* is the value assigned to temps before they are evaluated.
  (define *unevaluated* (gensym "unevaluated-"))
 
  ; if-temp : uninterned-symbol
  (define if-temp (gensym "if-temp-"))

  ; struct-flag : uninterned symbol
  (define struct-flag (gensym "struct-flag-"))

  ; make-gensym-source creates a pool of gensyms, indexed by arbitrary keys. These gensyms
  ; not eq? to any other symbols, but a client can always get the same symbol by
  ; invoking the resulting procedure with the same key (numbers work well). make-gensym-source
  ; also takes a string which will be part of the printed representation of the symbol;
  ; this makes debugging easier.
  ; make-gensym-source : (string -> (key -> symbol))
  
  (define (make-gensym-source id-string)
    (let ([assoc-table (make-hash-table)])
      (lambda (key)
        (let ([maybe-fetch (hash-table-get assoc-table key (lambda () #f))])
          (or maybe-fetch
             (begin
               (let ([new-sym (gensym (string-append id-string (format "~a" key) "-"))])
                 (hash-table-put! assoc-table key new-sym)
                 new-sym)))))))
  
  ; get-arg-symbol maintains a list of gensyms associated with the non-negative
  ; integers.  These symbols are used in the elaboration of applications; the nth
  ; in the application is evaluated and stored in a variable whose name is the nth
  ; gensym supplied by get-arg-symbol.
  
  (define get-arg-symbol
    (let ([gensym-source (make-gensym-source "arg")])
      (lambda (arg-num)
        (create-bogus-bound-varref (gensym-source arg-num)))))
  
  ; top-level-exp-gensym-source hands out gensyms for the expressions which are not top-level
  ; defines. these expressions' results are bound to variables named by these gensyms. Note that 
  ; this implementation depends on putting exprs in hash tables and thus on non-copying
  ; garbage collection.
  
  (define top-level-exp-gensym-source
    (make-gensym-source "top-level-exp"))

  ; test cases: (returns #t on success)
  #| (let ([arg3 (get-arg-symbol 3)]
        [arg2 (get-arg-symbol 2)]
        [arg1 (get-arg-symbol 1)]
        [arg2p (get-arg-symbol 2)])
    (and (not (eq? arg3 arg2))
         (not (eq? arg3 arg1))
         (not (eq? arg3 arg2p))
         (not (eq? arg2 arg1))
         (eq? arg2 arg2p)
         (not (eq? arg1 arg2p))))
  |#
  

  ; to perform source correlation, we use the 'register-client' ability of zodiac to
  ; add fields to parsed structures at runtime.
  
  (define-values (expr-read set-expr-read!)
    (let-values ([(getter setter) 
                  (z:register-client 'stepper:read (lambda () #f))])
      (values
       (lambda (parsed) (getter (z:parsed-back parsed)))
       (lambda (parsed read) (setter (z:parsed-back parsed) read)))))
  
 
  (define (list-take n a-list)
    (if (= n 0)
        null
        (cons (car a-list) (list-take (- n 1) (cdr a-list)))))
  
  (define (flatten-take n a-list)
    (apply append (list-take n a-list)))
  
  (define make-improper
    (lambda (combine)
      (rec improper ;; `rec' is for the name in error messages
	   (lambda (f list)
	     (let improper-loop ([list list])
	       (cond
		 ((null? list) list)
		 ((pair? list) (combine (f (car list))
					(improper-loop (cdr list))))
		 (else (f list))))))))
  (define improper-map (make-improper cons))
  (define improper-foreach (make-improper (lambda (x y) y)))
  
) 