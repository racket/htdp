
(module teachprims mzscheme
  
  (define (build-arg-list args)
    (let loop ([args args][n 0])
      (cond
       [(null? args) ""]
       [(= n 5) " ..."]
       [else
	(format " ~e~a" (car args) (loop (cdr args) (add1 n)))])))

  (define check-second 
    (lambda (prim-name a b)
      (unless (list? b)
	(raise
	 (make-exn:application:type
	  (format "~a: second argument must be of type <list>, given ~e and ~e"
		  prim-name
		  a b)
	  (current-continuation-marks)
	  b
	  'list)))))
  
  (define check-last
    (lambda (prim-name args)
      (let loop ([l args])
	(cond
	 [(null? l) (void)]
	 [(null? (cdr l))
	  (let ([last (car l)])
	    (unless (list? last)
	      (raise
	       (make-exn:application:type
		(format "~a: last argument must be of type <list>, given ~e; other args:~a"
			prim-name
			last
			(build-arg-list
			 (let loop ([args args])
			   (cond
			    [(null? (cdr args)) null]
			    [else (cons (car args) (loop (cdr args)))]))))
		(current-continuation-marks)
		last
		'list))))]
	 [else (loop (cdr l))]))))

  (define-syntax (define-teach stx)
    (syntax-case stx ()
      [(_ level id expr)
       (with-syntax ([level-id (datum->syntax-object
				(syntax id)
				(string->symbol
				 (format "~a-~a"
					 (syntax-object->datum (syntax level))
					 (syntax-object->datum (syntax id))))
				(syntax id))])
	 (syntax (define level-id
		   (let ([id expr])
		     id))))]))
		    
  (define-teach beginner +
    (lambda (a b . args)
      (apply + a b args)))

  (define-teach beginner /
    (lambda (a b . args)
      (apply / a b args)))
  
  (define-teach beginner *
    (lambda (a b . args)
      (apply * a b args)))

  (define-teach beginner cons 
    (lambda (a b)
      (check-second 'cons a b)
      (cons a b)))
  
  (define-teach advanced set-cdr!
    (lambda (a b)
      (check-second 'set-cdr! a b)
      (set-cdr! a b)))
  
  (define-teach beginner list*
    (lambda x
      (check-last 'list* x)
      (apply list* x)))
  
  (define-teach beginner append
    (lambda x
      (check-last 'append x)
      (apply append x)))
  
  (define-teach advanced append!
    (lambda x
      (check-last 'append! x)
      (apply append! x)))

  (define-teach beginner error
    (lambda (sym str)
      (unless (and (symbol? sym)
		   (string? str))
	(raise
	 (make-exn:application:type
	  (format "error: expected a symbol and a string, got ~e and ~e"
		  sym str)
	  (current-continuation-marks)
	  (if (symbol? sym) str sym)
	  (if (symbol? sym) 'string 'symbol))))
      (error sym "~a" str)))

  (provide beginner-+
	   beginner-/
	   beginner-*
	   beginner-cons
	   advanced-set-cdr!
	   beginner-list*
	   beginner-append
	   advanced-append!
	   beginner-error))
