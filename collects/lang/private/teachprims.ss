
(module teachprims mzscheme
  
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

  (define-teach beginner list?
    (lambda (x)
      (or (null? x) (pair? x))))
		    
  ;; Don't need this anymore, since we just check for pairs:
  '(define cyclic-list?
     (lambda (l)
       (or (list? l)
	   (and (pair? l)
		(let loop ([hare (cdr l)][turtle l])
		  (cond
		   [(eq? hare turtle) #t]
		   [(not (pair? hare)) #f]
		   [(eq? (cdr hare) turtle) #t]
		   [(not (pair? (cdr hare))) #f]
		   [else (loop (cddr hare) (cdr turtle))]))))))

  (define cyclic-list? beginner-list?)

  (define (build-arg-list args)
    (let loop ([args args][n 0])
      (cond
       [(null? args) ""]
       [(= n 5) " ..."]
       [else
	(format " ~e~a" (car args) (loop (cdr args) (add1 n)))])))

  (define (mk-check-second ok? type)
    (lambda (prim-name a b)
      (unless (ok? b)
	(raise
	 (make-exn:application:type
	  (format "~a: second argument must be of type <~a>, given ~e and ~e"
		  prim-name type
		  a b)
	  (current-continuation-marks)
	  b
	  'list)))))

  (define check-second 
    (mk-check-second beginner-list? "list"))

  (define check-second/cycle
    (mk-check-second cyclic-list? "list or cyclic list"))

  (define (mk-check-last ok? type)
    (lambda (prim-name args)
      (let loop ([l args])
	(cond
	 [(null? l) (void)]
	 [(null? (cdr l))
	  (let ([last (car l)])
	    (unless (ok? last)
	      (raise
	       (make-exn:application:type
		(format "~a: last argument must be of type <~a>, given ~e; other args:~a"
			prim-name type
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

  (define check-last 
    (mk-check-last beginner-list? "list"))

  (define check-last/cycle
    (mk-check-last cyclic-list? "list or cyclic list"))

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
  
  (define-teach beginner list*
    (lambda x
      (check-last 'list* x)
      (apply list* x)))
  
  (define-teach beginner append
    (lambda x
      (check-last 'append x)
      (apply append x)))
  
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

  (define-teach beginner struct?
    (lambda (x)
      (not (or (number? x)
	       (boolean? x)
	       (pair? x)
	       (symbol? x)
	       (string? x)
	       (procedure? x)
	       (vector? x)
	       (char? x)
	       (port? x)
	       (eof-object? x)
	       (void? x)))))

  (define-teach beginner exit
    (lambda () (exit)))

  (define-teach advanced cons 
    (lambda (a b)
      (check-second/cycle 'cons a b)
      (cons a b)))
  
  (define-teach advanced set-cdr!
    (lambda (a b)
      (check-second/cycle 'set-cdr! a b)
      (set-cdr! a b)))
  
  (define-teach advanced set-rest!
    (lambda (a b)
      (check-second/cycle 'set-rest! a b)
      (set-cdr! a b)))
  
  (define-teach advanced list*
    (lambda x
      (check-last/cycle 'list* x)
      (apply list* x)))
  
  (define-teach advanced append
    (lambda x
      (check-last/cycle 'append x)
      (apply append x)))
  
  (define-teach advanced append!
    (lambda x
      (check-last/cycle 'append! x)
      (apply append! x)))

  (provide beginner-+
	   beginner-/
	   beginner-*
	   beginner-list?
	   beginner-cons
	   beginner-list*
	   beginner-append
	   beginner-error
	   beginner-struct?
	   beginner-exit
	   advanced-cons
	   advanced-set-cdr!
	   advanced-set-rest!
	   advanced-list*
	   advanced-append
	   advanced-append!
	   cyclic-list?))
