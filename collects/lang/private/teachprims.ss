
(module teachprims mzscheme

  (require "../imageeq.ss")
  
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
	  prim-name)))))

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

  (define (check-three a b c prim-name ok1? 1type ok2? 2type ok3? 3type)
    (let ([bad
	   (lambda (v which type)
	     (raise
	      (make-exn:application:type
	       (format "~a: ~a argument must be of type <~a>, given ~e, ~e, and ~e"
		       prim-name which type
		       a b c)
	       (current-continuation-marks)
	       v
	       prim-name)))])
      (unless (ok1? a)
	(bad a "first" 1type))
      (unless (ok2? b)
	(bad b "second" 2type))
      (unless (ok3? c)
	(bad c "second" 3type))))

  (define (positive-real? v)
    (and (real? v) (>= v 0)))

  (define-teach beginner not
    (lambda (a)
      (unless (boolean? a)
	(raise
	 (make-exn:application:type
	  (format "not: expected either true or false; given ~e"
		  a)
	  (current-continuation-marks)
	  a
	  'not)))
      (not a)))

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
  
  (define-teach beginner equal?
    (lambda (a b)
      (if (image? a)
	  (and (image? b)
	       (image=? a b))
	  (equal? a b))))

  (define-teach beginner =~
    (lambda (a b c)
      (check-three a b c '=~ real? 'real real? 'real positive-real? 'non-negative-real)
      (<= (- a c) b (+ a c))))

  (define-teach beginner equal~?
    (lambda (a b c)
      (check-three a b c 'equal~? values 'any values 'any positive-real? 'non-negative-real)
      (if (and (real? a)
	       (real? b))
	  (<= (- a c) b (+ a c))
	  (beginner-equal? a b))))

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

  (provide beginner-not
	   beginner-+
	   beginner-/
	   beginner-*
	   beginner-list?
	   beginner-cons
	   beginner-list*
	   beginner-append
	   beginner-error
	   beginner-struct?
	   beginner-exit
	   beginner-equal?
	   beginner-equal~?
	   beginner-=~
	   advanced-cons
	   advanced-set-cdr!
	   advanced-set-rest!
	   advanced-list*
	   advanced-append
	   advanced-append!
	   cyclic-list?))
