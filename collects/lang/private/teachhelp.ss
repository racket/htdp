
(module teachhelp mzscheme

  (define (teach-syntax-error form stx msg . args)
    (raise-syntax-error
     form
     (apply format msg args)
     stx))

  (define (bad-use-error name stx)
    (teach-syntax-error
     name
     stx
     "found a use of `~a' that does not follow an open parenthesis"
     name))

  (define (something-else v)
    (let ([v (syntax-e v)])
      (cond
       [(number? v) "a number"]
       [(string? v) "a string"]
       [else "something else"])))
  
  (define (ordinal n)
    (cond
     [(or (<= 11 n 13)
	  (zero? (modulo n 10))
	  (<= 4 (modulo n 10) 9))
      (format "~ath" n)]
     [(= 1 (modulo n 10))
      (format "~ast" n)]
     [(= 2 (modulo n 10))
      (format "~and" n)]
     [(= 3 (modulo n 10))
      (format "~ard" n)]))

  (define (make-undefined-check check-proc tmp-id)
    (let ([set!-stx (datum->syntax-object check-proc 'set!)])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (with-syntax ([tmp-id tmp-id])
	      (syntax (set! tmp-id expr)))]
	   [(id . args)
	    (datum->syntax-object
	     check-proc
	     (cons (list check-proc 
			 (list 'quote (syntax id))
			 tmp-id)
		   (syntax args)))]
	   [id
	    (datum->syntax-object
	     check-proc
	     (list check-proc 
		   (list 'quote (syntax id))
		   tmp-id))])))))

  (provide teach-syntax-error
	   bad-use-error
	   something-else
	   ordinal
	   make-undefined-check))
