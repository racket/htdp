
(module teachhelp mzscheme

  (provide make-undefined-check)
  
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
		   tmp-id))]))))))

