
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
	      (syntax-property
	       (syntax (set! tmp-id expr))
	       'bound-in-source
	       (syntax-local-introduce
		(syntax id))))]
	   [(id . args)
	    (syntax-property
	     (datum->syntax-object
	      check-proc
	      (cons (list check-proc 
			  (list 'quote (syntax id))
			  tmp-id)
		    (syntax args)))
	     'bound-in-source
	     (syntax-local-introduce
	      (syntax id)))]
	   [id
	    (syntax-property
	     (datum->syntax-object
	      check-proc
	      (list check-proc 
		    (list 'quote (syntax id))
		    tmp-id))
	     'bound-in-source
	     (syntax-local-introduce
	      (syntax id)))]))))))


