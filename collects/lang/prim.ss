
;; Provides `define-primitive' and `define-higher-order-primitive'
;; for use in teachpacks for Beginner, especially those that
;; define a primitive operator that consumes a procedure.
;; See doc.txt for more information.

(module prim mzscheme
  (provide define-primitive
	   define-higher-order-primitive)

  (define-syntax (define-primitive stx)
    (syntax-case stx ()
      [(_ name implementation)
       #'(define-syntax (name stx)
	   (syntax-case stx ()
	     [(id . args)
	      (syntax/loc stx (#%app implementation . args))]
	     [id
	      ;; HACK: we disable all checks if #%top is 
	      ;; the usual one, which indicates that we're
	      ;; not in beginner
	      (module-identifier=? #'#%top (datum->syntax-object stx '#%top))
	      (syntax/loc stx implementation)]
	     [_else
	      (raise-syntax-error
	       #f
	       (string-append
		"this primitive operator must be applied to arguments; "
		"expected an open parenthesis before the operator name")
	       stx)]))]))

  (define-syntax (define-higher-order-primitive stx)
    (define (is-proc-arg? arg)
      (eq? '_ (syntax-e arg)))
    (syntax-case stx ()
      [(_ name implementation (arg ...))
       (let* ([args (syntax->list (syntax (arg ...)))]
	      [new-args (generate-temporaries args)])
         (for-each (lambda (id)
                     (unless (identifier? id)
                       (raise-syntax-error #f "not an identifier" stx id)))
                   (cons (syntax name)
                         args))
         (with-syntax ([(new-arg ...) new-args]
		       [(checks ...)
                        (map (lambda (arg new-arg)
                               (if (not (is-proc-arg? arg))
                                   #'(void)
                                   #`(unless (and (identifier? (#,#'syntax #,new-arg))
                                                  (or
                                                   (not (identifier-binding (#,#'syntax #,new-arg)))
						   ;; HACK - see note above
                                                   (module-identifier=? #'#%top 
                                                                        (datum->syntax-object 
                                                                         (#,#'syntax #,new-arg)
                                                                         '#%top))))
                                       (raise-syntax-error
                                        #f
                                        (format
                                         "primitive operator ~a expects a program name (usually `~a') in this position"
					 'name
                                         '#,arg)
                                        s
                                        (#,#'syntax #,new-arg)))))
                             args new-args)]
                       [(wrapped-arg ...)
                        (map (lambda (arg new-arg)
                               (if (is-proc-arg? arg)
                                   new-arg
				   #`(#%top . #,new-arg)))
                             args new-args)]
                       [num-arguments (length args)])
           (syntax/loc
            stx
            (define-syntax (name s)
              (syntax-case s ()
                [(__ new-arg ...)
                 (begin
                   checks ...
		   ;; s is a well-formed use of the primitive;
		   ;; generate the primitive implementation
                   (let ([k (quote-syntax
			     (implementation wrapped-arg ...))])
		     (datum->syntax-object
		      k
		      (syntax-e k)
		      s)))]
		[(__ . rest)
                 (raise-syntax-error
                  #f
                  (format
                   "primitive operator requires ~a arguments"
                   num-arguments)
                  s)]
                [_else
                 (raise-syntax-error
                  #f
                  (string-append
                   "this primitive operator must be applied to arguments; "
                   "expected an open parenthesis before the operator name")
                  s)])))))])))

