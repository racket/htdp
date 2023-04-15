#lang racket/base

; Once upon a time, there were three different variants.  Preserve the
; ability to do this.
(provide beginner-module-begin
         beginner-abbr-module-begin
         intermediate-module-begin
         intermediate-lambda-module-begin
         advanced-module-begin)

(require deinprogramm/signature/signature
         lang/private/signature-syntax
         (only-in test-engine/syntax test))

(require (for-syntax scheme/base)
         (for-syntax racket/list)
         (for-syntax syntax/boundmap)
         (for-syntax syntax/kerncase))

(require (for-syntax "firstorder.rkt"))

(define-syntax (print-results stx)
  (syntax-case stx ()
    ((_ expr)
     (not (syntax-property #'expr 'test-call))
     (syntax-property
      (syntax-property
       #'(#%app call-with-values (lambda () expr)
		do-print-results)
       'stepper-skipto 
       '(syntax-e cdr cdr car syntax-e cdr cdr car))
      'certify-mode
      'transparent))
    ((_ expr) #'expr)))

(define (do-print-results . vs)
  (for-each (current-print) vs)
  ;; Returning 0 values avoids any further result printing
  ;; (even if void values are printed)
  (values))
  
(define-syntaxes (beginner-module-begin
                  beginner-abbr-module-begin
                  intermediate-module-begin
                  intermediate-lambda-module-begin
                  advanced-module-begin
                  module-continue)
  (let ()
    ;; takes a list of syntax objects (the result of syntax-e) and returns all the syntax objects that correspond to
    ;; a signature declaration. Syntax: (: id signature)
    (define extract-signatures
      (lambda (lostx) 
	(let* ((table (make-bound-identifier-mapping))
               (ids '())
	       (non-signatures
		(filter-map (lambda (maybe)
			      (syntax-case maybe (:)
				((: ?exp ?sig)
				 (not (identifier? #'?exp))
				 #'(apply-signature/blame (signature ?sig) ?exp))
				((: ?id ?sig)
                                 (let ((real-id (first-order->higher-order #'?id)))
                                   (cond
                                     ((bound-identifier-mapping-get table real-id (lambda () #f))
                                      => (lambda (old-sig-stx)
                                           (unless (equal? (syntax->datum old-sig-stx)
                                                           (syntax->datum #'?sig))
                                             (raise-syntax-error #f
                                                                 "Second signature declaration for the same name."
                                                                 maybe))))
                                     (else
                                      (bound-identifier-mapping-put! table real-id #'?sig)))
                                   (set! ids (cons real-id ids))
                                   #f))
				((: ?id)
				 (raise-syntax-error #f "Signature declaration is missing a signature." maybe))
				((: ?id ?sig ?stuff0 ?stuff1 ...)
				 (raise-syntax-error #f "The : form expects a name and a signature; there is more."
						     (syntax/loc #'?stuff0
								 (?stuff0 ?stuff1 ...))))
				(_ maybe)))
			    lostx)))
	  (values table ids non-signatures))))

    (define local-expand-stop-list 
      (append (list #': #'define-signature)
	      (kernel-form-identifier-list)))
	
    (define (expand-signature-expressions signature-table expressions)

      (let loop ((exprs expressions))

	(cond
	 ((null? exprs)
	  (bound-identifier-mapping-for-each signature-table
					     (lambda (id thing)
					       (when thing
						 (if (identifier-binding id)
						     (raise-syntax-error #f "Cannot declare a signature for a built-in form." id)
						     (raise-syntax-error #f "There is no definition for this signature declaration." id)))))
	  #'(begin))
	 (else
	  (let ((expanded (car exprs)))

	    (syntax-case expanded (begin define-values)
	      ((define-values (?id ...) ?e1)
	       (with-syntax (((?enforced ...)
			      (map (lambda (id)
				     (cond
				      ((bound-identifier-mapping-get signature-table id (lambda () #f))
				       => (lambda (sig)
					    (bound-identifier-mapping-put! signature-table id #f) ; check for orphaned signatures
					    (with-syntax ((?id id)
							  (?sig sig))
					      #'(?id (signature ?sig)))))
				      (else
				       id)))
				   (syntax->list #'(?id ...))))
			     (?rest (loop (cdr exprs))))
		 (with-syntax ((?defn
				(syntax-track-origin
				 #'(define-values/signature (?enforced ...)
				     ?e1)
				 (car exprs)
				 (car (syntax-e expanded)))))

		   (syntax/loc (car exprs)
			       (begin
				 ?defn
				 ?rest)))))
	      ((begin e1 ...)
	       (loop (append (syntax-e (syntax (e1 ...))) (cdr exprs))))
	      (else 
	       (with-syntax ((?first expanded)
			     (?rest (loop (cdr exprs))))
		 (syntax/loc (car exprs)
			     (begin
			       ?first ?rest))))))))))

    ; these dummy uses are so that DrRacket can display binding arrows correctly
    (define (signature-id-dummy-uses ids)
      #`(begin
          #,@(map (lambda (id)
                    (syntax-property #`(begin #,id (void))
                                     'stepper-skip-completely 
                                     #t))
                  ids)))
    
    (define (mk-module-begin options)
      (lambda (stx)
        (syntax-case stx ()
          ((_ e1 ...)
           ;; module-begin-continue takes a sequence of expanded
           ;; exprs and a sequence of to-expand exprs; that way,
           ;; the module-expansion machinery can be used to handle
           ;; requires, etc.:
           #`(#%plain-module-begin
              (module-continue (e1 ...) () ())
              (module configure-runtime racket/base
                (require htdp/bsl/runtime)
                (configure '#,options))
              (module+ test (test)))))))
    
    (values
     ;; module-begin
     (mk-module-begin '(output-function-instead-of-lambda))
     (mk-module-begin '(abbreviate-cons-as-list
                        read-accept-quasiquote
                        output-function-instead-of-lambda))
     (mk-module-begin '(abbreviate-cons-as-list
                        read-accept-quasiquote
                        output-function-instead-of-lambda))
     (mk-module-begin '(abbreviate-cons-as-list
                        read-accept-quasiquote))
     (mk-module-begin '(abbreviate-cons-as-list
                        read-accept-quasiquote
                        show-sharing))

     ;; module-continue
     (lambda (stx)
       (syntax-case stx ()
	 ((_ () (e1 ...) (defined-id ...))
	  ;; Local-expanded all body elements, lifted out requires, etc.
	  ;; Now process the result.
	  (begin
	    ;; The expansion for signatures breaks the way that beginner-define, etc.,
	    ;;  check for duplicate definitions, so we have to re-check here.
	    ;; A better strategy might be to turn every define into a define-syntax
	    ;;  to redirect the binding, and then the identifier-binding check in
	    ;;  beginner-define, etc. will work.
	    (let ((defined-ids (make-bound-identifier-mapping)))
	      (for-each (lambda (id)
			  (when (bound-identifier-mapping-get defined-ids id (lambda () #f))
			    (raise-syntax-error
			     #f
			     "this name was defined previously and cannot be re-defined"
			     id))
			  (bound-identifier-mapping-put! defined-ids id #t))
			(reverse (syntax->list #'(defined-id ...)))))
	    ;; Now handle signatures:
	    (let ((top-level (reverse (syntax->list (syntax (e1 ...))))))
	      (let-values (((sig-table ids expr-list)
			    (extract-signatures top-level)))
		#`(begin
                    #,(expand-signature-expressions sig-table expr-list)
                    #,(signature-id-dummy-uses ids))))))
	 ((frm e3s e1s def-ids)
	  (let loop ((e3s #'e3s)
		     (e1s #'e1s)
		     (def-ids #'def-ids))
	    (syntax-case e3s ()
	      (()
	       #`(frm () #,e1s #,def-ids))
	      ((e2 . e3s)
	       (let ((e2 (local-expand #'e2 'module local-expand-stop-list)))
		 ;; Lift out certain forms to make them visible to the module
		 ;;  expander:
		 (syntax-case e2 (#%require #%provide #%declare
				  define-syntaxes begin-for-syntax define-values begin
				  define-signature :)
		   ((#%require . __)
		    #`(begin #,e2 (frm e3s #,e1s #,def-ids)))
		   ((#%provide . __)
		    #`(begin #,e2 (frm e3s #,e1s #,def-ids)))
		   ((#%declare . __)
		    #`(begin #,e2 (frm e3s #,e1s #,def-ids)))
		   ((define-syntaxes (id ...) . _)
		    #`(begin #,e2 (frm e3s #,e1s (id ... . #,def-ids))))
		   ((begin-for-syntax . _)
		    #`(begin #,e2 (frm e3s #,e1s #,def-ids)))
		   ((begin b1 ...)
		    (syntax-track-origin 
		     (loop (append (syntax->list #'(b1 ...)) #'e3s) e1s def-ids)
		     e2
		     (car (syntax-e e2))))
		   ((define-values (id ...) . _)
		    (loop #'e3s (cons e2 e1s) (append (syntax->list #'(id ...)) def-ids)))
		   ((define-signature id ctr)
		    (loop #'e3s (cons e2 e1s) def-ids))
		   ((: stuff ...)
		    (loop #'e3s (cons e2 e1s) def-ids))
		   (_
		    (loop #'e3s (cons #`(print-results #,e2) e1s) def-ids)))))))))))))
