
;; Implements the Teaching languages, at least in terms of the
;; forms and procedures. The reader-level aspects of the language
;; (e.g., case-sensitivity) are not implemented here.

;; Error-message conventions:
;;  - report errors, somewhat anthopomorphicly, in terms of "expected"
;;    versus "found" syntax
;;  - report errors according to a left-to-right reading; e.g., the
;;    error in `(define 1 2 3)' is "expected an identifier, found 1",
;;    not "expected two parts after `define', but found three"

;; Possible bug: keywords are not disallowed as function names or
;; arguments. (This is a "possible" bug because it's not clear we want
;; to maintain the restriction.)

(module teach mzscheme
  (require (lib "etc.ss")
	   (lib "list.ss")
	   (lib "math.ss"))
  (require-for-syntax "teachhelp.ss"
		      (lib "kerncase.ss" "syntax"))

  ;; syntax:
  (provide beginner-define
	   beginner-define-struct
	   beginner-lambda
	   beginner-app
	   beginner-cond
	   beginner-if
	   beginner-and
	   beginner-or
	   beginner-quote
	   
	   intermediate-local
	   intermediate-letrec
	   intermediate-let
	   intermediate-time

	   advanced-define
	   advanced-lambda
	   advanced-define-struct
	   advanced-let
	   advanced-recur)
  
  (define-struct posn (x y) (make-inspector)) ; transparent
  (provide (struct posn (x y)))

  ;; verify-boolean is inserted to check for boolean results:
  (define (verify-boolean b where)
    (if (or (eq? b #t) (eq? b #f))
	b
	(raise
	 (make-exn:application:type
	  (format "~a: question result is not true or false: ~e" where b)
	  (current-continuation-marks)
	  b
	  'boolean))))

  (define undefined (letrec ([x x]) x))

  (define (check-not-undefined name val)
    (if (eq? val undefined)
	(raise
	 (make-exn:variable
	  (format "local variable used before its definition: ~a" name)
	  (current-continuation-marks)
	  name))
	val))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; define (beginner)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (beginner-define stx)

    (define (check-single-result-expr exprs where enclosing-expr)
      (check-single-expression where
			       (format "within ~a" where)
			       enclosing-expr
			       exprs))

    (unless (or (memq (syntax-local-context) '(top-level module))
		(identifier? stx))
      (teach-syntax-error
       'define
       stx
       "found a `define' expression that is embedded in an expression, ~
        but all `define' expressions must be at the top level"))
      
    (syntax-case stx ()
      ;; Constant or lambda def:
      [(_ name expr)
       (identifier? (syntax name))
       (syntax-case (syntax expr) (beginner-lambda)
	 ;; Possibly well-formed lambda def:
	 [(beginner-lambda arg-seq lexpr ...)
	  (syntax-case (syntax arg-seq) () [(arg ...) #t][_else #f])
	  (let ([args (syntax->list (syntax arg-seq))])
	    (for-each (lambda (arg)
			(unless (identifier? arg)
			  (teach-syntax-error
			   'lambda
			   arg
			   "expected a name for a function argument, but found ~a"
			   (something-else arg))))
		      args)
	    (when (null? args)
	      (teach-syntax-error
	       'lambda
	       (syntax arg-seq)
	       "expected at least one argument name in the sequence after `lambda', but found none"))
	    (check-single-result-expr (syntax->list (syntax (lexpr ...)))
				      'lambda
				      (syntax expr))
	    (syntax/loc stx (define (name . arg-seq) lexpr ...)))]
	 ;; Bad lambda because bad args:
	 [(beginner-lambda args . _)
	  (teach-syntax-error
	   'lambda
	   (syntax args)
	   "expected a sequence of function arguments after `lambda', but found ~a"
	   (something-else (syntax args)))]
	 ;; Bad lambda, no args:
	 [(beginner-lambda)
	  (teach-syntax-error
	   'lambda
	   (syntax args)
	   "expected a sequence of function arguments after `lambda', but nothing's there")]
	 ;; Constant def
	 [_else
	  (syntax (define name expr))])]
      ;; Function definition:
      [(_ name-seq expr ...)
       (syntax-case (syntax name-seq) () [(name ...) #t][_else #f])
       ;; name-seq is at least a sequence
       (let ([names (syntax->list (syntax name-seq))])
	 (when (null? names)
	   (teach-syntax-error
	    'define
	    names
	    "expected a function name for a definition, but the name is missing"))
	 (let loop ([names names][pos 0])
	   (unless (null? names)
	     (unless (identifier? (car names))
	       (teach-syntax-error
		'define
		(car names)
		"expected a name for ~a, but found ~a"
		(cond
		 [(zero? pos) "a defined function"]
		 [else (format "the ~a argument" (ordinal pos))])
		(something-else (car names))))
	     (loop (cdr names) (add1 pos))))
	  (when (null? (cdr names))
	    (teach-syntax-error
	     'define
	     (syntax name-seq)
	     "expected at least one argument name after the function name, but found none"))
	  (check-single-result-expr (syntax->list (syntax (expr ...)))
				    'define
				    stx)
	  (syntax/loc stx (define name-seq expr ...)))]
      ;; Constant/lambda with too many or too few parts:
      [(_ name expr ...)
       (identifier? (syntax name))
       (let ([exprs (syntax->list (syntax (expr ...)))])
	 (check-single-expression 'define
				  (format "after the defined name ~a"
					  (syntax-e (syntax name)))
				  stx
				  exprs))]
      ;; Bad name/header:
      [(_ non-name expr ...)
       (teach-syntax-error
	'define
	(syntax non-name)
	"expected a function name, constant name, or function header for `define', ~
         but found ~a"
	(something-else (syntax non-name)))]
      ;; Missing name:
      [(_)
       (teach-syntax-error
	'define
	stx
	"expected a function name, constant name, or function header after `define', ~
         but nothing's there")]
      [_else
       (bad-use-error 'define stx)]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; lambda (beginner; only works with define)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (beginner-lambda stx)
    (syntax-case stx ()
      [(_ . rest)
       (teach-syntax-error
	'lambda
	stx
	"found a `lambda' expression that is not a function definition")]
      [_else
       (bad-use-error 'lambda stx)]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; define-struct (beginner)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-syntax (beginner-define-struct stx)
    (syntax-case stx ()
      [(_ name . __)
       (not (identifier? (syntax name)))
       (teach-syntax-error
	'define-struct
	(syntax name)
	"expected a structure type name after `define-syntax', but found ~a"
	(something-else (syntax name)))]
      [(_ name_ (field_ ...) . rest)
       (let ([name (syntax name_)]
	     [fields (syntax->list (syntax (field_ ...)))])
	 (for-each
	  (lambda (field)
	    (unless (identifier? field)
	      (teach-syntax-error
	       'define-struct
	       stx
	       "expected a structure field name, found ~a"
	       (something-else field))))
	  fields)
	 (let ([rest (syntax->list (syntax rest))])
	   (unless (null? rest)
	     (teach-syntax-error
	      'define-struct
	      (car rest)
	      "expected nothing after the field name sequence in `define-struct', ~
               but found an extra expression~a"
	      (if (null? (cdr rest))
		  ""
		  (format " (plus ~a more)" (length (cdr rest)))))))
	 (with-syntax ([(to-define-name ...)
			(let ([n (symbol->string (syntax-e name))]
			      [+ string-append])
			  (map (lambda (s)
				 (datum->syntax-object name (string->symbol s) name))
			       (append
				(list 
				 (+ "make-" n)
				 (+ n "?"))
				(map
				 (lambda (f) 
				   (+ n "-" (symbol->string (syntax-e f))))
				 fields))))])
	   (syntax (define-values (to-define-name ...)
		     (let ()
		       (define-struct name_ (field_ ...))
		       (values to-define-name ...))))))]
      [(_ name_ something . rest)
       (teach-syntax-error
	'define-struct
	(syntax something)
	"expected a sequence of field name after the structure type name in `define-struct', ~
         but found ~a"
	(something-else (syntax something)))]
      [(_)
       (teach-syntax-error
	'define-struct
	stx
	"expected a structure type name after `define-syntax', but nothing's there")]
      [_else (bad-use-error 'define-struct stx)]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; application (beginner)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; This application form disallows rator expressions that aren't
  ;; top-level identifiers or of the form `(check-not-undefined ...)'.

  ;; The latter is probably surprising. It turns out that every use of
  ;; a `local'-bound identifier gets converted to an undefined check,
  ;; and the call to `check-not-udnefined' can't be forged by the
  ;; programmer. So the pattern-match effectively recognizes uses of
  ;; `local'-bound identifiers, which are legal as rator
  ;; expressions. (`let' and `letrec' get converted to `local'.)

  (define-syntax (beginner-app stx)
    (syntax-case stx ()
      [(_ rator rand ...)
       (let* ([fun (syntax rator)]
	      [undef-check? (syntax-case fun (check-not-undefined)
			      [(check-not-undefined id)
			       #t]
			      [_else #f])]
	      [lex? (and (identifier? fun)
			 (eq? 'lexical (identifier-binding fun)))])
	 (unless (and (identifier? fun) (or undef-check? (not lex?)))
	   (teach-syntax-error
	    'application
	    fun
	    "expected a defined name or a primitive operation name after an ~
             open parenthesis, but found ~a"
	    (if lex?
		"a function argument name"
		(something-else fun))))
	 (when (null? (syntax->list (syntax (rand ...))))
	   (teach-syntax-error
	    'application
	    stx
	    "expected an argument after the function name for a function call, but nothing's there"))
	 (syntax (#%app rator rand ...)))]
      [(_)
       (teach-syntax-error
	'application
	stx
	"expected a defined name or a primitive operation name after after an ~
         open parenthesis, but nothing's there")]
      [_else (bad-use-error '#%app stx)]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; cond
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (beginner-cond stx)
    (syntax-case stx ()
      [(_)
       (teach-syntax-error
	'cond
	stx
	"expected a question-answer clause after `cond', but nothing's there")]
      [(_ clause ...)
       (let ([clauses (syntax->list (syntax (clause ...)))])
	 (let ([checked-clauses
		(map
		 (lambda (clause)
		   (syntax-case clause (else)
		     [(else answer)
		      (let ([lpos (memq clause clauses)])
			(when (not (null? (cdr lpos)))
			  (teach-syntax-error
			   'cond
			   clause
			   "found an `else' clause that isn't the last clause ~
                                    in its `cond' expression"))
			(syntax (else answer)))]
		     [(question answer)
		      (syntax ((verify-boolean question 'cond) answer))]
		     [()
		      (teach-syntax-error
		       'cond
		       clause
		       "expected a question-answer clause, but found an empty clause")]
		     [(question?)
		      (teach-syntax-error
		       'cond
		       clause
		       "expected a clause with a question and answer, but found a clause ~
                                with only one part")]
		     [(question? answer? ...)
		      (teach-syntax-error
		       'cond
		       clause
		       "expected a clause with one question and one answer, but found a clause ~
                                with ~a parts"
		       (length (syntax->list clause)))]
		     [_else
		      (teach-syntax-error
		       'cond
		       clause
		       "expected a question-answer clause, but found ~a"
		       (something-else clause))]))
		 clauses)])
	   ;; Add `else' clause for error, if necessary:
	   (let ([clauses (let loop ([clauses clauses])
			    (cond
			     [(null? clauses)
			      (list
			       (syntax/loc stx
				   [else (error 'cond "all question results were false")]))]
			     [(syntax-case (car clauses) (else)
				[(else . _) #t]
				[_else #f])
			      clauses]
			     [else (cons (car clauses) (loop (cdr clauses)))]))])
	     (with-syntax ([clauses clauses])
	       (syntax (cond . clauses))))))]
      [_else (bad-use-error 'cond stx)]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; if
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (beginner-if stx)
    (syntax-case stx ()
      [(_ test then else)
       (syntax (if (verify-boolean test 'if)
		   then
		   else))]
      [(_ . rest)
       (let ([n (length (syntax->list (syntax rest)))])
	 (teach-syntax-error
	  'if
	  stx
	  "expected one question expression and two answer expressions, but found ~a expression~a"
	  (if (zero? n) "no" n)
	  (if (= n 1) "" "s")))]
      [_else (bad-use-error 'if stx)]))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; or, and
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntaxes (beginner-or beginner-and)
    (let ([mk
	   (lambda (where)
	     (with-syntax ([swhere where])
	       (lambda (stx)
		 (syntax-case stx ()
		   [(_ a ...)
		    (let ([n (length (syntax->list (syntax (a ...))))])
		      (when (n . < . 2)
			(teach-syntax-error
			 where
			 stx
			 "expected at least two expressions after `~a', but found ~a"
			 where
			 (if (zero? n) "no expressions" "only one expression")))
		      (syntax (swhere (verify-boolean a 'swhere) ...)))]
		   [_else (bad-use-error where stx)]))))])
      (values (mk 'or) (mk 'and))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; quote (symbols)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (beginner-quote stx)
    (syntax-case stx ()
      [(_ expr)
       (let ([sym (syntax expr)])
	 (unless (identifier? sym)
	   (teach-syntax-error
	    'quote
	    stx
	    "expected a name after a ', found ~a"
	    (something-else sym)))
	 (syntax (quote expr)))]
      [_else (bad-use-error 'quote stx)]))


  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; local
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (intermediate-local stx)
    (syntax-case stx ()
      [(_ (definition ...) . exprs)
       (let ([defns (syntax->list (syntax (definition ...)))])
	 (let ([partly-expanded-defns 
		(map (lambda (d)
		       (local-expand
			d
			'top-level
			(kernel-form-identifier-list (quote-syntax here))))
		     defns)])
	   (let ([local-ids
		  (apply
		   append
		   (map (lambda (partly-expanded orig)
			  (syntax-case partly-expanded (define-values)
			    [(define-values (id ...) expr)
			     (andmap identifier? (syntax->list (syntax (id ...))))
			     (syntax->list (syntax (id ...)))]
			    [_else
			     (teach-syntax-error
			      'local
			      orig
			      "expected only definitions within the definition sequence, but found ~a"
			      (something-else orig))]))
			partly-expanded-defns defns))])
	     (let ([dup (check-duplicate-identifier local-ids)])
	       (when dup
		 (teach-syntax-error
		  'local
		  dup
		  "found a name that was defined locally more than once: ~a"
		  (syntax-e dup))))
	     (let ([exprs (syntax->list (syntax exprs))])
	       (check-single-expression 'local
					"after the local definition sequence"
					stx
					exprs))
	     (with-syntax ([((d-v (def-id ...) def-expr) ...) partly-expanded-defns])
	       (with-syntax ([((tmp-id ...) ...)
			      (map generate-temporaries
				   (syntax->list (syntax ((def-id ...) ...))))])
		 (with-syntax ([mappings
				(apply
				 append
				 (map
				  syntax->list
				  (syntax->list
				   (syntax
				    (([def-id (make-undefined-check 
					       (quote-syntax check-not-undefined)
					       (quote-syntax tmp-id))]
				      ...)
				     ...)))))])
		   (syntax/loc stx
		     (letrec-values ([(tmp-id ...)
				      (let-syntax mappings def-expr)]
				     ...)
		       (let-syntax mappings
			 . exprs)))))))))]
      [(_ def-non-seq . __)
       (teach-syntax-error
	'local
	(syntax def-non-seq)
	"expected a parenthesized definition sequence after `local', but found ~a"
	(something-else (syntax def-non-seq)))]
      [(_)
       (teach-syntax-error
	'local
	stx
	"expected a parenthesized definition sequence after `local', but nothing's there")]
      [_else (bad-use-error 'local stx)]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; letrec and let (intermediate)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (intermediate-letrec stx)
    (syntax-case stx ()
      [(_ ([name rhs-expr] ...) expr)
       (let ([names (syntax->list (syntax (name ...)))])
	 (and (andmap identifier? names)
	      (not (check-duplicate-identifier names))))
       (syntax/loc stx
	 (intermediate-local [(define-values (name) rhs-expr) ...] expr))]
      [_else (with-syntax ([stx stx])
	       (syntax (bad-let-form letrec stx)))]))

  (define-syntax (intermediate-let stx)
    (syntax-case stx ()
      [(_ ([name rhs-expr] ...) expr)
       (let ([names (syntax->list (syntax (name ...)))])
	 (and (andmap identifier? names)
	      (not (check-duplicate-identifier names))))
       (with-syntax ([(tmp ...)
		      (generate-temporaries (syntax (name ...)))])
	 (syntax/loc stx
	   (let ([tmp rhs-expr]
		 ...)
	     ;; Use `local' to tell `#%app' about the bindings:
	     (intermediate-local [(define-values (name ...) (values tmp ...))] 
                expr))))]
      [_else (with-syntax ([stx stx])
	       (syntax (bad-let-form let stx)))]))

  (define-syntax (bad-let-form stx)
    (syntax-case stx ()
      [(_ who stx)
       (let ([who (syntax-e (syntax who))]
	     [stx (syntax stx)])
	 (syntax-case stx ()
	   [(_ (binding ...) . exprs)
	    (let ([bindings (syntax->list (syntax (binding ...)))])
	      (for-each (lambda (binding)
			  (syntax-case binding ()
			    [(name expr)
			     (let ([name (syntax name)])
			       (unless (identifier? name)
				 (teach-syntax-error
				  who
				  name
				  "expected a name for a local binding, but found ~a"
				  (something-else name))))]
			    [(name . exprs)
			     (identifier? (syntax name))
			     (check-single-expression who
						      (format "after the name ~a"
							      (syntax-e (syntax name)))
						      binding
						      (syntax->list (syntax exprs)))]
			    [(something . exprs)
			     (teach-syntax-error
			      who
			      (syntax something)
			      "expected a name after the parenthesis for a ~a local definition, ~
                               but found ~a"
			      who
			      (something-else (syntax something)))]
			    [_else
			     (teach-syntax-error
			      who
			      binding
			      "expected a parenthesized name and expression for a ~a local definition, ~
                               but found ~a"
			      who
			      (something-else binding))]))
			bindings)
	      (let ([dup (check-duplicate-identifier (map (lambda (binding)
							    (syntax-case binding ()
							      [(name . _) (syntax name)]))
							  bindings))])
		(when dup
		  (teach-syntax-error
		   who
		   dup
		   "found a name that was defined locally more than once: ~a"
		   (syntax-e dup))))
	      (let ([exprs (syntax->list (syntax exprs))])
		(check-single-expression who 
					 "after the name-defining sequence"
					 stx
					 exprs)))]
	   [(_ binding-non-seq . __)
	    (teach-syntax-error
	     who
	     (syntax binding-non-seq)
	     "expected a parenthesized sequence of local name definitions after `~a', but found ~a"
	     who
	     (something-else (syntax binding-non-seq)))]
	   [(_)
	    (teach-syntax-error
	     who
	     stx
	     "expected a sequence of local name definitions after `~a', but nothing's there"
	     who)]
	   [_else
	    (bad-use-error who stx)]))]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; time
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (intermediate-time stx)
    (syntax-case stx ()
      [(_ . exprs)
       (check-single-expression 'time 
				"after `time'"
				stx
				(syntax->list (syntax exprs)))
       (syntax/loc stx (time . exprs))]
      [_else
       (bad-use-error 'time stx)]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; define (advanced)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (advanced-define stx)
    ;; Handle the case that doesn't fit into beginner, then dispatch to beginner
    (syntax-case stx ()
      [(_ (name) expr)
       (and (identifier? (syntax name))
	    (memq (syntax-local-context) '(top-level module)))
       (syntax/loc stx (define (name) expr))]
      [(_ . rest)
       (syntax/loc stx (beginner-define . rest))]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; lambda (advanced)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (advanced-lambda stx)
    (syntax-case stx ()
      [(_  (name ...) . exprs)
       (let ([names (syntax->list (syntax (name ...)))])
	 (for-each (lambda (name)
		     (unless (identifier? name)
		       (teach-syntax-error
			'lambda
			name
			"expected a name for an argument, but found ~a"
			(something-else name))))
		   names)
	 (let ([dup (check-duplicate-identifier names)])
	   (when dup
	     (teach-syntax-error
	      'lambda
	      dup
	      "found an argument name that is used more than once: ~a"
	      (syntax-e dup))))
	 (check-single-expression 'lambda 
				  "after the argument-name sequence"
				  stx
				  (syntax->list (syntax exprs)))
	 (syntax/loc stx (lambda (name ...) . exprs)))]
      [(_ arg-non-seq . exprs)
       (teach-syntax-error
	'lambda
	(syntax arg-non-seq)
	"expected a parenthesized sequence of argument names after `lambda', but found ~a"
	(something-else (syntax arg-non-seq)))]
      [(_)
       (teach-syntax-error
	'lambda
	stx
	"expected a sequence of argument names after `lambda', but nothing's there")]
      [_else
       (bad-use-error 'lambda stx)]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; define-struct (advanced)         >> weak errors <<
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (advanced-define-struct stx)
    (syntax-case stx ()
      [(_ name/sup fields)
       (syntax/loc stx (syntax (define-struct name/sup fields)))]))


  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; let (advanced)       >> mz errors in named case <<
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (advanced-let stx)
    (syntax-case stx ()
      [(_ name . rest)
       (identifier? (syntax name))
       (syntax/loc stx (let name . rest))]
      [(_ . rest)
       (syntax/loc stx (intermediate-let . rest))]
      [_else
       (bad-use-error 'let stx)]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; recur (advanced)               >> let errors!! <<
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (advanced-recur stx)
    (syntax-case stx ()
      [(_ . rest)
       (syntax/loc stx (advanced-let . rest))]
      [_else
       (bad-use-error 'recur stx)])))
