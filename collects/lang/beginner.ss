
;; Implements the Beginner Scheme language, at least in terms of the
;; forms and procedures. The reader-level aspects of the language
;; (e.g., case-sensitivity) are not implemented here.

;; Bugs: define-struct shouldn't define mutators (or struct)

(module beginner mzscheme
  (require (lib "etc.ss")
	   (lib "list.ss")
	   (lib "math.ss"))
  (require-for-syntax "private/teachhelp.ss")

  ;; syntax:
  (provide (rename beginner-define define)
	   (rename beginner-define-struct define-struct)
	   (rename beginner-lambda lambda)
	   (rename beginner-app #%app)
	   (rename beginner-cond cond)
	   (rename beginner-if if)
	   (rename beginner-and and)
	   (rename beginner-or or)
	   (rename beginner-quote quote)
	   (rename #%module-begin #%plain-module-begin))

  ;; procedures:
  (provide read number? = < > <= >= + - * / max min quotient remainder modulo
	   square sqrt expt abs exp log sin cos tan asin acos atan sinh cosh
	   exact? integer?  zero? odd? even? add1 sub1 lcm gcd rational?
	   numerator denominator inexact?  real? floor ceiling round complex?
	   make-polar real-part imag-part magnitude angle conjugate
	   exact->inexact inexact->exact number->string integer->char random
	   current-seconds e pi boolean? boolean=? not symbol? symbol=? cons?
	   pair? empty? null? list? cons first car rest cdr second cadr third
	   caddr fourth cadddr fifth sixth seventh eighth list-ref list append
	   length memq memv member reverse assq equal? char? char=? char<? char>?
	   char<=? char>=? char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
	   char-numeric? char-alphabetic? char-whitespace? char-upper-case?
	   char-lower-case? char-upcase char-downcase char->integer string?
	   string-length make-string string-ref substring string-copy
	   string-append string=? string<? string>? string<=? string>=?
	   string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
	   string->number string->list list->string format make-posn posn? posn-x
	   posn-y error eq? apply
	   true false empty)

  (define-struct posn (x y) (make-inspector)) ; transparent

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

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; define
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (beginner-define stx)

    (define (check-single-result-expr exprs where enclosing-expr)
      (when (null? exprs)
	(teach-syntax-error
	 where
	 enclosing-expr
	 "expected a result expression within `~a', but nothing's there"
	 where))
	(unless (null? (cdr exprs))
	  (teach-syntax-error
	   where
	   (cadr exprs)
	   "expected just one expression inside `~a', but found an extra expression~a"
	   where
	   (if (null? (cddr exprs))
	       ""
	       (format " (plus ~a more)" (length (cddr exprs)))))))

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
       (syntax-case* (syntax expr) (beginner-lambda)
	   (lambda (a b) (eq? (syntax-e a) (syntax-e b))) ;; lit. comparison for lambda
	 ;; Possibly well-formed lambda def:
	 [(beginner-lambda (arg ...) lexpr ...)
	  (begin
	    (for-each (lambda (arg)
			(unless (identifier? arg)
			  (teach-syntax-error
			   'lambda
			   arg
			   "expected a name for a function argument, but found ~a"
			   (something-else arg))))
		      (syntax->list (syntax (arg ...))))
	    (check-single-result-expr (syntax->list (syntax (lexpr ...)))
				      'lambda
				      (syntax expr))
	    (syntax (define (name arg ...) lexpr ...)))]
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
      [(_ (name ...) expr ...)
       (let ([names (syntax->list (syntax (name ...)))])
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
	 (check-single-result-expr (syntax->list (syntax (expr ...)))
				   'define
				   stx)
	 (syntax (define (name ...) expr ...)))]
      ;; Constant/lambda with too many or too few parts:
      [(_ name expr ...)
       (identifier? (syntax name))
       (let ([exprs (syntax->list (syntax (expr ...)))])
	 (if (null? exprs)
	     (teach-syntax-error
	      'define
	      stx
	      "expected an expression after the defined name ~a, but nothing's there"
	      (syntax-e (syntax name)))
	     (teach-syntax-error
	      'define
	      (cadr exprs)
	      "expected just one expression inside `define', but found an extra expression~a"
	      (if (null? (cddr exprs))
		  ""
		  (format " (plus ~a more)" (length (cddr exprs)))))))]
      ;; Bad name/header:
      [(_ non-name expr ...)
       (teach-syntax-error
	'define
	(syntax (non-name))
	"expected a function name, constant name, or function header for `define', ~
         but found ~a"
	(something-else (syntax non-name)))]
      ;; Missing name:
      [(_)
       (teach-syntax-error
	'define
	(syntax (non-name))
	"expected a function name, constant name, or function header after `define', ~
         but nothing's there")]
      [_else
       (bad-use-error 'define stx)]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; lambda (only works with define)
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
  ;; define-struct
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-syntax (beginner-define-struct stx)
    (syntax-case stx ()
      [(_ name_ (field_ ...))
       (let ([name (syntax name_)]
	     [fields (syntax->list (syntax (field_ ...)))])
	 (unless (identifier? name)
	   (teach-syntax-error
	    'define-struct
	    stx
	    "expected a structure type name, found ~a"
	    (something-else name)))
	 (for-each
	  (lambda (field)
	    (unless (identifier? field)
	      (teach-syntax-error
	       'define-struct
	       stx
	       "expected a structure field name, found ~a"
	       (something-else field))))
	  fields)
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
		       (values to-define-name ...))))))]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; application
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (beginner-app stx)
    (syntax-case stx ()
      [(_ rator rand ...)
       (let* ([fun (syntax rator)]
	      [lex? (and (identifier? fun)
			 (eq? 'lexical (identifier-binding fun)))])
	 (unless (and (identifier? fun) (not lex?))
	   (teach-syntax-error
	    'application
	    fun
	    "expected a defined name or a primitive operation name for a function call, but found ~a"
	    (if lex?
		"a function argument name"
		(something-else fun))))
	 (syntax (#%app rator rand ...)))]
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
      [_else (bad-use-error 'quote stx)])))
