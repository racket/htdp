
;; Implements the Beginner Scheme language, at least in terms of the
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

(module beginner mzscheme
  (require (lib "etc.ss")
	   (lib "list.ss")
	   (lib "math.ss"))

  ;; Implements the forms:
  (require "private/teach.ss")

  (define-struct posn (x y) (make-inspector)) ; transparent

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
	   true false empty))



