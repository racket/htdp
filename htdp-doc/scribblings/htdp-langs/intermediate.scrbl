#lang scribble/doc
@(require "common.rkt" "std-grammar.rkt" "prim-ops.rkt"
          (for-label lang/htdp-intermediate))


@title[#:tag "intermediate"]{Intermediate Student}

@section-index["ISL"]

@declare-exporting[lang/htdp-intermediate]

@grammar

@racketgrammar*+qq[
#:literals (define define-struct lambda cond else if and or require lib planet
            local let let* letrec time check-expect check-random check-within check-error check-satisfied
   	    : signature enum mixed -> ListOf)
(expr check-satisfied check-expect check-random check-within check-member-of check-range check-error require)
[program (code:line def-or-expr #, @dots)]
[def-or-expr definition
             expr
             test-case
             library-require]
[definition (define (name variable variable #, @dots) expr)
            (define name expr)
            (define name (lambda (variable variable #, @dots) expr))
            (define-struct name (name #, @dots))]
[expr (local [definition #, @dots] expr)
      (letrec ([name expr-for-let] #, @dots) expr)
      (let ([name expr-for-let] #, @dots) expr)
      (let* ([name expr-for-let] #, @dots) expr)
      (code:line (name expr expr #, @dots) )
      (cond [expr expr] #, @dots [expr expr])
      (cond [expr expr] #, @dots [else expr])
      (if expr expr expr)
      (and expr expr expr #, @dots)
      (or expr expr expr #, @dots)
      (time expr)
      (code:line name)
      (code:line @#,elem{@racketvalfont{'}@racket[_quoted]})
      (code:line @#,elem{@racketvalfont{`}@racket[_quasiquoted]})
      (code:line @#,elem{@racketvalfont{'}@racket[()]}) 
      number
      boolean 
      string
      character
      (signature signature-form)]
[expr-for-let (lambda (variable variable #, @dots) expr)
              expr]
[signature-declaration (: name signature-form)]
[signature-form 
	   (enum expr ...)
	   (mixed signature-form ...)
	   (signature-form ... -> signature-form)
	   (ListOf signature-form)
	   signature-variable
	   expr]
[signature-variable @#,elem{@racketvalfont{%}name}]
]

@prim-nonterms[("intermediate") define define-struct]

@prim-variables[("intermediate") empty true false .. ... .... ..... ......]

@; ----------------------------------------------------------------------

@section[#:tag "intermediate-syntax"]{Syntax for Intermediate}


@(intermediate-forms lambda
                     local
                     letrec
                     let*
                     let
                     time
                     define
                     define-struct)

@; ----------------------------------------------------------------------

@section[#:tag "intermediate-common-syntax"]{Common Syntaxes}

The following syntaxes behave the same in the @emph{Intermediate} level as they
did in the @secref["beginner-abbr"] level.

@(beginner-abbr-forms quote quasiquote unquote unquote-splicing)

@(define-forms/normal define)
@(define-form/explicit-lambda define lambda)

@(prim-forms 
                     ("intermediate")
                     define 
                     lambda
                     define-struct []
                     cond
                     else
                     if
                     and 
                     or
                     check-expect
                     check-random
                     check-satisfied
                     check-within
                     check-error
                     check-member-of
                     check-range
                     require
                     true false
                     #:with-beginner-function-call #t)


@; --------------------------------------------------

@section[#:tag "intermediate-signatures"]{Signatures}

@(signature-forms ("intermediate") define-struct : signature enum mixed -> ListOf)

@section[#:tag "intermediate-pre-defined" ]{Pre-defined Functions}

@pre-defined-fun

@(require (submod lang/htdp-intermediate procedures))
@(render-sections (docs) #'here "htdp-intermediate")

@;prim-op-defns['(lib "htdp-intermediate.rkt" "lang") #'here '()]

