#lang scribble/doc
@(require "common.rkt" "std-grammar.rkt" "prim-ops.rkt"
          (for-label lang/htdp-intermediate-lambda))

@title[#:tag "intermediate-lam"]{Intermediate Student with Lambda}

@section-index["ISL+"]

@declare-exporting[lang/htdp-intermediate-lambda]

@grammar

@racketgrammar*+qq[
#:literals (define define-struct lambda λ cond else if and or require lib planet
            local let let* letrec time check-expect check-random
	    check-within check-member-of check-range check-error check-satisfied)
(expr check-satisfied check-expect check-random check-within check-member-of check-range check-error require)
[program (code:line def-or-expr #, @dots)]
[def-or-expr definition
             expr
             test-case
             library-require]
[definition (define (name variable variable #, @dots) expr)
            (define name expr)
            (define-struct name (name #, @dots))]
[expr (lambda (variable variable #, @dots) expr)
      (λ (variable variable #, @dots) expr)
      (local [definition #, @dots] expr)
      (letrec ([name expr] #, @dots) expr)
      (let ([name expr] #, @dots) expr)
      (let* ([name expr] #, @dots) expr)
      (code:line (expr expr expr #, @dots))
      (cond [expr expr] #, @dots [expr expr])
      (cond [expr expr] #, @dots [else expr])
      (if expr expr expr)
      (and expr expr expr #, @dots)
      (or expr expr expr #, @dots)
      (time expr)
      (code:line name)
      (code:line prim-op)
      (code:line @#,elem{@racketvalfont{'}@racket[_quoted]})
      (code:line @#,elem{@racketvalfont{`}@racket[_quasiquoted]})
      (code:line @#,elem{@racketvalfont{'}@racket[()]}) 
      number
      boolean 
      string
      character]
]

@prim-nonterms[("intm-w-lambda") define define-struct]

@prim-variables[("intm-w-lambda") empty true false .. ... .... ..... ......]


@; ----------------------------------------------------------------------

@section[#:tag "intm-w-lambda-syntax"]{Syntax for Intermediate with Lambda}


@defform[(lambda (variable variable #, @dots) expression)]{

Creates a function that takes as many arguments as given @racket[variable]s,
and whose body is @racket[expression].}

@defform[(λ (variable variable #, @dots) expression)]{

The Greek letter @racket[λ] is a synonym for @racket[lambda].}



@defform/none[(expression expression expression #, @dots)]{

Calls the function that results from evaluating the first
@racket[expression]. The value of the call is the value of function's body when
every instance of @racket[name]'s variables are replaced by the values of the
corresponding @racket[expression]s.

The function being called must come from either a definition appearing before the
function call, or from a @racket[lambda] expression. The number of argument
@racket[expression]s must be the same as the number of arguments expected by
the function.}



@(intermediate-forms lambda
                     local
                     letrec
                     let*
                     let
                     time
                     define
                     define-struct)



@; ----------------------------------------------------------------------

@section[#:tag "intm-w-lambda-common-syntax"]{Common Syntaxes}

The following syntaxes behave the same in the @emph{Intermediate with Lambda}
level as they did in the @secref["intermediate"] level.

@(define-forms/normal define)

@(beginner-abbr-forms quote quasiquote unquote unquote-splicing)

@(prim-forms ("intermediate-lam")
             define 
             lambda
             define-struct []
             define-wish
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
             #:with-beginner-function-call #f)

@section[#:tag "intm-w-lambda-pre-defined"]{Pre-defined Functions}

@pre-defined-fun

@(require (submod lang/htdp-intermediate-lambda procedures))
@(render-sections (docs) #'here "htdp-intermediate-lambda")

@;prim-op-defns['(lib "htdp-intermediate-lambda.rkt" "lang") #'here '()]
