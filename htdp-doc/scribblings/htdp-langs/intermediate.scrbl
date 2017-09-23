#lang scribble/doc
@(require "common.rkt" "std-grammar.rkt" "prim-ops.rkt"
          (for-label lang/htdp-intermediate))


@title[#:tag "intermediate"]{Intermediate Student}

@section-index["ISL"]

@declare-exporting[lang/htdp-intermediate]

The grammar notation uses the notation @racket[X #, @dots] (bold dots) to indicate that
@racket[X] may occur an arbitrary number of times (zero, one, or more). The 
grammar also provides @racket[...] as an identifier to be used in templates. 

@racketgrammar*+qq[
#:literals (define define-struct lambda cond else if and or require lib planet
            local let let* letrec time check-expect check-random check-within check-error check-satisfied)
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
      character]
[expr-for-let (lambda (variable variable #, @dots) expr)
              expr]
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
                     #:with-beginner-function-call #t)




@section[#:tag "intermediate-pre-defined" ]{Pre-defined Functions}

The remaining subsections list those functions that are built into the
programming language. All other functions are imported from a teachpack or
must be defined in the program. 

@(require (submod lang/htdp-intermediate procedures))
@(render-sections (docs) #'here "htdp-intermediate")

@;prim-op-defns['(lib "htdp-intermediate.rkt" "lang") #'here '()]

