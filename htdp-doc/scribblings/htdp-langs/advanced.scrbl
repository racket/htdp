#lang scribble/doc
@(require "common.rkt" "std-grammar.rkt" "prim-ops.rkt"
          (for-label lang/htdp-advanced))

@(require scribble/example)

@title[#:tag "advanced"]{Advanced Student}

@section-index["ASL"]

@declare-exporting[lang/htdp-advanced]

@grammar

@racketgrammar*+qq[
#:literals (define define-struct define-datatype lambda λ cond else if and or require lib planet
            local let let* letrec time begin begin0 set! delay shared recur when case match unless
             ; match
             _ cons list list* struct vector box
            check-expect check-random check-within check-member-of
	    check-range check-error check-satisfied
	    : signature enum mixed -> ListOf)
(expr  check-satisfied check-expect check-random check-within check-error check-member-of check-range require)
[program (code:line def-or-expr #, @dots)]
[def-or-expr definition
             expr
             test-case
             library-require
	     signature-declaration]
[definition (define (name variable #, @dots) expr)
            (define name expr)
            (define-struct name (name #, @dots))
            (define-datatype name (name name #, @dots) #, @dots)]
[expr (begin expr expr #, @dots)
      (begin0 expr expr #, @dots)
      (set! variable expr)
      (delay expr)
      (lambda (variable #, @dots) expr)
      (λ (variable #, @dots) expr)
      (local [definition #, @dots] expr)
      (letrec ([name expr] #, @dots) expr)
      (shared ([name expr] #, @dots) expr)
      (let ([name expr] #, @dots) expr)
      (let name ([name expr] #, @dots) expr)
      (let* ([name expr] #, @dots) expr)
      (recur name ([name expr] #, @dots) expr)
      (code:line (expr expr #, @dots))
      (cond [expr expr] #, @dots [expr expr])
      (cond [expr expr] #, @dots [else expr])
      (case expr [(choice choice #, @dots) expr] #, @dots 
                 [(choice choice #, @dots) expr])
      (case expr [(choice choice #, @dots) expr] #, @dots 
                 [else expr])
      (match expr [pattern expr] #, @dots)
      (if expr expr expr)
      (when expr expr)
      (unless expr expr)
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
[choice (code:line name)
        number]
[pattern _
         name
         number
         true
         false
         string
         character
         @#,elem{@racketvalfont{'}@racket[_quoted]}
         @#,elem{@racketvalfont{`}@racket[_quasiquoted-pattern]}
         (cons pattern pattern)
         (list pattern #, @dots)
         (list* pattern #, @dots)
         (struct id (pattern #, @dots))
         (vector pattern #, @dots)
         (box pattern)]
[quasiquoted-pattern name
                     number
                     string
                     character
                     (quasiquoted-pattern #, @dots)
                     @#,elem{@racketvalfont{'}@racket[_quasiquoted-pattern]}
                     @#,elem{@racketvalfont{`}@racket[_quasiquoted-pattern]}
                     @#,elem{@racketfont[","]@racket[_pattern]}
                     @#,elem{@racketfont[",@"]@racket[_pattern]}]
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


@prim-nonterms[("advanced") define define-struct]

@prim-variables[("advanced") empty true false .. ... .... ..... ......]

@; ----------------------------------------------------------------------
@section[#:tag "advanced-syntax"]{Syntax for Advanced}

In Advanced, @racket[set!] can be used to mutate variables, and
@racket[define-struct]'s structures are mutatable. @racket[define] and
@racket[lambda] can define functions of zero arguments, and function calls can
invoke functions of zero arguments.


@defform[(lambda (variable #, @dots) expression)]{

Creates a function that takes as many arguments as given @racket[variable]s,
and whose body is @racket[expression].}

@defform[(λ (variable #, @dots) expression)]{

The Greek letter @racket[λ] is a synonym for @racket[lambda].}

@defform/none[(expression expression #, @dots)]{

Calls the function that results from evaluating the first
@racket[expression]. The value of the call is the value of function's body when
every instance of @racket[name]'s variables are replaced by the values of the
corresponding @racket[expression]s.

The function being called must come from either a definition appearing before the
function call, or from a @racket[lambda] expression. The number of argument
@racket[expression]s must be the same as the number of arguments expected by
the function.}

@; ----------------------------------------------------------------------


@defform[(define-datatype dataype-name [variant-name field-name #, @dots] #, @dots)]{

A short-hand for defining a group of related structures. The following @racket[define-datatype]:

@racketblock[
 (define-datatype datatype-name
   [variant-name field-name (unsyntax @racketidfont{#, @dots})]
   (unsyntax @racketidfont{...}))
]
is equivalent to:
@racketblock[
 (define ((unsyntax @racket[datatype-name])? x)
   (or ((unsyntax @racket[variant-name])? x) (unsyntax @racketidfont{...})))
 (define-struct variant-name (field-name (unsyntax @racketidfont{...})))
 (unsyntax @racketidfont{...})
]}



@defform[(begin expression expression #, @dots)]{

Evaluates the @racket[expression]s in order from left to right. The value of
the @racket[begin] expression is the value of the last @racket[expression].}



@defform[(begin0 expression expression #, @dots)]{

Evaluates the @racket[expression]s in order from left to right. The value of
the @racket[begin] expression is the value of the first @racket[expression].}



@defform[(set! variable expression)]{

Evaluates @racket[expression], and then mutates the @racket[variable]
to have @racket[expression]'s value. The @racket[variable] must be defined 
by @racket[define], @racket[letrec], @racket[let*], or @racket[let].}


@defform[(delay expression)]{

Produces a ``promise'' to evaluate @racket[expression]. The @racket[expression]
is not evaluated until the promise is forced with @racket[force]; when
the promise is forced, the result is recorded, so that any further
@racket[force] of the promise immediately produces the remembered value.}



@defform[(shared ([name expression] #, @dots) expression)]{

Like @racket[letrec], but when an @racket[expression] next to an @racket[id]
is a @racket[cons], @racket[list], @racket[vector], quasiquoted
expression, or @racketidfont{make-}@racket[_struct-name] from a
@racket[define-struct], the @racket[expression] can refer directly to any
@racket[name], not just @racket[name]s defined earlier. Thus,
@racket[shared] can be used to create cyclic data structures.}


@; ----------------------------------------------------------------------


@defform[(recur name ([name expression] #, @dots) expression)]{

A short-hand syntax for recursive loops. The first @racket[name] corresponds to
the name of the recursive function. The @racket[name]s in the parenthesis are
the function's arguments, and each corresponding @racket[expression] is a
value supplied for that argument in an initial starting call of the
function. The last @racket[expression] is the body of the function.

More precisely, the following @racket[recur]: 

@racketblock[
(recur func-name ([arg-name arg-expression] (unsyntax @racketidfont{...}))
  body-expression)
]

is equivalent to:

@racketblock[
(local [(define (func-name arg-name (unsyntax @racketidfont{...})) body-expression)]
  (func-name arg-expression (unsyntax @racketidfont{...})))
]}


@defform/none[(let name ([name expression] #, @dots) expression)]{

An alternate syntax for @racket[recur].}


@; ----------------------------------------------------------------------


@defform[(case expression [(choice #, @dots) expression] #, @dots [(choice #, @dots) expression])]{

A @racket[case] form contains one or more clauses. Each clause contains a
choices (in parentheses)---either numbers or names---and an answer
@racket[expression]. The initial @racket[expression] is evaluated, and its
value is compared to the choices in each clause, where the lines are considered
in order. The first line that contains a matching choice provides an answer
@racket[expression] whose value is the result of the whole @racket[case]
expression. Numbers match with the numbers in the choices, and symbols match
with the names. If none of the lines contains a matching choice, it is an
error.}

@defform/none[#:literals (case else)
              (case expression [(choice #, @dots) expression] #, @dots [else expression])]{

This form of @racket[case] is similar to the prior one, except that the final
@racket[else] clause is taken if no clause contains a choice matching the value
of the initial @racket[expression].}

@; ----------------------------------------------------------------------


@defform[(match expression [pattern expression] #, @dots)]{

A @racket[match] form contains one or more clauses that are surrounded by
square brackets. Each clause contains a pattern---a description of a value---and
an answer @racket[expression].  The initial @racket[expression] is evaluated,
and its value is matched against the pattern in each clause, where the clauses are
considered in order. The first clause that contains a matching pattern provides
an answer @racket[expression] whose value is the result of the whole
@racket[match] expression. This @racket[expression] may reference identifiers
defined in the matching pattern. If none of the clauses contains a matching
pattern, it is an error.}

@; ----------------------------------------------------------------------


@defform[(when question-expression body-expression)]{

If @racket[question-expression] evaluates to @racket[true], the result of the
@racket[when] expression is the result of evaluating the 
@racket[body-expression], otherwise the result is @racket[(void)] and the 
@racket[body-expression] is not evaluated. If the result of evaluating the
@racket[question-expression] is neither @racket[true] nor @racket[false], it is an
error.}

@defform[(unless question-expression body-expression)]{

Like @racket[when], but the @racket[body-expression] is evaluated when the
@racket[question-expression] produces @racket[false] instead of @racket[true].}


@section[#:tag "advanced-common-syntax"]{Common Syntaxes}

The following syntaxes behave the same in the @emph{Advanced}
level as they did in the @secref["intermediate-lam"] level.


@(intermediate-forms lambda
                     local
                     letrec
                     let*
                     let
                     time
                     define
                     define-struct)


@(define-forms/normal define)

@(prim-forms ("advanced")
             define 
             lambda
             define-struct 
             @{In Advanced, @racket[define-struct] introduces one additional function:
              @itemize[
               @item{@racketidfont{set-}@racket[_structure-name]@racketidfont{-}@racket[_field-name]@racketidfont{!}
                : takes an instance of the structure and a value, and
                mutates the instance's field to the given value.}]}
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

@section[#:tag "advanced-signatures"]{Signatures}

Signatures do not have to be comment: They can also be part of the
code.  When a signature is attached to a function, DrRacket will check
that program uses the function in accordance with the signature and
display signature violations along with the test results.

A signature is a regular value, and is specified as a
@seclink["advanced-signature-forms"]{@italic{signature form}}, a
special syntax that only works with @racket[:] signature declarations
and inside @racket[signature] expressions.

@defform[(: name signature-form)]{
This attaches the signature specified by @racket[signature-form] to
the definition of @racket[name].
There must be a definition of @racket[name] somewhere in the program.

@racketblock[
(: age Integer)
(define age 42)

(: area-of-square (Number -> Number)) 
(define (area-of-square len)
  (sqr len))
  ]

On running the program, Racket checks whether the signatures attached
with @racket[:] actually match the value of the variable.  If they
don't, Racket reports @italic{signature violation} along with test failures.

For example, this piece of code:

@racketblock[
(: age Integer)
(define age "fortytwo")
]

Yields this output:

@verbatim{
1 signature violation.

Signature violations:
        got "fortytwo" at line 2, column 12, signature at line 1, column 7
}

Note that a signature violation does not stop the running program.
}

@defform[(signature signature-form)]{
This returns the signature described by @racket[signature-form] as a value.
}

@subsection[#:tag "advanced-signature-forms"]{Signature Forms}

Any expression can be a signature form, in which case the signature is
the value returned by that expression.  There are a few special
signature forms, however:

In a signature form, any name that starts with a @racketvalfont{%} is a
@italic{signature variable} that stands for any signature depending on how
the signature is used.

Example:

@racketblock[
(: same (%a -> %a))

(define (same x) x)
]

@defform[#:id -> (input-signature-form ... -> output-signature-form)]{
This signature form describes a function with inputs described by the
@racket[input-signature-form]s and output described by
@racket[output-signature-form].
}	

@defform[(enum expr ...)]{
This signature describes an enumeration of the values returned by the @racket[expr]s.

Example:

@racketblock[
(: cute? ((enum "cat" "snake") -> Boolean))

(define (cute? pet)
  (cond
    [(string=? pet "cat") #t]
    [(string=? pet "snake") #f]))
]
}

@defform[(mixed signature-form ...)]{
This signature describes mixed data, i.e. an itemization where
each of the cases has a signature described by a @racket[signature-form].

Example:

@racketblock[(define SIGS (signature (mixed Aim Fired)))]
}

@defform[(ListOf signature-form)]{
This signature describes a list where the elements are described by
@racket[signature-form].
}


@subsection[#:tag "struct-signatures"]{Struct Signatures}

A @racket[define-struct] form defines two additional names that can be
used in signatures.  For a struct called @racketvalfont{struct}, these
are @racketvalfont{Struct} and @racketvalfont{StructOf}.  Note that
these names are capitalized.  In particular, a struct called
@racketvalfont{Struct}, will also define @racketvalfont{Struct} and
@racketvalfont{StructOf}.  Moreover, when forming the additional
names, hyphens are removed, and each letter following a hyphen is
capitalized - so a struct called @racketvalfont{foo-bar} will define
@racketvalfont{FooBar} and @racketvalfont{FooBarOf}.

@racketvalfont{Struct} is a signature that describes struct values
from this structure type.  @racketvalfont{StructOf} is a function
that takes as input a signature for each field.  It returns a
signature describing values of this structure type, additionally
describing the values of the fields of the value.

@racketblock[
(define-struct pair [fst snd])

(: add-pair ((PairOf Number Number) -> Number))
(define (add-pair p)
  (+ (pair-fst p) (pair-snd p)))
]

@; ----------------------------------------

@section[#:tag "advanced-pre-defined"]{Pre-Defined Functions}

@pre-defined-fun

@(require (submod lang/htdp-advanced procedures))
@(render-sections (docs) #'here "htdp-advanced")

@;prim-op-defns['(lib "htdp-advanced.rkt" "lang") #'here '()]
