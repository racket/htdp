#lang scribble/doc

@(require (for-label racket/base test-engine/racket-tests test-engine/test-markup))

@(require scribble/manual scribble/eval racket/sandbox)
@(define-syntax-rule (mk-eval defs ...)
  ;; ==> 
  (let ([me (make-base-eval)])
    (call-in-sandbox-context me (lambda () (error-print-source-location #f)))
    (interaction-eval #:eval me defs) 
    ...
    me))

@title{Test Support}

@table-of-contents[]

@; ----------------------------------------------------------------------

@section{Using Check Forms}

@defmodule[test-engine/racket-tests]

This module provides test forms for use in Racket programs, as well
as parameters to configure the behavior of test reports.

Each check form may only occur at the top-level; results are collected
and reported by the test function.  Note that the check forms only
register checks to be performed.  The checks are actually run by the
@racket[test] function.

@defform[(check-expect expr expected-expr)]{
Checks whether the value of the @racket[expr] expression is
@racket[equal?] to the value produced by the @racket[expected-expr].

It is an error for @racket[expr] or @racket[expected-expr] to produce a function
value or an inexact number.}

@defform[(check-random expr expected-expr)]{
Checks whether the value of the @racket[expr] expression is
@racket[equal?] to the value produced by the @racket[expected-expr].

The form supplies the same random-number generator to both parts. If both
parts request @racket[random] numbers from the same interval in the same
order, they receive the same random numbers. 

@examples[#:eval (mk-eval (require test-engine/racket-tests))

(check-random (random 10) (random 10))

(check-random 
  (begin (random 100) (random 200))
  (begin (random 100) (random 200)))

(test)
]

If the two parts call @racket[random] for different intervals, they are
likely to fail: 

@examples[#:eval (mk-eval (require test-engine/racket-tests))
(check-random 
  (begin (random 100) (random 200))
  (begin (random 200) (random 100)))

(test)
]

It is an error for @racket[expr] or @racket[expected-expr] to produce a function
value or an inexact number.}

@defform[(check-satisfied expr property?)]{
Checks whether the value of the @racket[expr] expression satisfies 
the @racket[property?] predicate (which must evaluate to a function of one
argument). 

@examples[#:eval (mk-eval (require test-engine/racket-tests))

(check-satisfied 1 odd?)

(check-satisfied 1 even?)

(test)
]

@history[
 #:changed 
 "1.1" 
 "allow the above examples to run in BSL and BSL+"]
} 

@defform[(check-within expr expected-expr delta-expr)
          #:contracts ([delta-expr number?])]{
Checks whether the value of the @racket[test] expression is structurally
equal to the value produced by the @racket[expected] expression; every
number in the first expression must be within @racket[delta] of the
corresponding number in the second expression.

It is an error for @racket[expr] or @racket[expected] to produce a function
value.} 

@defform*[ [(check-error expr)
            (check-error expr msg-expr)]
            #:contracts ([msg-expr string?]) ]{
Checks that evaluating @racket[expr] signals an error, where
the error message matches the string (if any).}

@defform[(check-member-of expr expected-expr ...)]{
Checks whether the value of the @racket[expr] expression is @racket[equal?]
to any of the values produced by the @racket[expected-expr]s.

It is an error for @racket[expr] or any of the @racket[expected-expr]s
to produce a function value or an inexact number.}

@defform[(check-range expr min-expr max-expr)
         #:contracts ([expr number?]
                      [min-expr number?]
                      [max-expr number?])]{
Checks whether value of @racket[expr] is between the values of
@racket[min-expr] and @racket[max-expr] inclusive.}

@defform[(test)]{

Runs all of the tests specified by check forms in the current module
and reports the results.  When using the gui module, the results are
provided in a separate window, otherwise the results are printed to
the current output port.}

@defboolparam[test-silence silence?]{

A parameter that stores a boolean, defaults to #f, that can be used to
suppress the printed summary from test.}


@defboolparam[test-execute execute?]{

A parameter that stores a boolean, defaults to #t, that can be used to
suppress evaluation of test expressions.
}

@section{Running Tests and Inspecting Test Results}

@defmodule[test-engine/test-engine]

This module defines language-agnostic procedures for running test code
to execute checks, and recording and inspecting their results.

A @italic{test} is a piece of code run for testing, a @italic{check}
is a single assertion within that code: Typically the tests are first
registered, then they are run, and then their results are inspected.
Both tests and the results of failed checks are recorded in a data
structure called a @italic{test object}.  There is always a current
test object associated with the current namespace.

@defstruct*[test-object
            ((tests (listof (-> any)))
             (failed-checks (listof failed-check?))
             (signature-violations (listof signature-violation?)))
	     #:omit-constructor]{
The three components of a @racket[test-object] are all in reverse order:

The first one is the list of tests (each represented by a thunk), the
others are failed checks and signature violations, respectively.
}

@defproc[(empty-test-object) test-object?]{
Creates an empty test object.
}

@defproc[(current-test-object) test-object?]{
Returns the current test object.
}

@defproc[(initialize-test-object!) any]{

Initializes the test object.  Note that this is not necessary before
using @racket[current-test-object] and the various other functions
operating on it: These will automatically initialize as necessary.
Use this function to reset the current test object.
}

@defproc[(add-test! [thunk (-> any)])  any]{
Register a test, represented by a thunk.  The thunk, when called, is
expected to call @racket[add-failed-check!] and
@racket[add-signature-violation!] as appropriate.
}

@defproc[(add-failed-check! [failed-check failed-check?]) any]{
Record a test failure.
}

@defproc[(add-signature-violation! [violation signature-violation?]) any]{
Record a signature violation.
}

@defproc[(run-tests!) test-object?]{
Run the tests, calling the thunks registered via @racket[add-test!] in
the order they were registered.
}


@defstruct*[failed-check ((reason fail-reason?)
                          (srcloc? (or/c #f srcloc?)))]{
This is a description of a failed check.
The source location, if present, is from an expression that may have caused the failure,
possibly an exception.}

@defstruct*[fail-reason ((srcloc srcloc?))]{
Common supertype of all objects describing a reason for a failed check.
The @code{srcloc} is the source location of the check.}

@defstruct*[(unexpected-error fail-reason)
            ((srcloc srcloc?)
             (expected any/c)
             (exn exn?))]{
An error happened instead of regular termination.
}

@defstruct*[(unequal fail-reason)
            ((srcloc srcloc?)
             (actual any/c)
             (expected any/c))]{
A value was supposed to be equal to another, but wasn't.
Generated by @racket[check-expect].
}

@defstruct*[(not-within fail-reason)
            ((srcloc srcloc?)
             (actual any/c)
             (expected any/c)
             (range real?))]{
A value was supposed to be equal to another within a certain range, but wasn't.
Generated by @racket[check-within].
}

@defstruct*[(incorrect-error fail-reason)
            ((srcloc srcloc?)
             (expected any/c)
             (exn exn?))]{
An exception was expected, but a different one occurred.
Generated by @racket[check-error].
}

@defstruct*[(expected-error fail-reason)
            ((srcloc srcloc?)
             (message (or/c #f string?))
             (value any/c))]{
An error was expected, but a value came out instead.
Generated by @racket[check-error].
}

@defstruct*[(not-mem fail-reason)
            ((srcloc srcloc?)
             (actual any/c)
             (set (listof any/c)))]{
The value produced was not part an the expected set.
Generated by @racket[check-member-of].
}

@defstruct*[(not-range fail-reason)
            ((srcloc srcloc?)
             (actual real?)
             (min real?)
             (max real?))]{
The value produced was not part an the expected range.
Generated by @racket[check-range].
}

@defstruct*[(satisfied-failed fail-reason)
            ((srcloc srcloc?)
             (actual any/c)
             (name string?))]{
The value produced did not satisfy a predicate.	     
The @code{name} field is the name of the predicate.
Generated by @racket[check-satisfied].
}

@defstruct*[(unsatisfied-error fail-reason)
            ((srcloc srcloc?)
             (name string?)
             (exn exn?))]{
A value was supposed to satsify a predicate, but an error happened
instead.  The @code{name} field is the name of the predicate.
Generated by @racket[check-satisfied].
}

@defstruct*[(violated-signature fail-reason)
            ((srcloc srcloc?)
             (obj any/c)
             (signature signature?)
             (blame (or/c #f syntax?)))]{
A signature was violated, and this was communicated via an exception.
Note that signature violations should really be (and usually are)
communicated via @racket[add-signature-violation!].
}

@defstruct*[signature-got
            ((value any/c))]{
The value that violated the signature.
}

@defstruct*[signature-violation
            ((obj any/c)
             (signature signature?)
             (message (or/c string? signature-got?))
             (srcloc (or/c #f srcloc?))
             (blame (or/c #f syntax?)))]{

Signature @code{signature} was violated by object @code{obj}.
The @code{srcloc} field is the location of the signature.
The optional @code{blame} field is for the piece of syntax to blame
for the violation.
}

@defstruct*[(property-fail fail-reason)
            ((srcloc srcloc?)
             (result check-result?))]{
A counterexample for a property was found, described in the @code{result} field.
}

@defstruct*[(property-error fail-reason)
            ((srcloc srcloc?)
             (exn exn?))]{
A property check produced an unexpected exception.
}

@section{Printing Test Results}

This module is responsible for output of test results: Where the
output goes, and some aspects of the formatting can be customized via parameters.

@defmodule[test-engine/test-markup]

@defparam[render-value-parameter render-value-proc (any/c . -> . string?)]{
This parameter determines how @racket[test-object->markup] renders a value
for display in an error message in a language-specific way.
The default is @racket[(lambda (v) (format "~V" v))].
}

@defparam[display-test-results-parameter display-test-proc (test-object? . -> . any)]{
This parameter determines how to output the test results.
The default prints to @racket[(current-output-port)].
}

@defproc[(display-test-results! [test-object test-object?]) any]{
This just calls the procedure bound to @racket[display-test-results-parameter].
}

@defparam[get-rewritten-error-message-parameter get-rewritten-error-message-proc (exn? . -> . string?)]{
This parameter determines how to get an error message from an exception,
possibly after reformulation and/or translation.
}

@defproc[(get-rewritten-error-message [exn exn?]) string?]{
This just calls the procedure bound to @racket[get-rewritten-error-message-parameter].
}

@defproc[(test-object->markup [test-object test-object?]) markup?]{
This generates a test report as markup, using
@racket[render-value-parameter] and
@racket[get-rewritten-error-message-parameter].
}


