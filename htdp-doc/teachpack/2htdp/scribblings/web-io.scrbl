#lang scribble/manual

@(require "shared.rkt" (for-label racket xml/xml))

@teachpack["web-io"]{Web IO}
@author{Matthias Felleisen}

@defmodule[#:require-form beginner-require 2htdp/web-io]

The teachpack provides a single function: 

@defproc[(show-in-browser [x xexpr?]) string?]{
 Translates the given X-expression into a String. It also has the
 @bold{effect} of opening an external browser and displaying the
 X-expression rendered as XHTML.

@bold{Example}

@racketblock[(show-in-browser '(html (body (b "hello world"))))]

}

@history[
 #:added "1.0" @;{list{Fri Nov  3 11:49:40 EDT 2017}}
]
